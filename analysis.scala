import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import java.math.BigInteger
import java.io._
import scala.collection.mutable
import vlab.cs.ucsb.edu.DriverProxy

var gVerbose: Boolean = true
var gShouldFold: Boolean = true
var gOutFile: BufferedWriter = null
var gStdOut: Boolean = false
var gInterestingNames: List[String] = List()

val Max8Bit = 255
val Min8Bit = -128
val Bound8Bit = 7L
val Max16Bit = 65535
val Bound16Bit = 15L

def writeOut(str: String): Unit = {
  gOutFile.write(str)
  if (gStdOut) print(str)
}

class ProbEdge(src: io.shiftleft.codepropertygraph.generated.nodes.StoredNode,
               dst: io.shiftleft.codepropertygraph.generated.nodes.StoredNode,
               srcVisible: Boolean = true, 
               label: String = "", 
               edgeType: String = "ProbCFG",
               val prob: String = "",
               val tf: Int = 0,
               val cond: String = "",
               val loop: Boolean = false) 
extends Edge(src: io.shiftleft.codepropertygraph.generated.nodes.StoredNode,
               dst: io.shiftleft.codepropertygraph.generated.nodes.StoredNode,
               srcVisible: Boolean, 
               label: String, 
               edgeType: String)
end ProbEdge

class BranchConstraint:
  private var declarations: String = ""
  private var assertions: String = ""
  private var assertIntFmt: String = ""
  private var seenIds = mutable.Set[Long]()
  var topNode: Long = 0

  val assertFmt = "(assert %s)\n"
  
  /* Each instance of an identifier in a condition has a unique joern ID.
   * This creates issues with calculating probabilities. Each instance of an
   * identifier in the condition should map to the same ID in the SMT constraint.
   */
  private var ids = mutable.Map[String,Long]()
  private def mapIds(node: nodes.CfgNode): Unit = node match {
    case call: nodes.Call => call.argument.toList.map(arg => mapIds(arg))
    case ident: nodes.Identifier => ids(ident.code) = ident.id
    case _ => () /* Ignore any other node types */
  }

  private var solver = DriverProxy()
  private var solverBound = Bound8Bit

  def this(node: nodes.CfgNode) = {
    this()
    topNode = node.id
    start(node)
  }

  private def assertDomain(domain: Int): String = """(assert 
  (or 
    (and (>= x%%d (- %d)) (<= x%%d %d)) 
    (and (>= x%%d 0) (<= x%%d %d))
  )
)
""".format(domain / 2 + 1, domain / 2, domain)

  private def findDomainSize(node: nodes.CfgNode): Int = node match {
    case call: nodes.Call => {
      val argDomains = call.argument.toList.map(findDomainSize)
      if (argDomains.isEmpty) Max8Bit else argDomains.max
    }
    case literal: nodes.Literal => {
      if (literal.typeFullName != "int" || (Min8Bit to Max8Bit contains literal.code.toInt)) Max8Bit 
      else Max16Bit
    }
    case _ => Max8Bit // Ignore any other node types
  }

  private def start(node: nodes.CfgNode) = {
    // Set SMT solver options
    solver.setOption(DriverProxy.Option.ENABLE_IMPLICATIONS)
    solver.setOption(DriverProxy.Option.USE_SIGNED_INTEGERS)

    mapIds(node)

    val domainSize = findDomainSize(node)
    solverBound = if (domainSize == Max16Bit) Bound16Bit else Bound8Bit
    assertIntFmt = assertDomain(domainSize)

    declarations += declareNode(node)
    if (declarations != "") {
      assertions += assertFmt.format(generateConstraint(node))
    }
  }

  private def isOperatorCall(name: String): Boolean = {
    name match {
      case "<operator>.logicalAnd" | 
           "<operator>.logicalOr" | 
           "<operator>.equals" | 
           "<operator>.greaterEqualsThan" | 
           "<operator>.lessEqualsThan" | 
           "<operator>.greaterThan" | 
           "<operator>.lessThan" | 
           "<operator>.notEquals" |
           "<operator>.logicalNot" => true
      case _ => false
    }
  }

  private def defaultDecl(id: Long, code: String): String = {
    "; x%d = \"%s\"\n(declare-fun x%d () Int)\n".format(id, code, id)
  }
  private def defaultAssert(id: Long): String = assertIntFmt.format(id, id, id, id)
  private def assertBool(id: Long): String = "(assert (or (= 0 x%d) (= 1 x%d)))\n".format(id, id)
  private def assertEqual(id: Long, value: String): String = "(assert (= x%d %s))\n".format(id, value)

  private def declareCall(call: nodes.Call): String = {
    /* If the call is to one of the default operators, do not output any declarations for the Call nodes. 
     * Simply recurse for each of the call's arguments.
     */
    if (isOperatorCall(call.name)) return call.argument.map(arg => declareNode(arg)).mkString

    val assert = if (call.typeFullName == "boolean") assertBool(call.id) else defaultAssert(call.id)
    var decls = defaultDecl(call.id, call.code) + assert

    call.name match {
      // Do not recurse on the arguments to these methods
      // @TODO: verify that the boolean methods can be included here
      case "<operator>.fieldAccess" | 
           "<operator>.indirectFieldAccess" | 
           "strncmp" | 
           "strcmp" | 
           "fopen" |
           "isEmpty" | 
           "include" | 
           "parseBoolean" => decls
      // Example that restricts the range of a method
      case "getopt" => {
        "(declare-fun x%d () Int)\n".format(call.id) + 
        "(assert (and (>= x%d (- 1)) (<= x%d 255)))\n".format(call.id, call.id)
      }
      case _ => decls + call.argument.map(arg => declareNode(arg)).mkString
    }
  }

  private def declareLiteral(literal: nodes.Literal): String = {
    var decl = defaultDecl(literal.id, literal.code) + defaultAssert(literal.id)

    literal.typeFullName match {
      case "int" => defaultDecl(literal.id, literal.code) + assertEqual(literal.id, literal.code)
      case "boolean" => {
        val value = if (literal.code == "true") "1" else "0"
        assertions += assertEqual(literal.id, value)
        defaultDecl(literal.id, literal.code) + assertBool(literal.id)
      }
      case "java.lang.String" => ""
      case "char" => {
        if (literal.code.startsWith("'") && literal.code == "'\\0'") {
          assertions += assertEqual(literal.id, "0")
        } else {
          val un = StringContext treatEscapes literal.code
          assertions += "(assert (= x%d %d))\n".format(literal.id, un.charAt(1).toInt)
        }
        decl
      }
      case _ => {
        if (literal.code == "null") assertions += assertEqual(literal.id, "0")
        else assertions += assertEqual(literal.id, literal.code)
        decl
      }
    }
  }

  private def declareIdentifier(ident: nodes.Identifier): String = {
    // Eliminates duplicate Joern IDs for identifiers
    val id = ids(ident.code)
    val assert = if (ident.typeFullName == "boolean") assertBool(id) else defaultAssert(id)
    if (ident.name == "NULL") assertions += assertEqual(id, "0")
    defaultDecl(id, ident.code) + assert
  }

  private def declareNode(node: CfgNode): String = {
    if (seenIds.contains(node.id)) return ""

    seenIds.add(node.id)
    
    node match {
      case call: nodes.Call => declareCall(call)
      case literal: nodes.Literal => declareLiteral(literal)
      case ident: nodes.Identifier => declareIdentifier(ident)
      case _ => "" /* Ignore any other node types */
    }
  }

  private def generateOperatorCallConstraint(call: nodes.Call): String = {
    val childConstraints = call.argument.map(arg => generateConstraint(arg)).mkString
    call.name match {
      case "<operator>.logicalAnd" => " (and" + childConstraints + ")"
      case "<operator>.logicalOr" => " (or" + childConstraints + ")"
      case "<operator>.equals" => " (=" + childConstraints + ")"
      case "<operator>.greaterEqualsThan" => " (>=" + childConstraints + ")"
      case "<operator>.lessEqualsThan" => " (<=" + childConstraints + ")"
      case "<operator>.greaterThan" => " (>" + childConstraints + ")"
      case "<operator>.lessThan" => " (<" + childConstraints + ")"
      case "<operator>.notEquals" => " (not (=" + childConstraints + "))"
      case "<operator>.logicalNot" => " (not" + childConstraints + ")"
      case _ => ""
    }
  }

  private def generateCallConstraint(call: nodes.Call): String = {
    if (isOperatorCall(call.name)) generateOperatorCallConstraint(call)
    else if (call.astParent.isInstanceOf[nodes.Call]) { 
      val parent = call.astParent.asInstanceOf[nodes.Call]
      if (parent.name == "<operator>.logicalNot") return " (= x%d 0)".format(call.id)
      else if (parent.name.startsWith("<operator>.logical")) return " (= x%d 1)".format(call.id)
      " x%d".format(call.id)
    } else if (call.astParent.isInstanceOf[nodes.ControlStructure]) " (not (= x%d 0))".format(call.id)
    else " x%d".format(call.id)
  }

  private def generateConstraint(node: CfgNode): String = {
    /* We care about calls, literals, and identifiers and their types */
    node match {
      case call: nodes.Call => generateCallConstraint(call)
      case literal: nodes.Literal => if (literal.typeFullName == "java.lang.String") "" else " x%d".format(literal.id)
      case ident: nodes.Identifier => {
        val id = ids(ident.code)
        // @TODO: this block and generateCallConstraints are identical...
        if (ident.astParent.isInstanceOf[nodes.Call]) {
          val parent = ident.astParent.asInstanceOf[nodes.Call]
          if (parent.name == "<operator>.logicalNot") return " (= x%d 0)".format(id)
          else if (parent.name.startsWith("<operator>.logical")) return " (= x%d 1)".format(id)
          " x%d".format(id)
        } else if (ident.astParent.isInstanceOf[nodes.ControlStructure]) " (not (= x%d 0))".format(id)
        else " x%d".format(id)
      }
      case _ => " ???" /* this shouldn't happen... @TODO: better error handling */
    }
  }

  def count: BigDecimal = {
    if (declarations == "") return BigDecimal("0")

    val constraint = String.format("%s%s\n(check-sat)\n", declarations, assertions)
    if (gVerbose) printf("\n\nCOUNT CONSTRAINTS: \n%s\n\n", constraint)
    val result = solver.isSatisfiable(constraint)
    if (result) {
      val count = solver.countInts(solverBound)
      val domain = domainSize
      val ratio = BigDecimal(count) / BigDecimal(domain)
      if (gVerbose) printf("count: %d, domain: %d, probability: %f\n", count, domain, ratio)
      ratio
    } else {
      println("Unsatisfiable!")
      println(topNode)
      println(constraint)
      BigDecimal("0")
    }
  }
  
  private def domainSize: BigInteger = {
    val constraint = String.format("%s\n(check-sat)\n", declarations)
    if (gVerbose) printf("\n\nDOMAIN CONSTRAINTS: \n%s\n\n", constraint)
    val result = solver.isSatisfiable(constraint)
    if (result) solver.countInts(solverBound)
    else {
      println("domain size: Unsatisfiable")
      BigInteger("0")
    }
  }

end BranchConstraint

def isConditionInControlStructure(v: Node): Boolean = cpg.controlStructure.condition.contains(v)

// @TODO: deleting this breaks it for some reason...
@main def extract(cpgFile: String, outFile: String) = {}

class VertexEdges {
  var in: List[StoredNode] = List()
  var out: List[StoredNode] = List()

  def addIn(n: StoredNode) = in = in :+ n
  def addOut(n: StoredNode) = out = out :+ n
}

class FoldedEdge {
  var src: StoredNode = null
  var dst: StoredNode = null
  var subEdges: List[Edge] = List()

  def addSubEdge(e: Edge) = subEdges = subEdges :+ e
}

def foldEdges(vertices: List[StoredNode], edges: List[Edge]): List[Edge] = {

  def isFoldableCall(node: StoredNode): Boolean = {
    node match {
      case call: nodes.Call => !gInterestingNames.contains(call.callee.head.name)
      case _ => false 
    }
  }

  var edgeMap: mutable.Map[Long,VertexEdges] = mutable.Map[Long,VertexEdges]()

  for (v <- vertices) {
    val foo = VertexEdges()
    for (edge <- edges.filter(e => e.src.id == v.id)) foo.addOut(edge.dst)
    for (edge <- edges.filter(e => e.dst.id == v.id)) foo.addIn(edge.src)
    edgeMap(v.id) = foo
  }

  var keep: List[Edge] = List()
  var foldedEdges: List[FoldedEdge] = List()

  for (edge <- edges) {
    val src = edge.src
    val dst = edge.dst
    val srcEdges = edgeMap(src.id)
    val dstEdges = edgeMap(dst.id)

    if (
      srcEdges.in.size == 0 || 
      srcEdges.out.size > 1 || 
      dstEdges.in.size > 1 || 
      !isFoldableCall(dst)
    ) {
      keep = keep :+ edge
    } else {
      val foldedEdge = FoldedEdge()
      foldedEdge.addSubEdge(edge)
      foldedEdge.src = src
      var e = edge
      while (
        edgeMap(e.dst.id).in.size == 1 && 
        edgeMap(e.dst.id).out.size == 1 && 
        isFoldableCall(e.dst)
      ) {
        e = edges.filter(e2 => e2.src.id == e.dst.id).head
        foldedEdge.addSubEdge(e)
      }
      foldedEdge.dst = e.dst
      foldedEdges = foldedEdges :+ foldedEdge
      keep = keep :+ Edge(foldedEdge.src, foldedEdge.dst)
    }
  }

  for (fe <- foldedEdges) {
    for (e <- fe.subEdges) {
      keep = keep.filterNot(k => k.src.id == e.src.id && e.src.id != fe.src.id)
    }
  }
  keep
}

class ProbCfgGenerator {
  /* Derives/extends CfgGenerator */

  val edgeType: String = "ProbCFG" 

  def generate(methodNode: Method): Graph = {
    var vertices = methodNode.cfgNode.l ++ List(methodNode, methodNode.methodReturn) ++ methodNode.parameter.l
    var verticesToDisplay = vertices.filter(cfgNodeShouldBeDisplayed)

    def edgesToDisplay(srcNode: StoredNode, visited: List[StoredNode] = List()): List[Edge] = {
      if (visited.contains(srcNode)) List()
      else {
        val children = expand(srcNode).filter(x => vertices.contains(x.dst))
        val (visible, invisible) = children.partition(x => cfgNodeShouldBeDisplayed(x.dst))
        visible.toList ++ invisible.toList.flatMap { n =>
          edgesToDisplay(n.dst, visited ++ List(srcNode)).map(
            y => Edge(srcNode, y.dst, edgeType = edgeType)
          )
        }
      }
    }

    var edges = verticesToDisplay.flatMap {v => edgesToDisplay(v)}.distinct
    var allIdsReferencedByEdges = edges.flatMap {edge => Set(edge.src.id, edge.dst.id)}
    val graphVertices = verticesToDisplay.filter(node => allIdsReferencedByEdges.contains(node.id))
    val graphEdges = 
      if (gShouldFold) labelEdges(graphVertices, foldEdges(graphVertices, edges))
      else labelEdges(graphVertices, edges)

    Graph(graphVertices, graphEdges)
  }

  protected def expand(v: StoredNode): Iterator[Edge] = v._cfgOut.map(node => Edge(v, node, edgeType = edgeType))

  private def labelEdges(vertices: List[StoredNode], edges: List[Edge]): List[Edge] = {
    edges.map(e => {
      if (isConditionInControlStructure(e.src)) {
        val cond = e.src.asInstanceOf[nodes.CfgNode]
        val cs = cpg.controlStructure.find(_.condition.contains(cond)).get
        labelControlEdge(cond, cs, e, edges)
      } else e
    })
   }

  private def labelControlEdge(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge, edges: List[Edge]): Edge = {
    cs.controlStructureType match {
      case "IF" => labelIfEdge(cond, cs, edge, edges)
      case "WHILE" => labelWhileEdge(cond, cs, edge)
      case "FOR" => labelForEdge(cond, cs, edge, edges)
      /* @TODO: implement labelSwitchEdge */
      case _ => edge
    }
  }

  def escapeCond(cond: String): String = {
    cond.replace("\"", "\\\"").map(c => {
      if (c == '\n' || c == '\t') ' '
      else c 
    })
  }

  private def newProbEdge(edge: Edge, isTrue: Boolean, isLoop: Boolean = false): ProbEdge = {
    val node = edge.src.asInstanceOf[nodes.CfgNode]
    val selectivity = if (isLoop) BigDecimal(1) else BranchConstraint(node).count
    var prob = if (isTrue) selectivity else 1 - selectivity
    var cond = escapeCond(node.code)
    if (!isTrue) cond = s"NOT ($cond)"
    val tf = if (isTrue) 1 else 0
    ProbEdge(edge.src, edge.dst, prob = s"$prob", cond = cond, tf = tf, loop = isLoop)
  }

  private def labelForEdge(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge, edges: List[Edge]): Edge = {
    /* This isn't very pretty... but the destination edges wasn't found when traversing either
     * the control structure's whenTrue AST or whenFalse AST.
     */
    val isTrue = cs.astChildren.repeat(_.astChildren)(_.until(_.id(edge.dst.id))).contains(edge.dst)
    newProbEdge(edge, isTrue, isLoop = true)
  }

  private def labelWhileEdge(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge): Edge = {
    val isTrue = cs.whenTrue.ast.contains(edge.dst)
    newProbEdge(edge, isTrue, isLoop = true)
  }

  private def getParentLoop(cs: nodes.ControlStructure): nodes.ControlStructure = {
    /*
     * From the branch condition, climb up the AST to find all of the for and
     * while loop control structures. The first one in the list is going to be
     * the loop that the given control structure is nested under.
     */
    val loops = cs.astParent.toList.repeat(_.astParent)(
      _.emit(_.isControlStructure).until(_.isMethod) // climb entire AST
    ).filterNot(_.isBlock)
    return loops.head.asInstanceOf[nodes.ControlStructure]
  }

  private def labelIfBreak(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge): Edge = {
    /*
     * Climb down the AST from the parent loop control structure. If we reach 
     * the edge's destination node, then the edge represents the false edge of 
     * the condition.
     */
    val parentLoop = getParentLoop(cs)
    val isTrue = !parentLoop.whenFalse.ast.contains(edge.dst)
    newProbEdge(edge, isTrue)
  }

  private def labelIfContinue(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge): Edge = {
    /*
     * Climb down the AST from the parent loop control structure, following the 
     * true branch. If we reach the edge's destination node, then the destination 
     * node is in the loop body. Therefore, execution did not continue to the 
     * beginning of the loop and the edge is the false edge of the condition.
     */
    val parentLoop = getParentLoop(cs)
    val isTrue = !parentLoop.whenTrue.ast.contains(edge.dst)
    newProbEdge(edge, isTrue)
  }

  private def labelIfEdge(cond: nodes.CfgNode, cs: nodes.ControlStructure, edge: Edge, edges: List[Edge]): Edge = {
    /*
     * Some strange edge conditions exist if the body of an if statement 
     * contains only a break statement or a continue statement.
     */
    val trueAstChild = cs.whenTrue.astChildren.head
    if (trueAstChild.isInstanceOf[nodes.ControlStructure]) {
      if (!trueAstChild.asInstanceOf[nodes.ControlStructure].isBreak.isEmpty) return labelIfBreak(cond, cs, edge)
      if (!trueAstChild.asInstanceOf[nodes.ControlStructure].isContinue.isEmpty) return labelIfContinue(cond, cs, edge)
    }

    val isTrue = cs.whenTrue.ast.contains(edge.dst)
    newProbEdge(edge, isTrue)
  }

  private def cfgNodeShouldBeDisplayed(v: Node): Boolean =
    isConditionInControlStructure(v) || 
    v.isInstanceOf[nodes.Method] ||
    v.isInstanceOf[nodes.MethodReturn] ||
    v.isInstanceOf[nodes.Return] || 
    v.isInstanceOf[nodes.Call] 
}

def genProbCfg(methods: Iterator[nodes.Method]): Iterator[Graph] = methods.map(genProbCfg)
def genProbCfg(method: nodes.Method): Graph = ProbCfgGenerator().generate(method)

def nodeToFacts(node: nodes.CfgNode) = {
  node match {
    case call: nodes.Call => {
      val callee = call.callee.toList.head
      if (gInterestingNames.contains(callee.name)) {
        writeOut("calls(%d, %d, %d).\n".format(call.method.id, callee.id, call.id))
      }
    }
    case meth: nodes.Method => writeOut("method(%d, \"%s\").\n".format(meth.id, meth.name))
    case ret: nodes.MethodReturn => writeOut("returns(%d, %d). %% %s\n".format(ret.method.id, ret.id, ret.method.name))
    case _ => ()
  }

  if (isConditionInControlStructure(node)) {
    node.lineNumber match {
      case Some(n) => {
        val file = node.file.name.head
        val func = node.method.name
        writeOut("location(%d, \"%s\", \"%s\", %s).\n".format(node.id, file, func, n))
      }
      case None => ()
    }
  }
}

def edgeToFacts(edge: Edge) = {
  edge match {
    case e: ProbEdge => {
      if (e.loop) writeOut("cfg_edge(%d,%d).\n".format(e.src.id, e.dst.id))
      else writeOut("%s::cfg_edge(%d,%d).\n".format(e.prob, e.src.id, e.dst.id))
      writeOut("branch(%d,%d,%d).\n".format(e.src.id, e.dst.id, e.tf))
      writeOut("branch_cond(%d,%d,\"%s\").\n".format(e.src.id, e.tf, e.cond))
    }
    case _ => writeOut("cfg_edge(%d,%d).\n".format(edge.src.id, edge.dst.id))
  }
}

def printPreamble() = {
  writeOut(":- discontiguous cfg_edge/2.\n")
  writeOut(":- discontiguous returns/2.\n")
  writeOut(":- discontiguous calls/3.\n")
  writeOut("\n:- use_module(library(pita)).\n")
  writeOut("\n:- pita.\n")
  writeOut("\n:- begin_lpad.\n\n")
}

@main def exec(
  cpgFile: String, 
  outFile: String, 
  verbose: Boolean, 
  shouldFold: Boolean, 
  onlyReachable: Boolean, 
  stdOut: Boolean
) = {
  importCpg(cpgFile, "testCPG")

  var methods = cpg.method.filterNot(_.isExternal)

  // Find nodes that result from user input
  if (onlyReachable) {
    val interesting = getInterestingMethods()

    // We are only interested in methods that user input can reach
    gInterestingNames = cpg.method.filterNot(_.isExternal).filter(
      m => m.methodReturn.toList.reachableBy(interesting).size > 0
    ).name.l

    methods = cpg.method.filterNot(_.isExternal).filter(
      m => m.methodReturn.toList.reachableBy(interesting).size > 0
    )
  } else gInterestingNames = cpg.method.filterNot(_.isExternal).name.l
  
  gVerbose = verbose
  gShouldFold = shouldFold
  val file = new java.io.File(outFile)
  gOutFile = new BufferedWriter(new FileWriter(file))
  gStdOut = stdOut

  val cfgs = genProbCfg(methods)

  printPreamble()

  for (cfg <- cfgs) {
    cfg.edges.map(edgeToFacts) 
    cfg.vertices.map(_.asInstanceOf[nodes.CfgNode]).map(nodeToFacts) 
  }
  val loops = cpg.controlStructure.isWhile.l ++ cpg.controlStructure.isFor.l
  loops.map(l => {
    writeOut("loop(%d).\n".format(l.condition.head.id))
    l.condition.ast.toList.map(n => {
      writeOut("in_loop_cond(%d, %d).\n".format(n.id, l.condition.head.id))
    })
  })

  writeOut(":- end_lpad.\n\n")
  gOutFile.close()
}

/* Functions for finding reachable methods for C programs.
 * 
 * A C program can receive user input in the following ways:
 *  - [x] argc or argv
 *  - [x] getopt return value or optarg
 *  - [x] fread(arg1,...)
 *  - [x] fgets(arg1,...)
 *  - [x] fgetc
 *  - [x] getchar
 *  - [x] read(...,arg2,...)
 *  - [x] scanf(...,arg2...argN)
 *  - [x] fscanf(...,arg3...argN)
 */
def getInterestingMethods(): List[nodes.CfgNode] = {
  val mainArgs = 
    cpg.identifier.name("argc").l ++ 
    cpg.identifier.name("argv")
  val getOpt = 
    cpg.identifier.name("getopt").l ++ 
    cpg.call.name("getopt").inAssignment.target.l
  val freads = 
    cpg.call.name("fread").argument(2).l ++ 
    cpg.call.name("fgets").argument(2).l
  val getchar = 
    cpg.call.name("getchar").inAssignment.target.l ++ 
    cpg.call.name("fgetc").inAssignment.target.l
  val read = cpg.call.name("read").argument(2).l
  val scanf = cpg.call.name("scanf").argument.l.drop(1)
  val fscanf = cpg.call.name("fscanf").argument.l.drop(2)
  return mainArgs ++ getOpt ++ freads ++ getchar ++ read ++ scanf ++ fscanf
}