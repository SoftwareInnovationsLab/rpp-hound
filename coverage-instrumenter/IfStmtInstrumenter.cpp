#include "IfStmtInstrumenter.h"

#include "RarePathCoverageCommon.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Format/Format.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace llvm;

namespace rarepath {

bool BranchStatementPrinterVisitor::instrumentBranch(Stmt *s, std::string PlaceHolder) {
    if (SeenStmts.find(s) != SeenStmts.end()) {
        return true;
    }
    if (CompoundStmt *cs = dyn_cast<CompoundStmt>(s)) {
        SourceLocation insertLoc = cs->getLBracLoc().getLocWithOffset(1);
        TheRewriter.InsertTextBefore(insertLoc, PlaceHolder);
        return true;
    } else {
        errs() << "Something went wrong (instrumentBranch) " << stmtToString(s) << "\n";
    }
    return true;
}

bool BranchStatementPrinterVisitor::instrumentLoop(Stmt *s, std::string PlaceHolder) {
    if (CompoundStmt *cs = dyn_cast<CompoundStmt>(s)) {
        SourceLocation LBrac = cs->getLBracLoc().getLocWithOffset(1);
        SourceLocation RBrac = cs->getRBracLoc().getLocWithOffset(1);
        TheRewriter.InsertTextBefore(LBrac, PlaceHolder);
        TheRewriter.InsertTextBefore(RBrac, PlaceHolder);
        TheRewriter.InsertTextBefore(RBrac.getLocWithOffset(-2), PlaceHolder);
        return true;
    } else {
        errs() << "Something went wrong (instrumentLoop) " << stmtToString(s) << "\n";
    }
    return true;
}

bool BranchStatementPrinterVisitor::instrumentIfStmt(IfStmt *If, std::string FalsePlaceholder) {
    Stmt *ifBody = If->getThen();
    if (!ifBody) {
        errs() << "Unexpected if without then: " << stmtToString(If) << "\n";
        return true;  // This probably shouldn't ever happen...
    }
    instrumentBranch(ifBody, FalsePlaceholder + newPlaceholder());

    Stmt *elseBody = If->getElse();
    if (!elseBody) {
        errs() << "Unexpected if without an else: " << stmtToString(If) << "\n";
        return true;  // This also shouldn't ever happen...
    }

    if (IfStmt *elseIf = dyn_cast<IfStmt>(elseBody)) {
        instrumentIfStmt(elseIf, FalsePlaceholder + newPlaceholder());
        SeenStmts.insert(elseIf->getThen());
    } else {
        instrumentBranch(elseBody, FalsePlaceholder + newPlaceholder());
        SeenStmts.insert(elseBody);
    }
    return true;
}

bool BranchStatementPrinterVisitor::VisitReturnStmt(ReturnStmt *s) {
    outs() << "return stmt: " << stmtToString(s) << "\n";
    SourceLocation Start = s->getBeginLoc();
    TheRewriter.InsertTextBefore(Start, "rpcov_trace_add(__FILE__, 0, \"Return\");");
    return true;
}

bool BranchStatementPrinterVisitor::VisitIfStmt(IfStmt *s) { return instrumentIfStmt(s, ""); }

bool BranchStatementPrinterVisitor::VisitForStmt(ForStmt *For) {
    Stmt *ForBody = For->getBody();
    if (!ForBody) {
        errs() << "Unexpected for without body: " << stmtToString(For) << "\n";
        return true;  // This shouldn't ever happen...
    }
    instrumentLoop(ForBody, newPlaceholder());
    return true;
}

bool BranchStatementPrinterVisitor::VisitWhileStmt(WhileStmt *While) {
    Stmt *WhileBody = While->getBody();
    if (!WhileBody) {
        errs() << "Unexpected for without body: " << stmtToString(While) << "\n";
        return true;  // This shouldn't ever happen...
    }
    instrumentLoop(WhileBody, newPlaceholder());
    return true;
}

void BranchStatementPrinterVisitor::WriteToFile(const std::string &Filename) {
    std::error_code EC;
    llvm::raw_fd_ostream outFile(Filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Error opening output file: " << EC.message() << "\n";
        return;
    }

    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(outFile);
}

class BranchStatementPrinterASTConsumer : public ASTConsumer {
   public:
    explicit BranchStatementPrinterASTConsumer(Rewriter &R) : Visitor(R) {}

    void HandleTranslationUnit(ASTContext &Context) override { Visitor.TraverseDecl(Context.getTranslationUnitDecl()); }

   private:
    BranchStatementPrinterVisitor Visitor;
};

std::unique_ptr<ASTConsumer> BranchStatementPrinterFrontendAction::CreateASTConsumer(CompilerInstance &CI,
                                                                                     StringRef file) {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<BranchStatementPrinterASTConsumer>(TheRewriter);
}

void BranchStatementPrinterFrontendAction::EndSourceFileAction() { Visitor.WriteToFile(OutputFilename); }

}  // namespace rarepath
