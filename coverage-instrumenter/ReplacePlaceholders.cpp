#include "ReplacePlaceholders.h"

#include "FunctionInserter.h"
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

using namespace clang;
using namespace llvm;

namespace rarepath {

std::string newCall(int line, std::string TF) {
    return AddToTraceFunctionName + "(__FILE__, " + std::to_string(line) + ", \"" + TF + "\");";
}

int UpdateLineNumsVisitor::getLineNumber(const Stmt *s) {
    const SourceManager &sourceManager = TheRewriter.getSourceMgr();
    SourceLocation startLoc = s->getBeginLoc();
    if (startLoc.isValid() && !sourceManager.isInSystemMacro(startLoc))
        return sourceManager.getSpellingLineNumber(startLoc);
    else
        return -1;  // Invalid line number
}

bool UpdateLineNumsVisitor::replacePlaceholder(Stmt *S, std::string ExpectedPlaceHolder, std::string ReplacementCalls) {
    if (SeenStmts.find(S) != SeenStmts.end() || !isa<CompoundStmt>(S)) return true;
    CompoundStmt *CS = dyn_cast<CompoundStmt>(S);
    SourceLocation Start = CS->getLBracLoc().getLocWithOffset(1);
    SourceLocation End = CS->body_front()->getEndLoc().getLocWithOffset(-1);
    Stmt *LastPlaceHolder = nullptr;
    for (auto BodyStmt : CS->body()) {
        if (CallExpr *Call = dyn_cast<CallExpr>(BodyStmt)) {
            Expr *Callee = Call->getCallee();
            std::string targetCallName = "PlaceHolder";
            if (exprToString(Callee).compare(targetCallName) == 0) {
                LastPlaceHolder = BodyStmt;
                continue;
            }
        }
        break;
    }
    End = LastPlaceHolder->getEndLoc().getLocWithOffset(1);
    SourceRange Range = SourceRange(Start, End);
    TheRewriter.ReplaceText(Range, ReplacementCalls);
    return true;
}

void UpdateLineNumsVisitor::replacePlaceholderAfterLoop(Stmt *S, int line) {
    if (!isa<CompoundStmt>(S)) return;

    SourceManager &SM = TheRewriter.getSourceMgr();

    CompoundStmt *CS = dyn_cast<CompoundStmt>(S);
    SourceLocation BeginLoc = CS->getRBracLoc().getLocWithOffset(1);

    auto NextToken = Lexer::findNextToken(BeginLoc, SM, LangOptions());
    while (NextToken && (std::string(NextToken->getName()).compare("semi") != 0)) {
        NextToken = Lexer::findNextToken(NextToken->getLocation(), SM, LangOptions());
    }
    if (!NextToken) {
        outs() << "No token following closing brace: " << stmtToString(CS) << "\n";
        return;
    }
    SourceLocation EndLoc = NextToken->getLocation();

    TheRewriter.ReplaceText(SourceRange(BeginLoc, EndLoc), newCall(line, "Exit"));
}

bool UpdateLineNumsVisitor::replaceLoopPlaceholders(Stmt *S, int line) {
    if (CompoundStmt *cs = dyn_cast<CompoundStmt>(S)) {
        SourceLocation EnterBegin = cs->body_front()->getBeginLoc();
        SourceLocation EnterEnd = cs->body_front()->getEndLoc().getLocWithOffset(1);
        SourceRange EnterRange = SourceRange(EnterBegin, EnterEnd);
        TheRewriter.ReplaceText(EnterRange, newCall(line, "Enter"));

        SourceLocation EndBegin = cs->body_back()->getBeginLoc();
        SourceLocation EndEnd = cs->getRBracLoc().getLocWithOffset(-1);
        SourceRange EndRange = SourceRange(EndBegin, EndEnd);
        TheRewriter.ReplaceText(EndRange, newCall(line, "End"));

        replacePlaceholderAfterLoop(S, line);

        return true;
    } else {
        errs() << "Something went wrong (replaceLoopPlaceholders) " << stmtToString(S) << "\n";
    }
    return true;
}

bool UpdateLineNumsVisitor::replaceIfStmtPlaceholder(IfStmt *If, std::string ExpectedPlaceHolder,
                                                     std::string ReplacementCalls) {
    Stmt *ifBody = If->getThen();
    if (!ifBody) {
        errs() << "Unexpected if without then: " << stmtToString(If) << "\n";
        return true;  // This probably shouldn't ever happen...
    }
    int ifLine = getLineNumber(If);
    if (ifLine == -1) {
        errs() << "Unable to determine line number for: " << stmtToString(If) << "\n";
        return true;
    }
    replacePlaceholder(ifBody, ExpectedPlaceHolder + newPlaceholder(), ReplacementCalls + newCall(ifLine, "T"));

    Stmt *elseBody = If->getElse();
    if (!elseBody) {
        errs() << "Unexpected if without an else: " << stmtToString(If) << "\n";
        return true;  // This also shouldn't ever happen...
    }

    if (IfStmt *elseIf = dyn_cast<IfStmt>(elseBody)) {
        replaceIfStmtPlaceholder(elseIf, ExpectedPlaceHolder + newPlaceholder(),
                                 ReplacementCalls + newCall(ifLine, "F"));
        SeenStmts.insert(elseIf->getThen());
    } else {
        replacePlaceholder(elseBody, ExpectedPlaceHolder + newPlaceholder(), ReplacementCalls + newCall(ifLine, "F"));
        SeenStmts.insert(elseBody);
    }
    return true;
}

bool UpdateLineNumsVisitor::VisitIfStmt(IfStmt *s) { return replaceIfStmtPlaceholder(s, "", ""); }

bool UpdateLineNumsVisitor::VisitForStmt(ForStmt *For) {
    Stmt *ForBody = For->getBody();
    if (!ForBody) {
        errs() << "Unexpected for without body: " << stmtToString(For) << "\n";
        return true;  // This shouldn't ever happen...
    }
    int line = getLineNumber(For);
    replaceLoopPlaceholders(ForBody, line);
    return true;
}

bool UpdateLineNumsVisitor::VisitWhileStmt(WhileStmt *While) {
    Stmt *WhileBody = While->getBody();
    if (!WhileBody) {
        errs() << "Unexpected for without body: " << stmtToString(While) << "\n";
        return true;  // This shouldn't ever happen...
    }
    int line = getLineNumber(While);
    replaceLoopPlaceholders(WhileBody, line);
    return true;
}

void UpdateLineNumsVisitor::WriteToFile(const std::string &Filename) {
    std::error_code EC;
    llvm::raw_fd_ostream outFile(Filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Error opening output file: " << EC.message() << "\n";
        return;
    }

    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(outFile);
}

class UpdateLineNumsASTConsumer : public ASTConsumer {
   public:
    explicit UpdateLineNumsASTConsumer(Rewriter &R) : Visitor(R) {}

    void HandleTranslationUnit(ASTContext &Context) override { Visitor.TraverseDecl(Context.getTranslationUnitDecl()); }

   private:
    UpdateLineNumsVisitor Visitor;
};

std::unique_ptr<ASTConsumer> UpdateLineNumsFrontendAction::CreateASTConsumer(CompilerInstance &CI, StringRef file) {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<UpdateLineNumsASTConsumer>(TheRewriter);
}

void UpdateLineNumsFrontendAction::EndSourceFileAction() { Visitor.WriteToFile(OutputFilename); }

}  // namespace rarepath
