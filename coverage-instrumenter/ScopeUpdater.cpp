#include "ScopeUpdater.h"

#include "RarePathCoverageCommon.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"

using namespace clang;
using namespace llvm;

namespace rarepath {

void WriteToFile(const std::string &Filename, Rewriter &TheRewriter) {
    std::error_code EC;
    llvm::raw_fd_ostream outFile(Filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Error opening output file: " << EC.message() << "\n";
        return;
    }

    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(outFile);
}

// ScopeVisitor

int ScopeVisitor::instrumentBranch(Stmt *s) {
    if (isa<CompoundStmt>(s)) return false;

    SourceRange range = s->getSourceRange();
    std::string original = TheRewriter.getRewrittenText(range);
    SourceLocation semicolon = range.getEnd();
    if (semicolon.isValid()) {
        range.setEnd(semicolon.getLocWithOffset(1));
        std::string compoundStmt = "{\n" + original + ";\n}";
        TheRewriter.ReplaceText(range, compoundStmt);
        return compoundStmt.size();
    }
    return 0;
}

bool ScopeVisitor::insertElse(IfStmt *s, int offset) {
    CompoundStmt *CS = dyn_cast<CompoundStmt>(s->getThen());
    if (nullptr == CS) outs() << "Something went wrong! " << stmtToString(s) << "\n";
    SourceLocation insertLoc = CS->getRBracLoc().getLocWithOffset(1);
    std::string elseBlock = "else { }";
    TheRewriter.InsertTextAfter(insertLoc, elseBlock);
    return true;
}

bool ScopeVisitor::VisitIfStmt(IfStmt *s) {
    Stmt *body = s->getThen();
    if (!body) return true;
    int modified = instrumentBranch(body);

    if (modified) return true;

    Stmt *ebody = s->getElse();
    if (!ebody) return insertElse(s, modified);
    if (isa<IfStmt>(ebody)) return true;
    instrumentBranch(ebody);
    return true;
}

bool ScopeVisitor::VisitForStmt(ForStmt *s) {
    Stmt *body = s->getBody();
    if (!body) return true;
    instrumentBranch(body);
    return true;
}

bool ScopeVisitor::VisitWhileStmt(WhileStmt *s) {
    Stmt *body = s->getBody();
    if (!body) return true;
    instrumentBranch(body);
    return true;
}

// ScopeASTConsumer

class ScopeASTConsumer : public ASTConsumer {
   public:
    explicit ScopeASTConsumer(Rewriter &R) : Visitor(R) {}

    void HandleTranslationUnit(ASTContext &Context) override { Visitor.TraverseDecl(Context.getTranslationUnitDecl()); }

   private:
    ScopeVisitor Visitor;
};

// ScopeFrontendAction

std::unique_ptr<ASTConsumer> ScopeFrontendAction::CreateASTConsumer(CompilerInstance &CI, StringRef file) {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<ScopeASTConsumer>(TheRewriter);
}

void ScopeFrontendAction::EndSourceFileAction() { WriteToFile(OutputFilename, TheRewriter); }

}  // namespace rarepath
