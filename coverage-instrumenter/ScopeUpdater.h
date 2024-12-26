#ifndef SCOPE_UPDATER_H
#define SCOPE_UPDATER_H

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"

using namespace clang;
using namespace llvm;

namespace rarepath {

class ScopeVisitor : public RecursiveASTVisitor<ScopeVisitor> {
   public:
    explicit ScopeVisitor(Rewriter &R) : TheRewriter(R) {}
    bool VisitIfStmt(IfStmt *s);
    bool VisitForStmt(ForStmt *s);
    bool VisitWhileStmt(WhileStmt *s);

   private:
    Rewriter &TheRewriter;
    int instrumentBranch(Stmt *s);
    bool insertElse(IfStmt *s, int offset);
};

class ScopeFrontendAction : public ASTFrontendAction {
   public:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override;
    void EndSourceFileAction() override;

   private:
    Rewriter TheRewriter;
    ScopeVisitor Visitor{TheRewriter};
};
}  // namespace rarepath

#endif  // SCOPE_UPDATER_H