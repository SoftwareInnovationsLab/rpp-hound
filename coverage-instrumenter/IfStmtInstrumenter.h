#ifndef IF_STMT_INSTRUMENTER_H
#define IF_STMT_INSTRUMENTER_H
//

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

class BranchStatementPrinterVisitor : public RecursiveASTVisitor<BranchStatementPrinterVisitor> {
   public:
    explicit BranchStatementPrinterVisitor(Rewriter &R) : TheRewriter(R) {}
    bool VisitIfStmt(IfStmt *s);
    bool VisitForStmt(ForStmt *s);
    bool VisitWhileStmt(WhileStmt *s);
    bool VisitReturnStmt(ReturnStmt *s);
    void WriteToFile(const std::string &Filename);

   private:
    std::set<Stmt *> SeenStmts;
    Rewriter &TheRewriter;
    bool instrumentBranch(Stmt *s, std::string PlaceHolder);
    bool instrumentLoop(Stmt *s, std::string PlaceHolder);
    bool instrumentIfStmt(IfStmt *If, std::string FalsePlaceholder);
};

class BranchStatementPrinterFrontendAction : public ASTFrontendAction {
   public:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override;
    void EndSourceFileAction() override;

   private:
    Rewriter TheRewriter;
    BranchStatementPrinterVisitor Visitor{TheRewriter};
};

}  // namespace rarepath

#endif  // IF_STMT_INSTRUMENTER_H