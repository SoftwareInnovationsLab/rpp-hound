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

class UpdateLineNumsVisitor : public RecursiveASTVisitor<UpdateLineNumsVisitor> {
   public:
    explicit UpdateLineNumsVisitor(Rewriter &R) : TheRewriter(R) {}
    bool VisitIfStmt(IfStmt *s);
    bool VisitForStmt(ForStmt *s);
    bool VisitWhileStmt(WhileStmt *s);
    void WriteToFile(const std::string &Filename);

   private:
    std::set<Stmt *> SeenStmts;
    Rewriter &TheRewriter;
    int getLineNumber(const Stmt *s);
    bool replacePlaceholder(Stmt *S, std::string ExpectedPlaceHolder, std::string ReplacementCalls);
    bool replaceLoopPlaceholders(Stmt *S, int line);
    bool replaceIfStmtPlaceholder(IfStmt *If, std::string ExpectedPlaceHolder, std::string ReplacementCalls);
    void replacePlaceholderAfterLoop(Stmt *S, int line);
};

class UpdateLineNumsFrontendAction : public ASTFrontendAction {
   public:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override;
    void EndSourceFileAction() override;

   private:
    Rewriter TheRewriter;
    UpdateLineNumsVisitor Visitor{TheRewriter};
};

}  // namespace rarepath
