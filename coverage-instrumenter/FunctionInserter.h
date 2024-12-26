#ifndef FUNCTION_INSERTER_H
#define FUNCTION_INSERTER_H

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

namespace rarepath {

extern std::string InitFunctionName;
extern std::string AddToTraceFunctionName;

class FunctionInserterVisitor : public RecursiveASTVisitor<FunctionInserterVisitor> {
   public:
    explicit FunctionInserterVisitor(Rewriter &R) : TheRewriter(R) {}
    bool VisitFunctionDecl(FunctionDecl *FD);

   private:
    Rewriter &TheRewriter;
};

class FunctionInserterAction : public ASTFrontendAction {
   public:
    FunctionInserterAction();

    virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &Compiler, llvm::StringRef InFile) override;
    void EndSourceFileAction() override;

   private:
    Rewriter TheRewriter;
};
}  // namespace rarepath

#endif  // FUNCTION_INSERTER_H
