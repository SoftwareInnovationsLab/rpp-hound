#include "FunctionInserter.h"

#include "RarePathCoverageCommon.h"
#include "llvm/Support/CommandLine.h"

namespace rarepath {

std::string InitFunctionName = "rpcov_trace_init";
std::string AddToTraceFunctionName = "rpcov_trace_add";
std::string CaptureFaultFunctionName = "rpcov_check_fault";

// FunctionInserter

class FunctionInserter : public ASTConsumer {
   public:
    explicit FunctionInserter(Rewriter &TheRewriter) : TheRewriter(TheRewriter) {}

    void HandleTranslationUnit(ASTContext &Context) override {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
        SourceManager &SM = TheRewriter.getSourceMgr();
        SourceLocation StartOfFile = SM.getLocForStartOfFile(SM.getMainFileID());
        SourceLocation EndOfFile = SM.getLocForEndOfFile(SM.getMainFileID());

        // Create the new function
        std::string NewFunctionDecls =
            "/* BEGIN Rare Path Coverage Instrumentation */\n"
            "#include <stdlib.h>\n#include<stdio.h>\n#include<time.h>\n"
            "void PlaceHolder(void);\n"
            "void " +
            InitFunctionName +
            "();\n"
            "void " +
            AddToTraceFunctionName +
            "(char *file, int line, char *tf);\n" +
            "void " +
            CaptureFaultFunctionName +
            "();\n"
            "FILE *gTraceFile = NULL;\n"
            "int actual = 0;\n"
            "int expected = 0;\n\n";
            "/* END Rare Path Coverage Instrumentation */\n\n";
        std::string NewFunctionDefs =
            "\n/* BEGIN Rare Path Coverage Instrumentation */\n"
            "\nvoid " +
            AddToTraceFunctionName +
            "(char *file, int line, char *tf) {\n"
            "   fprintf(gTraceFile,\"%s,%d,%s\\n\", file, line, tf);\nfflush(gTraceFile);\n"
            "}\n"
            "\nvoid " +
            InitFunctionName +
            "() {\n"
            "   time_t now_seconds;\n"
            "   time(&now_seconds);\n"
            "   struct timespec now_high_res;\n"
            "   clock_gettime(CLOCK_MONOTONIC, &now_high_res);\n"
            "   char ns_str[20];\n"
            "   sprintf(ns_str, \"%ld\", now_high_res.tv_nsec);\n"
            "   char *filename = malloc(sizeof(char) * (30 + sizeof(ns_str)));\n"
            "   sprintf(filename, \"trace_%ld_%s.txt\", now_seconds, ns_str);\n"
            "   gTraceFile = fopen(filename, \"w\");"
            "}\n"
            "\nvoid " +
            CaptureFaultFunctionName +
            "() {\n"
            "   if (actual != expected) {\n"
            "       fprintf(gTraceFile,\"FAULT\\n\");\n"
            "       fflush(gTraceFile);\n"
            "   }\n"
            "}\n"
            "/* END Rare Path Coverage Instrumentation */\n\n";

        TheRewriter.InsertTextAfter(StartOfFile, NewFunctionDecls);
        TheRewriter.InsertTextAfter(EndOfFile, NewFunctionDefs);
    }

   private:
    Rewriter &TheRewriter;
    FunctionInserterVisitor Visitor{TheRewriter};
};

// FunctionInserterAction

FunctionInserterAction::FunctionInserterAction() {}

void FunctionInserterAction::EndSourceFileAction() {
    std::error_code EC;
    llvm::raw_fd_ostream outFile(OutputFilename, EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Error opening output file: " << EC.message() << "\n";
        return;
    }

    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(outFile);
    outFile.close();
}

std::unique_ptr<ASTConsumer> FunctionInserterAction::CreateASTConsumer(CompilerInstance &CI, StringRef file) {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<FunctionInserter>(TheRewriter);
}

bool FunctionInserterVisitor::VisitFunctionDecl(FunctionDecl *FD) {
    if (FD->isMain()) {
        Stmt *Body = FD->getBody();
        if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Body)) {
            TheRewriter.InsertText(CS->getLBracLoc().getLocWithOffset(1), InitFunctionName + "();\n");
        }
    }
    return true;
}

}  // namespace rarepath
