#include "RarePathCoverageCommon.h"

#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/CommandLine.h"

// using namespace clang;
using namespace llvm;

cl::OptionCategory MyToolCategory("function-printer options");
cl::opt<std::string> OutputFilename("output", cl::desc("Specify the output file"), cl::value_desc("filename"),
                                    cl::init("output.c"),  // Set default value
                                    cl::cat(MyToolCategory));

cl::alias OutputFilenameShort("o", cl::desc("alias for --output"), cl::aliasopt(OutputFilename),
                              cl::cat(MyToolCategory));

std::string newPlaceholder() { return "\nPlaceHolder();"; }

std::string stmtToString(Stmt *stmt) {
    clang::LangOptions lo;
    std::string out_str;
    llvm::raw_string_ostream outstream(out_str);
    stmt->printPretty(outstream, NULL, PrintingPolicy(lo));
    return out_str;
}

std::string exprToString(Expr *stmt) {
    clang::LangOptions lo;
    std::string out_str;
    llvm::raw_string_ostream outstream(out_str);
    stmt->printPretty(outstream, NULL, PrintingPolicy(lo));
    return out_str;
}