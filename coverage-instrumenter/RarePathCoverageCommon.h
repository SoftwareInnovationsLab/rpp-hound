#ifndef _COVERAGE_H
#define _COVERAGE_H

#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace clang;

extern cl::OptionCategory MyToolCategory;
extern cl::opt<std::string> OutputFilename;
extern cl::alias OutputFilenameShort;

std::string newPlaceholder(void);
std::string stmtToString(Stmt *stmt);
std::string exprToString(Expr *stmt);

#endif  // _COVERAGE_H