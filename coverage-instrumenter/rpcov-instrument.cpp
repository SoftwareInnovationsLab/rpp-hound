#include "FunctionInserter.h"
#include "IfStmtInstrumenter.h"
#include "RarePathCoverageCommon.h"
#include "ReplacePlaceholders.h"
#include "ScopeUpdater.h"

int PhaseNum = 1;
std::vector<std::string> Files;

template <typename T>
int runTool(CompilationDatabase &CD) {
    ClangTool Tool(CD, Files);
    Tool.run(newFrontendActionFactory<T>().get());

    // Prepare for the next phase
    Files.clear();
    Files.push_back(OutputFilename);
    return 0;
}

int runClangTidy(std::string File) {
    std::string ClangFormat = "clang-format --style='{ BasedOnStyle: Google, ColumnLimit: 120}' -i " + File;
    return std::system(ClangFormat.c_str());
}

int main(int argc, const char **argv) {
    auto OptionsParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
    CompilationDatabase &CD = OptionsParser->getCompilations();
    Files = OptionsParser->getSourcePathList();
    std::string FinalOutname = OutputFilename;

    std::string TmpPrefix = "rpcov-tmp-";

    OutputFilename = TmpPrefix + std::to_string(PhaseNum++) + "-" + FinalOutname;
    runTool<rarepath::ScopeFrontendAction>(CD);
    runClangTidy(OutputFilename);

    OutputFilename = TmpPrefix + std::to_string(PhaseNum++) + "-" + FinalOutname;
    runTool<rarepath::ScopeFrontendAction>(CD);

    OutputFilename = TmpPrefix + std::to_string(PhaseNum++) + "-" + FinalOutname;
    runTool<rarepath::FunctionInserterAction>(CD);

    OutputFilename = TmpPrefix + std::to_string(PhaseNum++) + "-" + FinalOutname;
    runTool<rarepath::BranchStatementPrinterFrontendAction>(CD);
    runClangTidy(OutputFilename);

    OutputFilename = FinalOutname;
    runTool<rarepath::UpdateLineNumsFrontendAction>(CD);
    runClangTidy(OutputFilename);

    std::string CleanupCmd = "rm " + TmpPrefix + "*-" + OutputFilename;
    return std::system(CleanupCmd.c_str());
}
