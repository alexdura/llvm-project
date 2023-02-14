#include "ClangClog.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include <iostream>
#include <sstream>
#include <vector>
#include <ios>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;


int main(int argc, const char **argv) {
  llvm::outs() << "Hello from clang-clog\n";

  if (argc < 2)
    llvm::errs() << "Expecting exactly one argument.";


  clog::ClangClogBuilder builder{{std::string(argv[1])}};

  clog::ClangClog *clog = builder.build();

  if (clog == nullptr)
    return -1;

  clog->init();

  int DeclMatcher = clog->registerMatcher("decl().bind(\"d\")", true);

  clog->runGlobalMatchers();

  const auto &Result = clog->matchFromRoot(DeclMatcher);

  for (const auto &Row : Result) {
    for (auto Elem : Row)
      llvm::dbgs() << Elem << " ";
    llvm::dbgs() << "\n";
  }

  return 0;
}
