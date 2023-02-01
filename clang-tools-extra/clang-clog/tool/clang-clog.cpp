#include "ClangClog.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include <iostream>
#include <sstream>
#include <ios>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;


static cl::OptionCategory ClangClogCategory("clang-clog options");

int main(int argc, const char **argv) {
  llvm::outs() << "Hello from clang-clog\n";

  llvm::Expected<CommonOptionsParser> OptionsParser =
      CommonOptionsParser::create(argc, argv, ClangClogCategory,
                                  llvm::cl::OneOrMore);

  if (!OptionsParser) {
    llvm::errs() << llvm::toString(OptionsParser.takeError());
    return 1;
  }

  clog::ClangClog clog(OptionsParser->getCompilations(),
                       OptionsParser->getSourcePathList());

  if (!clog.init()) {
    return 2;
  }

  enum class Action {
    READ_COMMAND,
    PATTERN,
    CHILD,
    EXIT
  };

  Action NextAction = Action::READ_COMMAND;
  std::vector<BoundNodes> Matches;

  for (std::string Line; std::getline(std::cin, Line); ) {
    switch (NextAction) {
    case Action::READ_COMMAND:
      if (Line == "M") {
        NextAction = Action::PATTERN;
      } else if (Line == "C") {
        NextAction = Action::CHILD;
      } else {
        return 0;
      }
      break;

    case Action::PATTERN:
      NextAction = Action::READ_COMMAND;
      Matches.clear();
      if (!clog.match(Line, Matches)) {
        llvm::errs() << "Matching failed!";
      }
      llvm::outs() << "Found " << Matches.size() << " matches.\n";
      break;

    case Action::CHILD:
      NextAction = Action::READ_COMMAND;
      {
        std::stringstream Str(Line);
        uint64_t NodeId = 0;
        int ChildIdx;

        Str >> std::hex >> NodeId;
        Str >> ChildIdx;

        llvm::outs() << NodeId << "[" << ChildIdx << "] = " << 0 << "\n";
      }
      break;

    case Action::EXIT:
      return 0;
    }

  }


  return 0;

}
