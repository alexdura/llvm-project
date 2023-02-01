#include "ClangClog.h"
#include <vector>
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/Diagnostics.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "llvm/Support/ErrorOr.h"

using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;
using namespace clang::ast_matchers::dynamic;

namespace {
struct CollectBoundNodes : MatchFinder::MatchCallback {
  std::vector<BoundNodes> &Bindings;
  CollectBoundNodes(std::vector<BoundNodes> &Bindings) : Bindings(Bindings) {}
  void run(const MatchFinder::MatchResult &Result) override {
    Bindings.push_back(Result.Nodes);
  }
};
} // namespace

namespace clang {
namespace clog {
bool ClangClog::init() {
  switch (Tool.buildASTs(ASTs)) {
  case 1:
    llvm::errs() << "Failed to build the ASTs for some of the files.";
    LLVM_FALLTHROUGH;
  case 0:
    return true;
  default:
    llvm_unreachable("Unknown return code");
  }
}

bool ClangClog::match(const std::string &Pattern, std::vector<BoundNodes> &Matches) const {
  Diagnostics Diag;
  StringRef PatRef = Pattern;

  auto Matcher = Parser::parseMatcherExpression(PatRef, &Diag);
  if (!Matcher) {
    return false;
  }

  MatchFinder Finder;
  CollectBoundNodes Collect(Matches);

  if (!Finder.addDynamicMatcher(*Matcher, &Collect)) {
    llvm::errs() << "Not a valid top-level matcher.\n";
    return false;
  }

  for (auto &AST : this->ASTs) {
    auto &Ctx = AST->getASTContext();
    // const auto &SM = Ctx.getSourceManager();
    Ctx.getParentMapContext().setTraversalKind(TK_IgnoreUnlessSpelledInSource);
    Finder.matchAST(Ctx);
  }

  for (auto &M : Matches) {
    for (auto &B : M.getMap()) {
      llvm::outs() << B.first << " : "  << reinterpret_cast<uint64_t>(B.second.getMemoizationData()) << "\n";
    }
  }

  return true;
}
} // namespace clog
} // namespace clang
