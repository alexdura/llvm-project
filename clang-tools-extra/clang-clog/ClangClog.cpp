#include "ClangClog.h"
#include <string>
#include <iterator>
#include <vector>
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchersInternal.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/Diagnostics.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/ADT/DenseMap.h"

using namespace clang;
using namespace llvm;
using namespace clang::tooling;
using namespace clang::ast_matchers;
using namespace clang::ast_matchers::dynamic;
using namespace clang::ast_matchers::internal;


namespace clang {
namespace clog {
bool ClangClog::init() {
  llvm::errs() << "sizeof(DynTypedNode) = " << sizeof(DynTypedNode) << "\n";

  switch (Tool.buildASTs(ASTs)) {
  case 2:
    llvm::errs() << "Failed to build the ASTs for some of the files.";
    LLVM_FALLTHROUGH;
  case 0:
    return true;
  case 1:
    return false;
  default:
    llvm_unreachable("Unknown return code");
  }
}

uint64_t ClangClog::registerMatcher(const std::string &Pattern, bool IsGlobal) {
  StringRef PatRef = Pattern;
  Diagnostics Diag;
  auto Matcher = Parser::parseMatcherExpression(PatRef, &Diag);

  if (!Matcher)
    return 0;

  auto Ret = MatcherIds.getId(*Matcher);

  if (IsGlobal) {
    GlobalMatchers.push_back(Ret);
    GlobalMatches.emplace_back();
    GlobalCollectors.emplace_back(GlobalMatches.back());
    GlobalFinder.addDynamicMatcher(*Matcher, &GlobalCollectors.back());
  }

  return Ret;
}

void ClangClog::runGlobalMatchers() {
  for (auto &AST : ASTs) {
    auto &Ctx = AST->getASTContext();
    Ctx.getParentMapContext().setTraversalKind(TK_IgnoreUnlessSpelledInSource);
    GlobalFinder.matchAST(Ctx);
  }
}

std::vector<std::vector<uint64_t>> ClangClog::matchFromRoot(uint64_t MatcherId) {
  auto It = std::find(GlobalMatchers.begin(), GlobalMatchers.end(), MatcherId);
  if (It == GlobalMatchers.end())
    return std::vector<std::vector<uint64_t>>();

  auto Index = It - GlobalMatchers.begin();


  std::vector<std::vector<uint64_t>> Result;
  for (auto &M : GlobalMatches[Index]) {

    std::vector<uint64_t> Row;
    for (auto &B : M.getMap()) {
      // this relies on the fact that M.getMap() is a sorted map
      auto NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }

    Result.push_back(std::move(Row));
  }

  return Result;
}

} // namespace clog
} // namespace clang
