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

int64_t ClangClog::registerMatcher(const std::string &Pattern, bool IsGlobal) {
  StringRef PatRef = Pattern;
  Diagnostics Diag;
  auto Matcher = Parser::parseMatcherExpression(PatRef, &Diag);

  if (!Matcher)
    return -1;

  int64_t MatcherId = Matchers.size();

  Matchers.push_back(*Matcher);

  if (IsGlobal) {
    GlobalMatchers.insert(MatcherId);
  }

  return MatcherId;
}

void ClangClog::runGlobalMatchers() {
  MatchFinder GlobalFinder;

  GlobalCollectors.reserve(GlobalMatchers.size());

  for (int64_t MatchId : GlobalMatchers) {
    GlobalCollectors.emplace_back(NodeToAST);
    MatcherIdToCollector[MatchId] = &GlobalCollectors.back();
    GlobalFinder.addDynamicMatcher(Matchers[MatchId], &GlobalCollectors.back());
  }

  for (auto &AST : ASTs) {
    auto &Ctx = AST->getASTContext();
    Ctx.getParentMapContext().setTraversalKind(TK_IgnoreUnlessSpelledInSource);
    GlobalFinder.matchAST(Ctx);
  }
}

std::vector<std::vector<int64_t>> ClangClog::matchFromRoot(int64_t MatcherId) {
  auto It = MatcherIdToCollector.find(MatcherId);
  if (It == MatcherIdToCollector.end())
    llvm_unreachable("Expecting a global matcher id.");

  std::vector<std::vector<int64_t>> Result;
  for (auto BN : It->second->Bindings) {
    std::vector<int64_t> Row;
    for (auto B : BN.getMap()) {
      int64_t NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }
    Result.push_back(std::move(Row));
  }

  return Result;
}

std::vector<std::vector<int64_t>> ClangClog::matchFromNode(int64_t MatcherId, int64_t NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end())
    llvm_unreachable("Could not find ASTContext for node.");

  auto *Context = It->second;
  ast_matchers::MatchFinder Finder;

  CollectBoundNodes Collector(NodeToAST);

  Finder.addDynamicMatcher(Matchers[MatcherId], &Collector);

  Finder.match(Node, *Context);

  std::vector<std::vector<int64_t>> Result;
  for (auto BN : Collector.Bindings) {
    std::vector<int64_t> Row;
    for (auto B : BN.getMap()) {
      int64_t NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }
    Result.push_back(std::move(Row));
  }
  return Result;
}

std::tuple<std::string, int64_t, int64_t, int64_t, int64_t> ClangClog::srcLocation(int64_t NodeId) const {
  auto Node = NodeIds.getEntry(NodeId);
  auto SR = Node.getSourceRange();
  auto ASTIt = NodeToAST.find(Node);
  if (ASTIt == NodeToAST.end())
    llvm_unreachable("Could not find ASTContext for node.");

  const FullSourceLoc &SrcLocBegin = ASTIt->second->getFullLoc(SR.getBegin());
  const FullSourceLoc &SrcLocEnd = ASTIt->second->getFullLoc(SR.getEnd());

  return std::make_tuple(SrcLocBegin.getFileEntry()->getName().str(),
                         SrcLocBegin.getLineNumber(),
                         SrcLocBegin.getColumnNumber(),
                         SrcLocEnd.getLineNumber(),
                         SrcLocEnd.getColumnNumber());

}
} // namespace clog
} // namespace clang
