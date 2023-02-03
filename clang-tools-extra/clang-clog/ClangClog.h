#include <vector>
#include "clang/AST/ASTTypeTraits.h"
#include "clang/Basic/PlistSupport.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/Diagnostics.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "llvm/Support/ErrorOr.h"


#pragma once
namespace clang {
namespace clog {
class ClangClog {
  template<typename T> class IdMap {
    mutable llvm::DenseMap<T, uint64_t> TToIdMap;
    mutable std::vector<T> IdToTMap;
  public:
    uint64_t getId(const T& Node) const {
      auto It = TToIdMap.find(Node);
      if (It == TToIdMap.end()) {
        uint64_t NodeId = IdToTMap.size();
        TToIdMap[Node] = NodeId;
        IdToTMap.push_back(Node);
        return NodeId + 1; // Return a NodeId offset by 1
      }

      return It->second + 1; // Return a NodeId offset by 1
    }

    const T& getEntry(uint64_t Id) const {
      assert (Id > 0);
      assert (Id <= IdToTMap.size());
      return IdToTMap[Id - 1]; // External NodeId are offset by 1, to avoid having 0 as a NodeId
    }
  };

  struct CollectBoundNodes : ast_matchers::MatchFinder::MatchCallback {
    std::vector<ast_matchers::BoundNodes> &Bindings;
    CollectBoundNodes(std::vector<ast_matchers::BoundNodes> &Bindings) : Bindings(Bindings) {}
    void run(const ast_matchers::MatchFinder::MatchResult &Result) override {
      Bindings.push_back(Result.Nodes);
    }
  };

private:
  const tooling::CompilationDatabase &CDB;
  const std::vector<std::string> &Srcs;
  tooling::ClangTool Tool;
  std::vector<std::unique_ptr<ASTUnit>> ASTs;

public:
  ClangClog(tooling::CompilationDatabase &CDB, const std::vector<std::string> &Srcs) :
    CDB(CDB), Srcs(Srcs), Tool(CDB, Srcs) {}

  bool init();
  bool match(const std::string &Pattern, std::vector<ast_matchers::BoundNodes> &Result) const;

  uint64_t registerMatcher(const std::string &Matcher, bool IsGlobal);
  void runGlobalMatchers();
  std::vector<std::vector<uint64_t>> matchFromRoot(uint64_t MatcherId);
  std::vector<std::vector<uint64_t>> matchFromNode(uint64_t MatcherId, uint64_t NodeId) const;
  std::tuple<std::string, uint64_t, uint64_t, uint64_t, uint64_t> srcLocation(uint64_t NodeId) const;
  std::vector<uint64_t> parent(const uint64_t NodeId) const;

private:
  IdMap<DynTypedNode> NodeIds;
  // TODO: this won't compile. Use Matcher ID instead
  IdMap<ast_matchers::dynamic::DynTypedMatcher> MatcherIds;

  std::vector<uint64_t> GlobalMatchers;
  std::vector<CollectBoundNodes> GlobalCollectors;
  std::vector<std::vector<ast_matchers::BoundNodes>> GlobalMatches;
  ast_matchers::MatchFinder GlobalFinder;
};
} // namespace clog
} // namespace clang
