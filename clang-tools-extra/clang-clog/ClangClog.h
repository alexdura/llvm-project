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
    mutable llvm::DenseMap<T, int64_t> TToIdMap;
    mutable std::vector<T> IdToTMap;
  public:
    int64_t getId(const T& Node) const {
      auto It = TToIdMap.find(Node);
      if (It == TToIdMap.end()) {
        int64_t NodeId = IdToTMap.size();
        TToIdMap[Node] = NodeId;
        IdToTMap.push_back(Node);
        return NodeId + 1; // Return a NodeId offset by 1
      }

      return It->second + 1; // Return a NodeId offset by 1
    }

    const T& getEntry(int64_t Id) const {
      assert (Id > 0);
      assert (Id <= (int64_t) IdToTMap.size());
      return IdToTMap[Id - 1]; // External NodeId are offset by 1, to avoid having 0 as a NodeId
    }
  };

  struct CollectBoundNodes : ast_matchers::MatchFinder::MatchCallback {
    std::vector<ast_matchers::BoundNodes> Bindings;
    llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST;
    CollectBoundNodes(llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST) : NodeToAST(NodeToAST) {}
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

  int64_t registerMatcher(const std::string &Matcher, bool IsGlobal);
  void runGlobalMatchers();
  std::vector<std::vector<int64_t>> matchFromRoot(int64_t MatcherId);
  std::vector<std::vector<int64_t>> matchFromNode(int64_t MatcherId, int64_t NodeId);
  std::tuple<std::string, int64_t, int64_t, int64_t, int64_t> srcLocation(int64_t NodeId) const;
  std::vector<int64_t> parent(const int64_t NodeId) const;

private:
  // AST node <-> uint64_t map
  IdMap<DynTypedNode> NodeIds;

  // Map AST nodes to their AST context; Used for running local matchers
  llvm::DenseMap<DynTypedNode, ASTContext*> NodeToAST;

  // Matchers indexed by their Id
  std::vector<ast_matchers::dynamic::DynTypedMatcher> Matchers;
  std::set<uint64_t> GlobalMatchers;
  std::map<int64_t, CollectBoundNodes*> MatcherIdToCollector;
  std::vector<CollectBoundNodes> GlobalCollectors;


};
} // namespace clog
} // namespace clang
