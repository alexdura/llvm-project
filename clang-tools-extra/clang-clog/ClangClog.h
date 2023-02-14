#include <cstdint>
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
#include "clang/Tooling/CommonOptionsParser.h"

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

  struct CollectBoundNodes : clang::ast_matchers::MatchFinder::MatchCallback {
    std::vector<clang::ast_matchers::BoundNodes> Bindings;
    llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST;
    CollectBoundNodes(llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST) : NodeToAST(NodeToAST) {}
    void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
      Bindings.push_back(Result.Nodes);
    }
  };

private:
  const clang::tooling::CompilationDatabase &CDB;
  const std::vector<std::string> &Srcs;
  clang::tooling::ClangTool Tool;
  std::vector<std::unique_ptr<ASTUnit>> ASTs;

public:
  struct Loc {
    std::string Filename;
    int64_t StartLine;
    int64_t StartCol;
    int64_t EndLine;
    int64_t EndCol;
  public:
    Loc() : StartLine(0), StartCol(0), EndLine(0), EndCol(0) {}
    Loc(const std::string &Filename,
        int64_t StartLine,
        int64_t StartCol,
        int64_t EndLine,
        int64_t EndCol) : Filename(Filename),
                          StartLine(StartLine),
                          StartCol(StartCol),
                          EndLine(EndLine),
                          EndCol(EndCol) {}
  };

  ClangClog(const clang::tooling::CompilationDatabase &CDB, const std::vector<std::string> &Srcs) :
    CDB(CDB), Srcs(Srcs), Tool(CDB, Srcs) {}

  //  ClangClog(ClangClog &&) = default;
  //  ClangClog(const ClangClog &) = default;


  bool init();

  int64_t registerMatcher(const std::string &Matcher, bool IsGlobal);
  void runGlobalMatchers();
  std::vector<std::vector<int64_t>> matchFromRoot(int64_t MatcherId);
  std::vector<std::vector<int64_t>> matchFromNode(int64_t MatcherId, int64_t NodeId);
  Loc srcLocation(int64_t NodeId) const;
  std::vector<int64_t> parent(const int64_t NodeId) const { llvm_unreachable("Unimplemented"); }

private:
  // AST node <-> uint64_t map
  IdMap<DynTypedNode> NodeIds;

  // Map AST nodes to their AST context; Used for running local matchers
  llvm::DenseMap<DynTypedNode, ASTContext*> NodeToAST;

  // Matchers indexed by their Id
  std::vector<clang::ast_matchers::dynamic::DynTypedMatcher> Matchers;
  std::set<uint64_t> GlobalMatchers;
  std::map<int64_t, CollectBoundNodes*> MatcherIdToCollector;
  std::vector<CollectBoundNodes> GlobalCollectors;
};

class ClangClogBuilder {
  // The whole purpose of this class is to hold the ownership of argc and argv
  int Argc;
  const char **Argv;
  ClangClog *Instance = nullptr;
  tooling::CommonOptionsParser OptionsParser;

  static const char **buildArgv(const std::vector<std::string> &Args);
  static tooling::CommonOptionsParser buildOptionsParser(int Argc, const char **Argv);

public:
  ClangClogBuilder(const std::vector<std::string> &Args);
  ~ClangClogBuilder();
  ClangClog* build();
};


} // namespace clog
} // namespace clang
