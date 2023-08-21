#include <cstdint>
#include <vector>
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/DeclBase.h"
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
#include "clang/Analysis/CFG.h"


#pragma once

namespace clang {
namespace clog {

using i64 = signed long long int;
static_assert(sizeof(i64) == 8);
using u64 = unsigned long long int;
static_assert(sizeof(u64) == 8);



class ClangClog {
  template<typename T> class IdMap {
    mutable llvm::DenseMap<T, i64> TToIdMap;
    mutable std::vector<T> IdToTMap;
  public:
    i64 getId(const T& Node) const {
      auto It = TToIdMap.find(Node);
      if (It == TToIdMap.end()) {
        i64 NodeId = IdToTMap.size();
        TToIdMap[Node] = NodeId;
        IdToTMap.push_back(Node);
        return NodeId + 1; // Return a NodeId offset by 1
      }

      return It->second + 1; // Return a NodeId offset by 1
    }

    const T& getEntry(i64 Id) const {
      assert (Id > 0);
      assert (Id <= (i64) IdToTMap.size());
      return IdToTMap[Id - 1]; // External NodeId are offset by 1, to avoid having 0 as a NodeId
    }
  };

  std::pair<DynTypedNode, ASTContext*> getNodeFromId(i64 Id) const;
  i64 getIdForNode(DynTypedNode N, ASTContext *Ctx);

  class ClangClogCFG {
    using EdgeMap = llvm::DenseMap<const Stmt*, SmallVector<const Stmt*, 1>>;
    using succ_range = llvm::iterator_range<SmallVector<const Stmt*, 1>::const_iterator>;

    ASTContext *Ctx;
    std::unique_ptr<CFG> Cfg;

    void mapStmtsToSuccessors(const CFG &CFG);

    // Map AST statements to the last synthetic statement in their expansion
    llvm::DenseMap<const DeclStmt*, const DeclStmt*> SyntheticDecl;
    // Map statements (AST or synthetic) to their control-flow successors
    EdgeMap SuccStmt;
    // Set of synthetic statements that are last in their expansion
    llvm::DenseSet<const DeclStmt*> LastSyntheticDeclStmt;
    // Map Decl's to their DeclStmt
    llvm::DenseMap<const Decl*, const DeclStmt*> DeclToStmt;


  public:
    succ_range successors(const Stmt *S) const;

    const DeclStmt* lookupStmtForDecl(const Decl *D) const {
      auto It = DeclToStmt.find(D);
      if (It == DeclToStmt.end())
        return nullptr;
      return It->second;
    }

    const DeclStmt* getLastSyntheticDeclStmt(const Stmt *S) const {
      if (const auto *D = dyn_cast<DeclStmt>(S)) {
        if (D->isSingleDecl())
          return nullptr;
        auto It = SyntheticDecl.find(D);
        if (It != SyntheticDecl.end())
          return It->second;
      }
      return nullptr;
    }

    const Stmt* skipSyntheticSuccessor(const Stmt *Succ) const {
      // Synthetic stmts arise only in DeclStmts
      const auto *D = dyn_cast<DeclStmt>(Succ);
      if (!D)
        return Succ;

      // This is not a synthetic DeclStmt
      auto It = Cfg->getSyntheticDeclStmts().find(D);
      if (It == Cfg->getSyntheticDeclStmts().end())
        return Succ;

      if (LastSyntheticDeclStmt.contains(D)) {
        // This is last synthetic DeclStmt in its expansion, replace it with the
        // AST DeclStmt
        return It->getSecond();
      } else {
        // This is synthetic, but is not the last in its expansion, so it has precisely one successor
        // because it's not at the end of the block.
        const Stmt *SS = *successors(D).begin();
        return skipSyntheticSuccessor(SS);
      }
    }


    ClangClogCFG(const Stmt *S, ASTContext *Ctx) :
      Ctx(Ctx),
      Cfg(CFG::buildCFG(nullptr, const_cast<Stmt*>(S), Ctx, CFG::BuildOptions().setAllAlwaysAdd())) {
      mapStmtsToSuccessors(*Cfg);
    }
  };

  llvm::DenseMap<const Stmt*, ClangClogCFG> StmtToCFG;

  struct CollectBoundNodes : clang::ast_matchers::MatchFinder::MatchCallback {
    std::vector<clang::ast_matchers::BoundNodes> Bindings;
    llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST;
    CollectBoundNodes(llvm::DenseMap<DynTypedNode, ASTContext*> &NodeToAST) : NodeToAST(NodeToAST) {}
    void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
      for (auto Node : Result.Nodes.getMap())
        NodeToAST.insert(std::make_pair(Node.second, Result.Context));

      Bindings.push_back(Result.Nodes);
    }
  };

  const clang::tooling::CompilationDatabase &CDB;
  const std::vector<std::string> &Srcs;
  clang::tooling::ClangTool Tool;
  std::vector<std::unique_ptr<ASTUnit>> ASTs;
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts;
  DiagnosticsEngine DiagEngine;

  const SourceManager SM;

public:
  struct Loc {
    std::string Filename;
    i64 StartLine;
    i64 StartCol;
    i64 EndLine;
    i64 EndCol;
  public:
    Loc() : StartLine(0), StartCol(0), EndLine(0), EndCol(0) {}
    Loc(const std::string &Filename,
        i64 StartLine,
        i64 StartCol,
        i64 EndLine,
        i64 EndCol) : Filename(Filename),
                          StartLine(StartLine),
                          StartCol(StartCol),
                          EndLine(EndLine),
                          EndCol(EndCol) {}
  };

  ClangClog(const clang::tooling::CompilationDatabase &CDB, const std::vector<std::string> &Srcs) :
    CDB(CDB), Srcs(Srcs), Tool(CDB, Srcs),
    DiagOpts(new DiagnosticOptions()),
    DiagEngine(IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()), DiagOpts.get()),
    SM(DiagEngine, Tool.getFiles()) {}

  //  ClangClog(ClangClog &&) = default;
  //  ClangClog(const ClangClog &) = default;


  bool init();

  i64 registerMatcher(const std::string &Matcher, bool IsGlobal);
  void runGlobalMatchers();
  std::vector<std::vector<i64>> matchFromRoot(i64 MatcherId);
  std::vector<std::vector<i64>> matchFromNode(i64 MatcherId, i64 NodeId);

  // Node properties
  Loc srcLocation(i64 NodeId) const;
  i64 type(i64 NodeId);
  i64 decl(i64 NodeId);
  bool hasGlobalStorage(i64 NodeId);
  i64 parent(i64 NodeId);
  bool isParent(const i64 ParentId, const i64 NodeId);
  bool isAncestor(const i64 AncestorId, const i64 NodeId);
  std::string name(const i64 NodeId);

  // CFG
  i64 cfg(i64 NodeId);
  std::vector<i64> cfgSucc(i64 NodeId);
  std::vector<i64> cfgPred(i64 Cfg, i64 NodeId) { /* not implemented */ };

  // Debug
  std::string dump(const i64 NodeId);
  std::string kind(const i64 NodeId);

private:
  // AST node <-> u64 map
  IdMap<DynTypedNode> NodeIds;

  // Map AST nodes to their AST context; Used for running local matchers
  llvm::DenseMap<DynTypedNode, ASTContext*> NodeToAST;

  // Matchers indexed by their Id
  std::vector<clang::ast_matchers::dynamic::DynTypedMatcher> Matchers;
  std::set<u64> GlobalMatchers;
  std::map<i64, CollectBoundNodes*> MatcherIdToCollector;
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
