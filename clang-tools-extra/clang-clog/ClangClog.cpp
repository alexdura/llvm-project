#include "ClangClog.h"
#include <string>
#include <iterator>
#include <vector>
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchersInternal.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/Diagnostics.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/ADT/DenseMap.h"
#include "clang/Analysis/CFG.h"
#include "clang/Analysis/CFGStmtMap.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace llvm;
using namespace clang::tooling;
using namespace clang::ast_matchers;
using namespace clang::ast_matchers::dynamic;
using namespace clang::ast_matchers::internal;


namespace clang {
namespace clog {


static bool topLevelVisitor(void *Context, const Decl *D) {
  auto *ST = reinterpret_cast<StringMap<const Decl *>*>(Context);
  if (const auto *F = dyn_cast<FunctionDecl>(D)) {
    if (F->isExternallyVisible() && F->hasBody()) {
      ST->insert(std::make_pair(F->getName(), D));
    }
  }
  return true;
}

bool ClangClog::init() {
  switch (Tool.buildASTs(ASTs)) {
  case 2:
    llvm::errs() << "Failed to build the ASTs for some of the files.";
    LLVM_FALLTHROUGH;
  case 0:
    for (auto &AST : ASTs) {
      AST->visitLocalTopLevelDecls(&CrossTUSymbolTable, topLevelVisitor);
    }
    return true;
  case 1:
    return false;
  default:
    llvm_unreachable("Unknown return code");
  }
}

i64 ClangClog::registerMatcher(const std::string &Pattern, bool IsGlobal) {
  StringRef PatRef = Pattern;
  Diagnostics Diag;
  auto Matcher = Parser::parseMatcherExpression(PatRef, &Diag);


  if (!Matcher) {
    Diag.printToStreamFull(llvm::errs());
    return -1;
  }

  // Ignore all implicit nodes
  Matcher = Matcher->withTraversalKind(TK_IgnoreUnlessSpelledInSource);

  i64 MatcherId = Matchers.size();

  Matchers.push_back(*Matcher);

  if (IsGlobal) {
    GlobalMatchers.insert(MatcherId);
  }

  return MatcherId;
}

void ClangClog::runGlobalMatchers() {
  MatchFinder GlobalFinder;

  GlobalCollectors.reserve(GlobalMatchers.size());

  for (i64 MatchId : GlobalMatchers) {
    GlobalCollectors.emplace_back(NodeToAST);
    MatcherIdToCollector[MatchId] = &GlobalCollectors.back();
    GlobalFinder.addDynamicMatcher(Matchers[MatchId], &GlobalCollectors.back());
  }

  for (auto &AST : ASTs) {
    auto &Ctx = AST->getASTContext();
    TraversalKindScope TS(Ctx, TK_IgnoreUnlessSpelledInSource);
    GlobalFinder.matchAST(Ctx);
  }
}

std::vector<std::vector<i64>> ClangClog::matchFromRoot(i64 MatcherId) {
  auto It = MatcherIdToCollector.find(MatcherId);
  if (It == MatcherIdToCollector.end()) {
    llvm::errs() << "Expecting a global matcher id, but got " << MatcherId << "\n";
    llvm_unreachable("Expecting a global matcher id.");

  }

  std::vector<std::vector<i64>> Result;
  for (auto BN : It->second->Bindings) {
    std::vector<i64> Row;
    for (auto B : BN.getMap()) {
      i64 NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }
    Result.push_back(std::move(Row));
  }

  return Result;
}


std::pair<DynTypedNode, ASTContext*> ClangClog::getNodeFromId(i64 NodeId) const {
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << " .";
    llvm_unreachable("Ooops!");
  }
  return std::make_pair(Node, It->second);
}

i64 ClangClog::getIdForNode(DynTypedNode N, ASTContext *Ctx) {
  NodeToAST.insert(std::make_pair(N, Ctx));
  return NodeIds.getId(N);
}


/// Copied from RewriteRule.cpp
template <typename T>
class DynamicForEachDescendantMatcher
    : public ast_matchers::internal::MatcherInterface<T> {
  const DynTypedMatcher DescendantMatcher;

public:
  explicit DynamicForEachDescendantMatcher(DynTypedMatcher DescendantMatcher)
      : DescendantMatcher(std::move(DescendantMatcher)) {}

  bool matches(
      const T &Node, ast_matchers::internal::ASTMatchFinder *Finder,
      ast_matchers::internal::BoundNodesTreeBuilder *Builder) const override {
    return Finder->matchesDescendantOf(
        Node, this->DescendantMatcher, Builder,
        ast_matchers::internal::ASTMatchFinder::BK_All);
  }
};


std::vector<std::vector<i64>> ClangClog::matchFromNode(i64 MatcherId, i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << " .";
    llvm_unreachable("Ooops!");
  }

  auto NodeKind = Node.getNodeKind();
  DynTypedMatcher DescendantMatcher = Matchers[MatcherId];
  if (DescendantMatcher.canMatchNodesOfKind(NodeKind)) {
    auto CladeKind = NodeKind.getCladeKind();
    if (CladeKind.isSame(ASTNodeKind::getFromNodeKind<Stmt>())) {
      DescendantMatcher = ast_matchers::internal::makeMatcher(new DynamicForEachDescendantMatcher<Stmt>(DescendantMatcher));
    } else if (CladeKind.isSame(ASTNodeKind::getFromNodeKind<Decl>())) {
      DescendantMatcher = ast_matchers::internal::makeMatcher(new DynamicForEachDescendantMatcher<Decl>(DescendantMatcher));
    } else {
      llvm_unreachable((std::string("Unknown clade kind ") + CladeKind.asStringRef().str()).c_str());
    }
  } else {
    return {};
  }

  ast_matchers::MatchFinder Finder;
  CollectBoundNodes Collector(NodeToAST);
  Finder.addDynamicMatcher(DescendantMatcher, &Collector);

  auto *Context = It->second;
  TraversalKindScope TS(*Context, TK_IgnoreUnlessSpelledInSource);
  Finder.match(Node, *Context);

  std::vector<std::vector<i64>> Result;
  for (auto BN : Collector.Bindings) {
    std::vector<i64> Row;
    for (auto B : BN.getMap()) {
      i64 NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }
    Result.push_back(std::move(Row));
  }
  return Result;
}

std::vector<std::vector<i64>> ClangClog::matchAtNode(i64 MatcherId, i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << " .";
    llvm_unreachable("Ooops!");
  }

  auto *Context = It->second;
  ast_matchers::MatchFinder Finder;

  CollectBoundNodes Collector(NodeToAST);
  Finder.addDynamicMatcher(Matchers[MatcherId], &Collector);

  TraversalKindScope TS(*Context, TK_IgnoreUnlessSpelledInSource);
  Finder.match(Node, *Context);

  std::vector<std::vector<i64>> Result;
  for (auto BN : Collector.Bindings) {
    std::vector<i64> Row;
    for (auto B : BN.getMap()) {
      i64 NodeId = NodeIds.getId(B.second);
      Row.push_back(NodeId);
    }
    Result.push_back(std::move(Row));
  }
  return Result;
}

ClangClog::Loc ClangClog::srcLocation(i64 NodeId) const {
  DynTypedNode Node;
  ASTContext *Ctx;

  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  auto SR = Node.getSourceRange();

  const auto &SM = Ctx->getSourceManager();

  const auto &Presumed = SM.getPresumedLoc(SM.getSpellingLoc(SR.getBegin()));
  if (Presumed.isValid())
    return {
      Presumed.getFilename(),
      SM.getSpellingLineNumber(SR.getBegin()),
      SM.getSpellingColumnNumber(SR.getBegin()),
      SM.getSpellingLineNumber(SR.getEnd()),
      SM.getSpellingColumnNumber(SR.getEnd())
    };

  return {"", 0, 0, 0, 0};
}

ClangClog::Loc ClangClog::srcExpansionLocation(i64 NodeId) const {
  DynTypedNode Node;
  ASTContext *Ctx;

  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  auto SR = Node.getSourceRange();

  const auto &SM = Ctx->getSourceManager();

  const auto &Presumed = SM.getPresumedLoc(SM.getExpansionLoc(SR.getBegin()));
  if (Presumed.isValid())
    return {
      Presumed.getFilename(),
      SM.getExpansionLineNumber(SR.getBegin()),
      SM.getExpansionColumnNumber(SR.getBegin()),
      SM.getExpansionLineNumber(SR.getEnd()),
      SM.getExpansionColumnNumber(SR.getEnd())
    };

  return {"", 0, 0, 0, 0};
}



i64 ClangClog::type(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  if (const auto *E = Node.get<Expr>()) {
    const auto &T = E->getType();
    auto DynNode = DynTypedNode::create(T);
    return getIdForNode(DynNode, Ctx);
  }

  return 0;
}

i64 ClangClog::decl(i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);

  if (const auto *DeclRef = Node.get<DeclRefExpr>()) {
    const NamedDecl *D = DeclRef->getFoundDecl();
    if (D) {
      // Prefer function definitions to function declarations
      if (const auto *FunDecl = dyn_cast<FunctionDecl>(D)) {
        const FunctionDecl *Def = nullptr;
        if (FunDecl->isDefined(Def)) {
          D = Def;
        } else {
          auto It = CrossTUSymbolTable.find(D->getName());
          if (It != CrossTUSymbolTable.end()) {
            D = It->second;
          }
        }
      }

      auto DynNode = DynTypedNode::create(*D);
      NodeToAST.insert(std::make_pair(DynNode, &D->getASTContext()));
      return NodeIds.getId(DynTypedNode::create(*D));
    }
  }
  return 0;
}

bool ClangClog::isParent(const i64 ParentId, const i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  auto ParentNode = NodeIds.getEntry(ParentId);

  TraversalKindScope TS(*Ctx, TK_IgnoreUnlessSpelledInSource);

  for (const auto &ParentCandidate : Ctx->getParents(Node)) {
    if (ParentNode == ParentCandidate)
      return true;
  }
  return false;
}

static bool isAncestorHelper(DynTypedNode Ancestor, DynTypedNode Node, ASTContext &Ctx) {
  for (const auto &Parent : Ctx.getParents(Node)) {
    if (Ancestor == Parent)
      return true;
    if (isAncestorHelper(Ancestor, Parent, Ctx))
      return true;
  }
  return false;
}

bool ClangClog::isAncestor(const i64 AncestorId, const i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  TraversalKindScope TS(*Ctx, TK_IgnoreUnlessSpelledInSource);

  auto AncestorNode = NodeIds.getEntry(AncestorId);

  return isAncestorHelper(AncestorNode, Node, *Ctx);
}

std::string ClangClog::name(const i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  if (const auto *ND = Node.get<NamedDecl>()) {
    return ND->getNameAsString();
  } else if (const auto *R = Node.get<DeclRefExpr>()) {
    return R->getDecl()->getNameAsString();
  }
  return "";
}

std::string ClangClog::kind(const i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  return Node.getNodeKind().asStringRef().str();
}

i64 ClangClog::parent(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  TraversalKindScope TS(*Ctx, TK_IgnoreUnlessSpelledInSource);
  auto Parents = Ctx->getParents(Node);

  if (Parents.empty()) {
    return 0;
  } else {
    const auto *PIt = Parents.begin();
    auto ParentNode = *PIt;

    if (std::next(PIt) != Parents.end()) {
      llvm::errs() << "Expecting that nodeId=" << NodeId << " has at most one parent. This should hold for C (no templates)";
      llvm_unreachable("Ooops!");
    }

    return getIdForNode(ParentNode, Ctx);
  }
}

i64 ClangClog::index(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  TraversalKindScope TS(*Ctx, TK_IgnoreUnlessSpelledInSource);
  auto Parents = Ctx->getParents(Node);

  if (Parents.empty())
    return -1;

  const auto PIt = Parents.begin();
  if (std::next(PIt) != Parents.end())
    return -1; // Expecting at most one parent.

  if (const auto *C = PIt->get<CallExpr>()) {
    if (const auto *Arg = Node.get<Expr>()) {
      for (unsigned i = 0; i < C->getNumArgs(); ++i) {
        if (C->getArg(i)->IgnoreUnlessSpelledInSource() == Arg)
          return i;
      }
    }
  } else if (const auto *P = Node.get<ParmVarDecl>()) {
    return P->getParameterIndex();
  }

  return -1;
}

template <typename NodeT>
static const FunctionDecl* getParentFunction(const NodeT *N, ASTContext &Ctx) {
  for (const auto &P : Ctx.getParents(*N)) {
    if (const auto *FuncDecl = P.template get<FunctionDecl>()) {
      return FuncDecl;
    } else if (const auto *ParentS = P.template get<Stmt>()) {
      return getParentFunction(ParentS, Ctx);
    } else if (const auto *ParentD = P.template get<Decl>()) {
      return getParentFunction(ParentD, Ctx);
    }
    // Only look at first element
    break;
  }
  return nullptr;
}

template<typename NodeT>
static const Stmt* getParentFunctionBody(const NodeT *N, ASTContext &Ctx) {
  if (const FunctionDecl *F = getParentFunction(N, Ctx)) {
    return F->getBody();
  }
  return nullptr;
}

i64 ClangClog::cfg(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  const auto *S = Node.get<Stmt>();
  if (!S)
    return 0;

  const auto *Body = getParentFunctionBody(S, *Ctx);

  auto TheCFG =
    CFG::buildCFG(nullptr, const_cast<Stmt*>(Body), Ctx, CFG::BuildOptions().setAllAlwaysAdd());


  auto DumpFile = "cfg_" + std::to_string(NodeId) + ".dump";
  std::error_code EC;
  auto DumpStream = raw_fd_ostream(DumpFile, EC);
  llvm::dbgs() << "CFG dumped to " << DumpFile << "\n";
  TheCFG->print(DumpStream, LangOptions(), false);

  // TheCFG->viewCFG(LangOptions());

  return 0;
}

bool ClangClog::hasGlobalStorage(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  const auto *S = Node.get<VarDecl>();
  if (!S)
    return false;
  return S->hasGlobalStorage();
}

static const Stmt* nextStmtInBlock(CFGBlock::const_iterator Begin,
                                   CFGBlock::const_iterator End) {
  for (auto NextEIt = Begin; NextEIt != End; ++NextEIt) {
    if (NextEIt->getKind() == CFGElement::Statement) {
      return NextEIt->castAs<CFGStmt>().getStmt();
    }
  }
  return nullptr;
}

static const Stmt* prevStmtInBlock(CFGBlock::const_reverse_iterator RBegin,
                                   CFGBlock::const_reverse_iterator REnd) {
  for (auto PrevEIt = RBegin; PrevEIt != REnd; ++PrevEIt) {
    if (PrevEIt->getKind() == CFGElement::Statement) {
      return PrevEIt->castAs<CFGStmt>().getStmt();
    }
  }
  return nullptr;
}

static const Stmt* firstStmtInBlock(const CFGBlock *B) {
  const Stmt* NextStmt = nextStmtInBlock(B->begin(), B->end());
  if (NextStmt) {
    return NextStmt;
  } else if (const Stmt *SuccT = B->getTerminatorStmt()) {
    return SuccT;
  } else if (std::next(B->succ_begin()) == B->succ_end()) {
    // empty block with exactly one successor
    auto *FallthroughSucc = B->succ_begin()->getReachableBlock();
    // this may cause infinite loops and asserting on those of length 2 is cheap
    assert (FallthroughSucc != B && "Empty block is its own successor!");
    if (FallthroughSucc)
      return firstStmtInBlock(FallthroughSucc);
  }
  return nullptr;
}

static void lastStmtInBlockHelper(const CFGBlock *B, std::vector<const Stmt*> &LastStmt) {
  if (const Stmt *T = B->getTerminatorStmt()) {
    LastStmt.push_back(T);
    return;
  }

  if (const Stmt* PrevStmt = prevStmtInBlock(B->rbegin(), B->rend())) {
    LastStmt.push_back(PrevStmt);
    return;
  }

  for (const CFGBlock *PredB : B->preds()) {
    lastStmtInBlockHelper(PredB, LastStmt);
  }
}

static std::vector<const Stmt*> lastStmtInBlock(const CFGBlock *B) {
  std::vector<const Stmt*> LastStmt;
  lastStmtInBlockHelper(B, LastStmt);
  return LastStmt;
}

void ClangClog::ClangClogCFG::mapStmtsToSuccessors(const CFG &Cfg) {
  for (const CFGBlock *B : llvm::make_range(Cfg.begin(), Cfg.end())) {
    const Stmt* PrevS = nullptr;
    for (const CFGElement &E : llvm::make_range(B->begin(), B->end())) {
      if (const auto S = E.getAs<CFGStmt>()) {
        if (PrevS) {
          SuccStmt[PrevS].push_back(S->getStmt());
        }
        PrevS = S->getStmt();

        if (const DeclStmt *D = dyn_cast<DeclStmt>(S->getStmt())) {
          const auto *SingleDecl = D->getSingleDecl();
          DeclToStmt[SingleDecl] = D;
        }
      }
    }

    if (const Stmt *T = B->getTerminatorStmt()) {
      if (PrevS) {
        SuccStmt[PrevS].push_back(T);
      }
      PrevS = T;
    }

    if (PrevS) {
      for (CFGBlock::AdjacentBlock SuccB : B->succs()) {
        if (!SuccB.isReachable())
          continue;
        const Stmt *FirstInSuccessor = firstStmtInBlock(SuccB.getReachableBlock());
        if (FirstInSuccessor) {
          SuccStmt[PrevS].push_back(FirstInSuccessor);
        }
      }
    }
  }
}

ClangClog::ClangClogCFG::succ_range ClangClog::ClangClogCFG::successors(const Stmt *S) const {
  auto It = SuccStmt.find(S);
  if (It == SuccStmt.end()) {
    // Statement with no successors
    SmallVector<const Stmt*, 1> Empty;
    return llvm::make_range(Empty.begin(), Empty.end());
  }

  return llvm::make_range(It->getSecond().begin(), It->getSecond().end());
}

std::vector<i64> ClangClog::cfgSucc(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  const Stmt *S = Node.get<Stmt>();
  const Decl *D = Node.get<Decl>();

  if (!S && !D)
    return std::vector<i64>();

  const Stmt *Body = S ? getParentFunctionBody(S, *Ctx) : getParentFunctionBody(D, *Ctx);
  if (!Body)
    return std::vector<i64>();

  decltype(StmtToCFG)::iterator CfgIt;
  std::tie(CfgIt, std::ignore) = StmtToCFG.try_emplace(Body, Body, Ctx);

  const auto &Cfg = CfgIt->getSecond();


  if (D) {
    // This is a declaration
    S = Cfg.lookupStmtForDecl(D);
    if (!S)
      return std::vector<i64>();
  }

  std::vector<i64> Ret;
  for (const Stmt *Succ : Cfg.successors(S)) {
    if (const auto *SuccD = dyn_cast<DeclStmt>(Succ)) {
      Ret.push_back(getIdForNode(DynTypedNode::create(*SuccD->getSingleDecl()), Ctx));
    } else {
      Ret.push_back(getIdForNode(DynTypedNode::create(*Succ), Ctx));
    }
  }

  return Ret;
}

const Stmt* ClangClog::ClangClogCFG::entryStmt() const {
  return firstStmtInBlock(&Cfg->getEntry());
}

std::vector<const Stmt*> ClangClog::ClangClogCFG::exitStmts() const {
  std::vector<const Stmt*> Ret;
  lastStmtInBlockHelper(&Cfg->getExit(), Ret);
  return Ret;
}

i64 ClangClog::cfgEntry(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  if (const auto *F = Node.get<FunctionDecl>()) {
    if (F->hasBody()) {
      const Stmt *Body = F->getBody();

      decltype(StmtToCFG)::iterator CfgIt;
      std::tie(CfgIt, std::ignore) = StmtToCFG.try_emplace(Body, Body, Ctx);

      const auto &Cfg = CfgIt->getSecond();

      if (const auto *EntryStmt = Cfg.entryStmt())
        return getIdForNode(DynTypedNode::create(*EntryStmt), Ctx);
    }
  } else if (const auto *S = Node.get<Stmt>()) {
    auto LocalCfg = CFG::buildCFG(nullptr, const_cast<Stmt*>(S), Ctx, CFG::BuildOptions().setAllAlwaysAdd());
    if (!LocalCfg) {
      // If this node does not contain any CFG elements, e.g. it's a break statements, then just return
      // the node itself
      return NodeId;
    } else if (const auto *EntryStmt = firstStmtInBlock(&LocalCfg->getEntry())) {
      if (const auto *DS = dyn_cast<DeclStmt>(EntryStmt)) {
        if (DS->decl_begin() != DS->decl_end()) {
          return getIdForNode(DynTypedNode::create(**DS->decl_begin()), Ctx);
        }
      }
      return getIdForNode(DynTypedNode::create(*EntryStmt), Ctx);
    }

  }
  return 0;
}

std::vector<i64> ClangClog::cfgExit(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  if (const auto *F = Node.get<FunctionDecl>()) {
    if (F->hasBody()) {
      const Stmt *Body = F->getBody();


      decltype(StmtToCFG)::iterator CfgIt;
      std::tie(CfgIt, std::ignore) = StmtToCFG.try_emplace(Body, Body, Ctx);

      const auto &Cfg = CfgIt->getSecond();

      std::vector<i64> Ret;
      for (const auto *Stmt : Cfg.exitStmts()) {
        Ret.push_back(getIdForNode(DynTypedNode::create(*Stmt), Ctx));
      }
      return Ret;
    }
  }

  return std::vector<i64>();
}

std::string ClangClog::dump(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  std::string Str;
  llvm::raw_string_ostream Out(Str);
  Node.dump(Out, *Ctx);
  return Str;
}

i64 ClangClog::enclosingFunction(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);
  const Stmt *S = Node.get<Stmt>();
  const Decl *D = Node.get<Decl>();

  if (!S && !D)
    return 0;

  if (const FunctionDecl *F = S ? getParentFunction(S, *Ctx) : getParentFunction(D, *Ctx)) {
    return getIdForNode(DynTypedNode::create(*F), Ctx);
  }

  return 0;
}

ClangClogBuilder::~ClangClogBuilder() {
  // Argv[I] is not new[]'d, so start from 1.
  for (unsigned I = 1; I < Argc; ++I) {
    delete[] Argv[I];
  }
  delete[] Argv;
}

const char **ClangClogBuilder::buildArgv(const std::vector<std::string> &Args) {
  const char **Argv = new const char*[Args.size() + 2];

  Argv[0] = "clang-clog";
  Argv[Args.size() + 1] = nullptr;

  for (unsigned I = 0; I < Args.size(); ++I) {
    char *Tmp = new char[Args[I].size() + 1];
    std::memcpy(Tmp, Args[I].c_str(), Args[I].length() + 1);
    Argv[I + 1] = Tmp;
  }

  return Argv;
}


static llvm::cl::OptionCategory ClangClogCategory("clang-clog options");

tooling::CommonOptionsParser ClangClogBuilder::buildOptionsParser(int Argc, const char **Argv) {
  auto P = CommonOptionsParser::create(Argc, Argv, ClangClogCategory, llvm::cl::OneOrMore);
  if (!P) {
    llvm::errs() << P.takeError() << "\n";
    llvm_unreachable("Ooops! Failed to build the OptionParser.");
  } else {
    llvm::dbgs() << "Clog flags: ";
    for (unsigned I = 0; I < Argc; ++I) {
      dbgs() << Argv[I] << " ";
    }
    dbgs() << "\n";
  }
  return std::move(*P);
}

ClangClogBuilder::ClangClogBuilder(const std::vector<std::string> &Args) :
  Argc(Args.size() + 1),
  Argv(buildArgv(Args)),
  OptionsParser(buildOptionsParser(Argc, Argv))
{
  Argv = new const char*[Args.size() + 2];

  Argv[0] = "clang-clog";
  Argv[Args.size() + 1] = nullptr;

  for (unsigned I = 0; I < Args.size(); ++I) {
    char *Tmp = new char[Args[I].size() + 1];
    std::memcpy(Tmp, Args[I].c_str(), Args[I].length() + 1);
    Argv[I + 1] = Tmp;
  }

  Argc = Args.size() + 1;
}



ClangClog* ClangClogBuilder::build() {
  if (Instance)
    return Instance;

  Instance = new ClangClog(OptionsParser.getCompilations(),
                           OptionsParser.getSourcePathList());

  return Instance;
}

} // namespace clog
} // namespace clang
