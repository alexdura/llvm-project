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

i64 ClangClog::registerMatcher(const std::string &Pattern, bool IsGlobal) {
  StringRef PatRef = Pattern;
  Diagnostics Diag;
  auto Matcher = Parser::parseMatcherExpression(PatRef, &Diag);


  if (!Matcher) {
    Diag.printToStreamFull(llvm::errs());
    return -1;
  }

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
    Ctx.getParentMapContext().setTraversalKind(TK_IgnoreUnlessSpelledInSource);
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

std::vector<std::vector<i64>> ClangClog::matchFromNode(i64 MatcherId, i64 NodeId) {
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

  return {
    SM.getPresumedLoc(SM.getSpellingLoc(SR.getBegin())).getFilename(),
    SM.getSpellingLineNumber(SR.getBegin()),
    SM.getSpellingColumnNumber(SR.getBegin()),
    SM.getSpellingLineNumber(SR.getEnd()),
    SM.getSpellingColumnNumber(SR.getEnd())
  };
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

i64 ClangClog::parent(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

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

static const Stmt* getParentFunctionBody(const Stmt* N, ASTContext &Ctx) {
  for (const auto &P : Ctx.getParents(*N)) {
    if (P.get<FunctionDecl>()) {
      return N;
    } else if (const auto *ParentS = P.get<Stmt>()) {
      return getParentFunctionBody(ParentS, Ctx);
    }
    // Only look at first element
    break;
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


  auto DumpFile = "cfg_" + std::to_string(NodeId) + ".dot";
  std::error_code EC;
  auto DumpStream = raw_fd_ostream(DumpFile, EC);
  llvm::dbgs() << "CFG dumped to " << DumpFile << "\n";
  TheCFG->print(DumpStream, LangOptions(), false);

  // TheCFG->viewCFG(LangOptions());

  return 0;
}

DenseMap<const Stmt*, const CFGBlock*> ClangClog::ClangClogCFG::mapStmtsToBlocks(const CFG &Cfg) {
  DenseMap<const Stmt *, const CFGBlock *> Ret;

  for (const CFGBlock *B : llvm::make_range(Cfg.begin(), Cfg.end())) {
    if (const Stmt *T = B->getTerminatorStmt())
      Ret[T] = B;
    for (const CFGElement &E : llvm::make_range(B->begin(), B->end())) {
      if (const auto S = E.getAs<CFGStmt>()) {
        Ret[S->getStmt()] = B;
      }
    }
  }

  return Ret;
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


std::vector<i64> ClangClog::cfgSucc(i64 NodeId) {
  DynTypedNode Node;
  ASTContext *Ctx;
  std::tie(Node, Ctx) = getNodeFromId(NodeId);

  const auto *S = Node.get<Stmt>();
  if (!S)
    return std::vector<i64>();


  const Stmt *Body = getParentFunctionBody(S, *Ctx);
  if (!Body)
    return std::vector<i64>();

  decltype(StmtToCFG)::iterator CfgIt;
  std::tie(CfgIt, std::ignore) = StmtToCFG.try_emplace(S, Body, Ctx);

  auto BIt = CfgIt->second.StmtToBlock.find(S);
  if (BIt == CfgIt->second.StmtToBlock.end())
    return std::vector<i64>();

  const auto *B = BIt->second;

  for (auto EIt = B->begin(); EIt != B->end(); ++EIt) {
    if (EIt->getKind() != CFGElement::Statement)
      continue;

    const auto &S1 = EIt->castAs<CFGStmt>();
    if (S1.getStmt() != S)
      continue;

    // Found the current Stmt, look for the next one
    const Stmt *NextStmt = nextStmtInBlock(std::next(EIt), B->end());
    if (NextStmt) {
      return {getIdForNode(DynTypedNode::create(*NextStmt), Ctx)};
    } else if (const Stmt *T = B->getTerminatorStmt()) {
      return {getIdForNode(DynTypedNode::create(*T), Ctx)};
    } else {
      // If we reached this point, then the successors are not in this block
      std::vector<i64> Successors;
      for (CFGBlock::AdjacentBlock SuccB : B->succs()) {
        // TODO: For now we skip unreachable blocks as successors. Decide
        // if this is a good idea.
        if (!SuccB.isReachable())
          continue;

        // Assume that the successor has at least one statement
        const auto *First = firstStmtInBlock(SuccB.getReachableBlock());
        if (First) {
          Successors.push_back(getIdForNode(DynTypedNode::create(*First), Ctx));
        }
      }
      return Successors;
    }
  }

  if (S == B->getTerminatorStmt()) {
    std::vector<i64> Successors;
    for (CFGBlock::AdjacentBlock SuccB : B->succs()) {
      if (!SuccB.isReachable())
        continue;
      // Assume that the successor has at least one statement
      const auto *First = firstStmtInBlock(SuccB.getReachableBlock());
      if (First) {
        Successors.push_back(getIdForNode(DynTypedNode::create(*First), Ctx));
      }
    }
    return Successors;
  }

  return {};
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
    llvm_unreachable("Ooops! Failed to build the OptionParser.");
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
