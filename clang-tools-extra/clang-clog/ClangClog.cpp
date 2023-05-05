#include "ClangClog.h"
#include <string>
#include <iterator>
#include <vector>
#include "clang/AST/Expr.h"
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
  auto Node = NodeIds.getEntry(NodeId);
  auto SR = Node.getSourceRange();
  auto ASTIt = NodeToAST.find(Node);
  if (ASTIt == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node id " << NodeId << "\n";
    llvm_unreachable("Could not find ASTContext for node.");
  }

  const auto &SM = ASTIt->second->getSourceManager();

  return {
    SM.getPresumedLoc(SM.getSpellingLoc(SR.getBegin())).getFilename(),
    SM.getSpellingLineNumber(SR.getBegin()),
    SM.getSpellingColumnNumber(SR.getBegin()),
    SM.getSpellingLineNumber(SR.getEnd()),
    SM.getSpellingColumnNumber(SR.getEnd())
  };
}

i64 ClangClog::type(i64 NodeId) {
  auto Node = NodeIds.getEntry(NodeId);
  auto ASTIt = NodeToAST.find(Node);
  if (ASTIt == NodeToAST.end()) {
    return 0;
  }

  if (const auto *E = Node.get<Expr>()) {
    const auto &T = E->getType();
    auto DynNode = DynTypedNode::create(T);
    NodeToAST.insert(std::make_pair(DynNode, ASTIt->getSecond()));
    return NodeIds.getId(DynTypedNode::create(T));
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
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << ".";
    llvm_unreachable("Ooops!");
  }

  auto ParentNode = NodeIds.getEntry(ParentId);

  for (const auto &ParentCandidate : It->getSecond()->getParents(Node)) {
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
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << ".";
    llvm_unreachable("Ooops!");
  }

  auto AncestorNode = NodeIds.getEntry(AncestorId);

  return isAncestorHelper(AncestorNode, Node, *It->getSecond());
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
  auto Node = NodeIds.getEntry(NodeId);
  auto It = NodeToAST.find(Node);
  if (It == NodeToAST.end()) {
    llvm::errs() << "Could not find ASTContext for node nodeId=" << NodeId << ".";
    llvm_unreachable("Ooops!");
  }

  auto Parents = It->second->getParents(Node);

  if (Parents.empty()) {
    return 0;
  } else {
    auto PIt = Parents.begin();
    auto ParentNode = *PIt;

    if (std::next(PIt) != Parents.end()) {
      llvm::errs() << "Expecting that nodeId=" << NodeId << " has at most one parent. This should hold for C (no templates)";
      llvm_unreachable("Ooops!");
    }

    NodeToAST.insert(std::make_pair(ParentNode, It->second));
    return NodeIds.getId(ParentNode);
  }
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
