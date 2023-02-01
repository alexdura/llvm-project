#include <vector>
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

  std::vector<uint64_t> children(const uint64_t NodeId);

  uint64_t parent(const uint64_t NodeId);
};
} // namespace clog
} // namespace clang
