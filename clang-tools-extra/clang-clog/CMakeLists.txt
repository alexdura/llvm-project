set(LLVM_LINK_COMPONENTS
  support
  FrontendOpenMP

  )

add_clang_library(clangClog
  ClangClog.cpp

  DEPENDS
  omp_gen
  )

clang_target_link_libraries(clangClog
  PRIVATE
  clangQuery
  clangAnalysis
  clangAST
  clangASTMatchers
  clangBasic
  clangDynamicASTMatchers
  clangFrontend
  clangTooling
  clangSerialization
  )

add_subdirectory(tool)
add_subdirectory(swig)
