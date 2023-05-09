include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)

add_clang_tool(clang-clog
  clang-clog.cpp)

clang_target_link_libraries(clang-clog
  PRIVATE
  clangQuery
  clangAST
  clangAnalysis
  clangASTMatchers
  clangBasic
  clangDynamicASTMatchers
  clangFrontend
  clangTooling
  clangSerialization
  )

target_link_libraries(clang-clog
  PRIVATE
  clangClog)
