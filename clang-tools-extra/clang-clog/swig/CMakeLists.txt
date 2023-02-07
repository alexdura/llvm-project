include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)

find_package(JNI)

if (JNI_FOUND)
    message (STATUS "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")
    message (STATUS "JNI_LIBRARIES=${JNI_LIBRARIES}")
endif()

include_directories(${JNI_INCLUDE_DIRS})

add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/clang-clog_wrap.cxx
  COMMAND /usr/bin/swig4.0 -DSWIGWORDSIZE64 -java -c++ clang-clog.i
  DEPENDS clang-clog.i ${CMAKE_CURRENT_SOURCE_DIR}/../ClangClog.h
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

add_clang_library(clangClogSWIG
  clang-clog_wrap.cxx

  DEPENDS clangClog
)

clang_target_link_libraries(clangClogSWIG
  PRIVATE
  clangQuery
  clangAST
  clangASTMatchers
  clangBasic
  clangDynamicASTMatchers
  clangFrontend
  clangTooling
  clangSerialization
  clangClog
  )



set_source_files_properties(clang-clog_wrap.cxx PROPERTIES COMPILE_FLAGS -fexceptions)
