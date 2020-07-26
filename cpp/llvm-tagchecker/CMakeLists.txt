set(LLVM_LINK_COMPONENTS
  FrontendOpenMP
  Support
  )

add_clang_executable(tag-checker
  TagChecker.cpp
  )

clang_target_link_libraries(tag-checker
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  clangToolingRefactoring
  )
