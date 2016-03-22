cmake_minimum_required(VERSION 3.0)

project(clang-test)

# Should work, but apt packages don't provide
# the ClangConfig.cmake file.
# find_package(Clang 3.6 CONFIG REQUIRED)

# Be lazy until the right thing (above) works:
execute_process(
  COMMAND llvm-config-3.6 --libfiles
  OUTPUT_VARIABLE LLVM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_LIBS ${LLVM_LIBS})
execute_process(
  COMMAND llvm-config-3.6 --includedir
  OUTPUT_VARIABLE LLVM_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
set(CMAKE_CXX_STANDARD 14)
add_library(ClangAPI INTERFACE)
set_property(TARGET ClangAPI PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${LLVM_INCLUDE_DIR})
set_property(TARGET ClangAPI PROPERTY INTERFACE_LINK_LIBRARIES ${LLVM_LIBS})
# End lazyness

add_library(clang-plugin-test MODULE clang-plugin-test.cpp)
target_link_libraries(clang-plugin-test PRIVATE ClangAPI)

# clang++ -std=c++11 -Xclang -load -Xclang ./libclang-plugin-test.so -Xclang -add-plugin -Xclang clang-plugin-test -c ../testinput.cpp
