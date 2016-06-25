#ifndef ICARUS_UTIL_SOURCE_FILE_H
#define ICARUS_UTIL_SOURCE_FILE_H

#include "util/pstr.h"

namespace AST {
struct Statements;
} // namespace AST

struct SourceFile {
  SourceFile(const std::string &file_name = "")
      : name(file_name), ast(nullptr) {}
  std::string name;
  std::vector<pstr> lines;
  AST::Statements *ast;
};

#endif // ICARUS_UTIL_SOURCE_FILE_H
