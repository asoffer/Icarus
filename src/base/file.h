#ifndef ICARUS_BASE_FILE_H
#define ICARUS_BASE_FILE_H

#include <fstream>
#include <string>
#include <vector>

#include "util/pstr.h"

namespace AST {
struct Statements;
} // namespace AST

struct File {
  File(const std::string &file_name = "")
      : name(file_name), ast(nullptr), ifs(name, std::ifstream::in) {}
  ~File() { ifs.close(); }

  std::string name;
  std::vector<pstr> lines;
  AST::Statements *ast;
  std::ifstream ifs;
};

#endif // ICARUS_BASE_FILE_H
