#ifndef ICARUS_BASE_SOURCE_H
#define ICARUS_BASE_SOURCE_H

#include "../util/pstr.h"

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

namespace AST {
struct Statements;
}

struct Source {
  virtual ~Source() {}
  virtual std::pair<bool, std::string> NextLine() = 0;

  std::string name;
  std::vector<pstr> lines;
};

struct ReplSource : public Source {
  ~ReplSource() final {}
  ReplSource(std::vector<std::string> lines) : lines_(std::move(lines)) {
    name = "<<REPL>>";
  }

  std::pair<bool, std::string> NextLine() final;

  size_t index_ = 0;
  std::vector<std::string> lines_;
};

struct File : Source {
  File(const std::string &file_name = "") : ifs(name, std::ifstream::in) {
    name = file_name;
  }
  ~File() final { ifs.close(); }

  std::pair<bool, std::string> NextLine() final;
  AST::Statements *ast = nullptr;
  std::ifstream ifs;
};

#endif // ICARUS_BASE_SOURCE_H
