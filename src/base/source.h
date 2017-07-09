#ifndef ICARUS_BASE_SOURCE_H
#define ICARUS_BASE_SOURCE_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <memory>

#include "owned_ptr.h"

namespace AST {
struct Statements;
}

struct Source {
  struct Line {
    std::string text;
    bool eof = false;
  };

  virtual ~Source() {}
  virtual Line NextLine() = 0;
  virtual base::owned_ptr<AST::Statements> Parse() = 0;

  std::string name;
  std::vector<std::string> lines{1}; // Start with one blank line because line
                                     // numbers are 1-indexed not 0-indexed.
  // TODO this is a hacky way to do it and you should just shift the counter by
  // one.
};

struct Repl: public Source {
  ~Repl() final {}
  Repl() { name = "<<REPL>>"; }

  Source::Line NextLine() final;
  base::owned_ptr<AST::Statements> Parse() final;

  bool first_entry = true;
};

struct File : Source {
  File(const std::string &file_name = "")
      : ifs(file_name, std::ifstream::in) {
    name = file_name;
  }
  ~File() final { ifs.close(); }

  Source::Line NextLine() final;
  base::owned_ptr<AST::Statements> Parse() final;

  AST::Statements *ast = nullptr;
  std::ifstream ifs;
};

#endif // ICARUS_BASE_SOURCE_H
