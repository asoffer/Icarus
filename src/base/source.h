#ifndef ICARUS_BASE_SOURCE_H
#define ICARUS_BASE_SOURCE_H

#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

struct Context;

namespace AST {
struct Statements;
}

struct Source {
  using Name = std::string;
  using Line = std::string;

  virtual ~Source() {}
  virtual std::optional<Line> NextLine() = 0;
  virtual std::unique_ptr<AST::Statements> Parse(Context *) = 0;

  std::vector<Line> lines{1}; // Start with one blank line because line numbers
                              // are 1-indexed not 0-indexed.
  // TODO this is a hacky way to do it and you should just shift the counter by
  // one.

  Name name;
  bool seen_eof = false;

protected:
  Source(Name name) : name(std::move(name)) {}
};

struct Repl: public Source {
  ~Repl() final {}
  Repl() : Source(Source::Name("")) {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<AST::Statements> Parse(Context *) final;

  bool first_entry = true;
};

struct File : Source {
  File(Source::Name source_name)
      : Source(std::move(source_name)), ifs(name.c_str(), std::ifstream::in) {}
  ~File() final {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<AST::Statements> Parse(Context*) final;

  AST::Statements *ast = nullptr;
  std::ifstream ifs;
};

#endif // ICARUS_BASE_SOURCE_H
