#ifndef ICARUS_BASE_SOURCE_H
#define ICARUS_BASE_SOURCE_H

#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "strong_types.h"

namespace error {
struct Log;
}
namespace AST {
struct Statements;
}

struct Source {
  DEFINE_STRONG_STRING(Name);
  DEFINE_STRONG_STRING(Line);

  virtual ~Source() {}
  virtual std::optional<Line> NextLine() = 0;
  virtual std::unique_ptr<AST::Statements> Parse(error::Log *) = 0;

  std::vector<Line> lines{1}; // Start with one blank line because line numbers
                              // are 1-indexed not 0-indexed.
  // TODO this is a hacky way to do it and you should just shift the counter by
  // one.

  Name name;
  bool seen_eof = false;

protected:
  Source(Name name) : name(std::move(name)) {}
};

DEFINE_STRONG_HASH(Source::Name);
DEFINE_STRONG_HASH(Source::Line);

struct Repl: public Source {
  ~Repl() final {}
  Repl() : Source(Source::Name("")) {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<AST::Statements> Parse(error::Log *) final;

  bool first_entry = true;
};

struct File : Source {
  File(Source::Name source_name)
      : Source(std::move(source_name)), ifs(name.c_str(), std::ifstream::in) {}
  ~File() final {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<AST::Statements> Parse(error::Log*) final;

  AST::Statements *ast = nullptr;
  std::ifstream ifs;
};

#endif // ICARUS_BASE_SOURCE_H
