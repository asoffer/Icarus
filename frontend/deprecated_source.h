#ifndef ICARUS_FRONTEND_DEPRECATED_SOURCE_H
#define ICARUS_FRONTEND_DEPRECATED_SOURCE_H

#include <string_view>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "frontend/source.h"

struct Context;

namespace ast {
struct Statements;
}

namespace frontend {

struct Source {
  using Name = std::string;
  using Line = std::string;

  virtual ~Source() {}
  virtual std::optional<Line> NextLine()                    = 0;
  virtual std::unique_ptr<ast::Statements> Parse(Context *) = 0;

  std::vector<Line> lines{
      1};  // Start with one blank line because line numbers
           // are 1-indexed not 0-indexed.
  // TODO this is a hacky way to do it and you should just shift the counter by
  // one.

  Name name;
  bool seen_eof = false;

 protected:
  Source(Name name) : name(std::move(name)) {}
};

struct Repl : public Source {
  ~Repl() final {}
  Repl() : Source(Source::Name("")) {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<ast::Statements> Parse(Context *) final;

  bool first_entry = true;
};

// Hack for the time being so we can migrate to using Src
struct SrcSource: public Source {
  ~SrcSource() final {}
  SrcSource(FileSrc src) : Source(src.path().string()), src_(std::move(src)) {}

  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<ast::Statements> Parse(Context *) final;

  ast::Statements *ast = nullptr;
  FileSrc src_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_DEPRECATED_SOURCE_H
