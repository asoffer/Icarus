#ifndef ICARUS_FRONTEND_DEPRECATED_SOURCE_H
#define ICARUS_FRONTEND_DEPRECATED_SOURCE_H

#include <string_view>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "frontend/source.h"

struct Module;

namespace error {
struct Log;
}  // namespace error

namespace ast {
struct Statements;
}  // namespace ast

namespace frontend {

struct Source {
  using Name = std::string;
  using Line = std::string;

  virtual ~Source() {}
  virtual std::vector<std::string> LoadLines()                          = 0;
  virtual std::optional<Line> NextLine()                                = 0;
  virtual std::unique_ptr<ast::Statements> Parse(Module *, error::Log*) = 0;

  Name name;
  bool seen_eof = false;
  std::string current_line_;

 protected:
  Source(Name name) : name(std::move(name)) {}
};

struct Repl : public Source {
  ~Repl() final {}
  Repl() : Source(Source::Name("")) {}

  std::vector<std::string> LoadLines() final { return lines_; };
  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<ast::Statements> Parse(Module *, error::Log*) final;
  bool first_entry = true;
  std::vector<std::string> lines_{1};
};

// Hack for the time being so we can migrate to using Src
struct SrcSource: public Source {
  ~SrcSource() final {}
  SrcSource(FileSrc src) : Source(src.path().string()), src_(std::move(src)) {}

  std::vector<std::string> LoadLines() final;
  std::optional<Source::Line> NextLine() final;
  std::unique_ptr<ast::Statements> Parse(Module *, error::Log *) final;

  FileSrc src_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_DEPRECATED_SOURCE_H
