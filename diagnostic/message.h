#ifndef ICARUS_DIAGNOSTIC_MESSAGE_H
#define ICARUS_DIAGNOSTIC_MESSAGE_H

#include <algorithm>
#include <string>
#include <variant>

#include "absl/strings/str_format.h"
#include "frontend/source/buffer.h"
#include "frontend/source/source.h"

namespace diagnostic {
// TODO Do you need `Fatal`?
enum class Category { Note, Warning, Error, Fatal };

struct Style {
  enum class Color : uint8_t {
    Black   = 0,
    Red     = 1,
    Green   = 2,
    Yellow  = 3,
    Blue    = 4,
    Magenta = 5,
    Cyan    = 6,
    White   = 7
  } color = Color::White;

  static constexpr Style ErrorText() { return Style{.color = Color::Red}; }
};

struct Highlight {
  Highlight(frontend::SourceRange range, Style style)
      : range(range), style(style) {}
  frontend::SourceRange range;
  Style style;
};

struct SourceQuote {
  explicit SourceQuote(frontend::Source const* source) : source(source) {}

  // TODO: implement for real.
  SourceQuote& Highlighted(frontend::SourceRange range, Style style) {
    if (source) {
      auto line_interval = range.lines(source->buffer());
      line_interval.begin() =
          std::max(line_interval.begin() - 1, frontend::LineNum(1));
      line_interval.end() =
          std::min(line_interval.end() + 1,
                   frontend::LineNum(source->buffer().num_lines()));

      lines.insert(line_interval);
    }
    highlights.emplace_back(range, style);
    return *this;
  }

  SourceQuote& Line(frontend::LineNum l) {
    lines.insert(
        base::Interval<frontend::LineNum>(l, l + 1).expanded(1).clamped_below(
            frontend::LineNum(1)));
    return *this;
  }

  frontend::Source const* source;
  base::IntervalSet<frontend::LineNum> lines;
  std::vector<Highlight> highlights;
};

struct Text {
  explicit Text(std::string message) : message_(std::move(message)) {}
  template <typename... Args, std::enable_if_t<sizeof...(Args) != 0>* = nullptr>
  explicit Text(absl::FormatSpec<Args...> const& format, Args const&... args)
      : message_(absl::StrFormat(format, args...)) {}

  char const* c_str() const { return message_.c_str(); }

 private:
  std::string message_;
};

struct List {
  explicit List(std::vector<std::string> items) : items_(std::move(items)) {}
  absl::Span<std::string const> items() const { return items_; }

 private:
  std::vector<std::string> items_;
};

struct DiagnosticMessage {
 private:
  using Component = std::variant<SourceQuote, Text, List>;

 public:
  template <typename... Ts>
  DiagnosticMessage(Ts&&... ts) {
    components_.reserve(sizeof...(Ts));
    (components_.emplace_back(std::forward<Ts>(ts)), ...);
  }

  template <typename Fn>
  void for_each_component(Fn const& fn) const {
    for (auto const& component : components_) { std::visit(fn, component); }
  }

 private:
  std::vector<Component> components_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_MESSAGE_H
