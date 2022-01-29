#ifndef ICARUS_FRONTEND_LEX_LEX_H
#define ICARUS_FRONTEND_LEX_LEX_H

#include <optional>
#include <string_view>
#include <vector>

#include "base/debug.h"
#include "diagnostic/consumer/consumer.h"

namespace frontend {

struct Lexeme {
  enum class Kind {
    LeftDelimiter = 0,
    RightDelimiter = 1,
    Comment,
    Character,
    String,
    Identifier,
    Hash,
    Number,
    Operator,
    Newline,
    EndOfFile,
  };

  explicit Lexeme(Kind k, std::string_view content)
      : data_(content.data()),
        kind_(static_cast<uint64_t>(k)),
        length_(content.size()) {
    ASSERT(content.length() < (uint64_t{1} << 60));
  }

  constexpr Kind kind() const { return static_cast<Kind>(kind_); }
  constexpr bool delimiter() const { return kind_ == 0 or kind_ == 1; }

  constexpr std::string_view content() const {
    if (delimiter()) { return std::string_view(data_, 1); }
    return std::string_view(data_, length_);
  }

 private:
  friend struct LexResultBuilder;

  void set_match_offset(uint64_t offset) {
    ASSERT(delimiter() == true);
    length_ = offset;
  }

  char const *data_;
  struct {
    uint64_t kind_ : 4;
    // `length_` represents the length of the string content starting at `data_`
    // for most Lexeme kinds. However, delimiters are always known to have
    // length 1, and so we reuse these bits in that case to store the relative
    // offset of the matching delimiter.
    uint64_t length_ : 60;
  };
};

struct LexResult {
 private:
  friend struct LexResultBuilder;
  std::vector<Lexeme> lexemes_;
};

// Analyzes `source` constructing a `LexResult` object or `nullopt` if `source`
// does not represent a lexically valid program. Parts of the returned
// `LexResult` may reference ranges of characters in `source`. Thus, underlying
// data referenced by `source` must remain valid for the lifetime of the return
// value of this function.
std::optional<LexResult> Lex(
    std::string_view source,
    diagnostic::DiagnosticConsumer &diagnostic_consumer);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H
