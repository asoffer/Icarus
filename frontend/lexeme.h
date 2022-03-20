#ifndef ICARUS_FRONTEND_LEXEME_H
#define ICARUS_FRONTEND_LEXEME_H

#include <ostream>
#include <string_view>

#include "base/debug.h"

namespace frontend {

struct Lexeme {
  enum class Kind {
    LeftDelimiter  = 0,
    RightDelimiter = 1,
    Comma,
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

  Lexeme() : Lexeme(Kind::EndOfFile, "") {}
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

  size_t match_offset() const {
    ASSERT(delimiter() == true);
    return length_;
  }

  void set_match_offset(uint64_t offset) {
    ASSERT(delimiter() == true);
    length_ = offset;
  }

  friend std::ostream& operator<<(std::ostream& os, Lexeme  l) {
    return os << "[kind = " << static_cast<int>(l.kind()) << "](content: " << l.content()
              << ")";
  }

 private:
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

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEXEME_H
