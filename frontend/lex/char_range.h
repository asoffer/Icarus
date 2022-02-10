#ifndef ICARUS_FRONTEND_LEX_CHAR_RANGE_H
#define ICARUS_FRONTEND_LEX_CHAR_RANGE_H

namespace frontend {

// `char_range` is a data structure representing a range of characters with a
// similar API and usage pattern to `std::string_view`. However, the internal
// representation of `char_range` holds two pointers rather than a pointer and a
// length. The benefit of this representation is that while lexing we frequently
// consume characters from the prefix. `char_range` will only need to update one
// pointer, whereas `std::string_view::remove_prefix` would need to update both
// the data pointer and the range length.
struct char_range {
  char_range(std::string_view s) : head_(s.data()), tail_(head_ + s.size()) {}

  constexpr char operator[](size_t n) const {
    if (not std::is_constant_evaluated()) { ASSERT(tail_ - head_ > n); }
    return head_[n];
  }

  constexpr char const *data() const { return head_; }

  constexpr size_t size() const { return tail_ - head_; }
  constexpr bool empty() const { return head_ == tail_; }

  constexpr char const *begin() const { return head_; }
  constexpr char const *end() const { return tail_; }

  constexpr void remove_prefix(int64_t n) { head_ += n; }

  constexpr std::string_view extract_prefix(int64_t n) {
    std::string_view result(head_, n);
    head_ += n;
    return result;
  }

  bool starts_with(std::string_view s) {
    return std::string_view(head_, tail_ - head_).starts_with(s);
  }

 private:
  char const *head_;
  char const *tail_;
};

namespace internal_char_bits {

struct char_bits {
  bool alpha : 1;
  bool upper : 1;
  bool digit : 1;
  bool hex_digit : 1;
  bool vertical_whitespace : 1;
  bool horizontal_whitespace : 1;
  bool printable : 1;
};

inline constexpr std::array<char_bits, 256> kCharBitsTable = [] {
  std::array<char_bits, 256> result{};
  for (int n = 0; n < 256; ++n) {
    if ('a' <= n and n <= 'z') {
      result[n].alpha     = true;
      result[n].printable = true;
      if (n <= 'f') { result[n].hex_digit = true; }
    } else if ('A' <= n and n <= 'Z') {
      result[n].alpha = true;
      result[n].upper = true;
      if (n <= 'F') { result[n].hex_digit = true; }
      result[n].printable = true;
    } else if ('0' <= n and n <= '9') {
      result[n].digit     = true;
      result[n].hex_digit = true;
      result[n].printable = true;
    } else if (n == '\n' or n == '\r') {
      result[n].horizontal_whitespace = true;
      result[n].printable             = true;
    } else if (n == ' ' or n == '\t') {
      // Note: Vertical tabs are not considered valid as either horizontal or
      // vertical whitespace.
      result[n].vertical_whitespace = true;
      result[n].printable           = true;
    } else {
      result[n].printable = (n > 31 and n < 127);
    }
  }
  return result;
}();

}  // namespace internal_char_bits

inline constexpr bool IsDigit(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].digit;
}

inline constexpr bool IsAlpha(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].alpha;
}

inline constexpr bool IsHexDigit(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].hex_digit;
}

inline constexpr bool IsAlphaNumeric(uint8_t c) {
  return IsAlpha(c) or IsDigit(c);
}

inline constexpr bool IsHorizontalWhitespace(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].horizontal_whitespace;
}

inline constexpr bool IsVerticalWhitespace(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].vertical_whitespace;
}

inline constexpr bool IsWhitespace(uint8_t c) {
  return IsVerticalWhitespace(c) or IsHorizontalWhitespace(c);
}

inline constexpr bool IsAlphaOrUnderscore(uint8_t c) {
  return IsAlpha(c) or (c == '_');
}

inline constexpr bool IsAlphaNumericOrUnderscore(uint8_t c) {
  return IsAlphaNumeric(c) or (c == '_');
}

inline constexpr bool IsPrintable(uint8_t c) {
  return internal_char_bits::kCharBitsTable[c].printable;
}

}  // namespace frontend
#endif  // ICARUS_FRONTEND_LEX_CHAR_RANGE_H
