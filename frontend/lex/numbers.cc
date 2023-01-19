#include "frontend/lex/numbers.h"

#include <string>
#include <string_view>
#include <variant>

namespace frontend {
namespace {
template <int Base>
int64_t DigitInBase(char c) {
  if constexpr (Base == 10) {
    return ('0' <= c and c <= '9') ? (c - '0') : -1;
  } else if constexpr (Base == 2) {
    return ((c | 1) == '1') ? (c - '0') : -1;
  } else if constexpr (Base == 8) {
    return ((c | 7) == '7') ? (c - '0') : -1;
  } else if constexpr (Base == 16) {
    int digit = DigitInBase<10>(c);
    if (digit != -1) { return digit; }
    if ('A' <= c and c <= 'F') { return c - 'A' + 10; }
    if ('a' <= c and c <= 'f') { return c - 'a' + 10; }
    return -1;
  }
}

template <int Base>
bool IntRepresentableInBase(std::string_view s) {
  if constexpr (Base == 10) {
    // TODO this is specific to 64-bit integers.
    return s.size() < 19 or (s.size() == 19 and s <= "9223372036854775807");
  } else if constexpr (Base == 2) {
    return s.size() < kMaxIntBytes * 8;
  } else if constexpr (Base == 8) {
    constexpr const char kFirstCharLimit[] = "371";
    return (s.size() < (kMaxIntBytes * 8 / 3)) or
           (s.size() == kMaxIntBytes and
            s[0] <= kFirstCharLimit[kMaxIntBytes % 3]);
  } else if constexpr (Base == 16) {
    return (s.size() < kMaxIntBytes) or
           (s.size() == kMaxIntBytes and s[0] <= '7');
  }
}

template <int Base>
std::variant<nth::Integer, double, NumberParsingError> ParseIntInBase(
    std::string_view s) {
  if (not IntRepresentableInBase<Base>(s)) {
    return NumberParsingError::kTooLarge;
  }
  nth::Integer result = 0;
  for (char c : s) {
    int64_t digit = DigitInBase<Base>(c);
    if (digit == -1) { return NumberParsingError::kInvalidDigit; }
    result = result * Base + digit;
  }
  return result;
}

template <int Base>
std::variant<nth::Integer, double, NumberParsingError> ParseRealInBase(
    std::string_view s, int dot) {
  int64_t int_part = 0;
  for (int i = 0; i < dot; ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return NumberParsingError::kInvalidDigit; }
    int_part = int_part * Base + digit;
  }

  int64_t frac_part = 0;
  int64_t exp       = 1;
  for (size_t i = dot + 1; i < s.size(); ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return NumberParsingError::kInvalidDigit; }
    exp *= Base;
    frac_part = frac_part * Base + digit;
  }
  return int_part + static_cast<double>(frac_part) / exp;
}

template <int Base>
std::variant<nth::Integer, double, NumberParsingError> ParseNumberInBase(
    std::string_view sv) {
  std::string copy;
  for (char c : sv) {
    if (c != '_') { copy.push_back(c); }
  }

  int first_dot   = -1;
  size_t num_dots = 0;
  for (size_t i = 0; i < copy.size(); ++i) {
    if (copy[i] != '.') { continue; }
    if (num_dots++ == 0) { first_dot = i; }
  }

  if (num_dots == copy.size()) {
    // TODO better error message here
    return NumberParsingError::kNoDigits;
  }

  switch (num_dots) {
    case 0: return ParseIntInBase<Base>(copy);
    case 1: return ParseRealInBase<Base>(copy, first_dot);
    default: return NumberParsingError::kTooManyDots;
  }
}
}  // namespace

std::variant<nth::Integer, double, NumberParsingError> ParseNumber(
    std::string_view sv) {
  if (sv.size() > 1 and sv[0] == '0') {
    if (sv[1] == '.') { return ParseNumberInBase<10>(sv); }
    char base = sv[1];
    sv.remove_prefix(2);
    switch (base) {
      case 'b': return ParseNumberInBase<2>(sv);
      case 'o': return ParseNumberInBase<8>(sv);
      case 'd': return ParseNumberInBase<10>(sv);
      case 'x': return ParseNumberInBase<16>(sv);
      default: return NumberParsingError::kUnknownBase;
    }
  } else {
    return ParseNumberInBase<10>(sv);
  }
}
}  // namespace frontend
