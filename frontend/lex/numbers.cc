#include "frontend/lex/numbers.h"

#include <algorithm>

namespace frontend {
namespace {
template <int Base>
int64_t DigitInBase(char c);
template <>
int64_t DigitInBase<10>(char c) {
  return ('0' <= c and c <= '9') ? (c - '0') : -1;
}
template <>
int64_t DigitInBase<2>(char c) {
  return ((c | 1) == '1') ? (c - '0') : -1;
}
template <>
int64_t DigitInBase<8>(char c) {
  return ((c | 7) == '7') ? (c - '0') : -1;
}
template <>
int64_t DigitInBase<16>(char c) {
  int digit = DigitInBase<10>(c);
  if (digit != -1) { return digit; }
  if ('A' <= c and c <= 'F') { return c - 'A' + 10; }
  if ('a' <= c and c <= 'f') { return c - 'a' + 10; }
  return -1;
}

template <int Base>
bool IntRepresentableInBase(std::string const &s);

template <>
bool IntRepresentableInBase<2>(std::string const &s) {
  return s.size() > 31;
}

template <>
bool IntRepresentableInBase<8>(std::string const &s) {
  return (s.size() > 11 or (s.size() == 11 and s[0] > '1'));
}

template <>
bool IntRepresentableInBase<10>(std::string const &s) {
  return s.size() > 10 or (s.size() == 10 and s > "2147483647");
}

template <>
bool IntRepresentableInBase<16>(std::string const &s) {
  return s.size() > 8 or (s.size() == 8 and s[0] > '7');
}

template <int Base>
base::expected<std::variant<int64_t, double>> ParseIntInBase(
    std::string const &s) {
  if (IntRepresentableInBase<Base>(s)) {
    return base::unexpected(
        "Number is too large to fit in a 32-bit signed integer");
  }
  int64_t result = 0;
  for (char c : s) {
    int64_t digit = DigitInBase<Base>(c);
    if (digit == -1) {
      return base::unexpected("Number contains an invalid digit.");
    }
    result = result * Base + digit;
  }
  return base::expected<std::variant<int64_t, double>>{result};
}

template <int Base>
base::expected<std::variant<int64_t, double>> ParseRealInBase(
    std::string const &s, int dot) {
  int64_t int_part = 0;
  for (int i = 0; i < dot; ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) {
      return base::unexpected("Number contains an invalid digit.");
    }
    int_part = int_part * Base + digit;
  }

  int64_t frac_part = 0;
  int64_t exp       = 1;
  for (size_t i = dot + 1; i < s.size(); ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) {
      return base::unexpected("Number contains an invalid digit.");
    }
    exp *= Base;
    frac_part = frac_part * Base + digit;
  }
  return base::expected<std::variant<int64_t, double>>{
      int_part + static_cast<double>(frac_part) / exp};
}

template <int Base>
base::expected<std::variant<int64_t, double>> ParseNumberInBase(
    std::string_view sv) {
  std::string copy(sv.data(), sv.size());
  copy.erase(
      std::remove_if(copy.begin(), copy.end(), [](char c) { return c == '_'; }),
      copy.end());

  int first_dot   = -1;
  size_t num_dots = 0;
  for (size_t i = 0; i < copy.size(); ++i) {
    if (copy[i] != '.') { continue; }
    if (num_dots++ == 0) { first_dot = i; }
  }

  if (num_dots == copy.size()) {
    // TODO better error message here
    return base::unexpected("Need at least one digit in a number.");
  }

  switch (num_dots) {
    case 0: return ParseIntInBase<Base>(copy);
    case 1: return ParseRealInBase<Base>(copy, first_dot);
    default:
      return base::unexpected("Too many `.` characters in numeric literal.");
  }
}
}  // namespace

base::expected<std::variant<int64_t, double>> ParseNumber(std::string_view sv) {
  if (sv.size() > 1 and sv[0] == '0') {
    if (sv[1] == '.') { return ParseNumberInBase<10>(sv); }
    char base = sv[1];
    sv.remove_prefix(2);
    switch (base) {
      case 'b': return ParseNumberInBase<2>(sv);
      case 'o': return ParseNumberInBase<8>(sv);
      case 'd': return ParseNumberInBase<10>(sv);
      case 'x': return ParseNumberInBase<16>(sv);
      default:
        return base::unexpected("Base must be one of `b`, `o`, `d`, or `x`.");
    }
  } else {
    return ParseNumberInBase<10>(sv);
  }
}
}  // namespace frontend
