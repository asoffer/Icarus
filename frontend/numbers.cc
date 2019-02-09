#include "frontend/numbers.h"

#include <algorithm>
#include "base/debug.h"

namespace frontend {
namespace {
template <int Base>
int64_t DigitInBase(char c);
template <>
int64_t DigitInBase<10>(char c) {
  return ('0' <= c && c <= '9') ? (c - '0') : -1;
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
  if ('A' <= c && c <= 'F') { return c - 'A' + 10; }
  if ('a' <= c && c <= 'f') { return c - 'a' + 10; }
  return -1;
}

template <int Base>
bool IntRepresentableInBase(const std::string &s);

template <>
bool IntRepresentableInBase<2>(const std::string &s) {
  return s.size() > 31;
}

template <>
bool IntRepresentableInBase<8>(const std::string &s) {
  return (s.size() > 11 || (s.size() == 11 && s[0] > '1'));
}

template <>
bool IntRepresentableInBase<10>(const std::string &s) {
  return s.size() > 10 || (s.size() == 10 && s > "2147483647");
}

template <>
bool IntRepresentableInBase<16>(const std::string &s) {
  return s.size() > 8 || (s.size() == 8 && s[0] > '7');
}

template <int Base>
NumberOrError ParseIntInBase(const std::string &s) {
  if (IntRepresentableInBase<Base>(s)) {
    return "Number is too large to fit in a 32-bit signed integer";
  }
  int64_t result = 0;
  for (char c : s) {
    int64_t digit = DigitInBase<Base>(c);
    if (digit == -1) { return "Number contains an invalid digit."; }
    result = result * Base + digit;
  }
  return result;
}

template <int Base>
NumberOrError ParseRealInBase(const std::string &s, int dot) {
  int64_t int_part = 0;
  for (int i = 0; i < dot; ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return "Number contains an invalid digit."; }
    int_part = int_part * Base + digit;
  }

  int64_t frac_part = 0;
  int64_t exp       = 1;
  for (size_t i = dot + 1; i < s.size(); ++i) {
    int64_t digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return "Number contains an invalid digit."; }
    exp *= Base;
    frac_part = frac_part * Base + digit;
  }
  return int_part + static_cast<double>(frac_part) / exp;
}

template <int Base>
NumberOrError ParseNumberInBase(std::string_view sv) {
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
    return "Need at least one digit in a number.";
  }

  switch (num_dots) {
    case 0: return ParseIntInBase<Base>(copy);
    case 1: return ParseRealInBase<Base>(copy, first_dot);
    default: return "Too many `.` characters in numeric literal.";
  }
  UNREACHABLE();
}
}  // namespace

NumberOrError ParseNumber(std::string_view sv) {
  if (sv.size() > 1 && sv[0] == '0') {
    if (sv[1] == '.') { return ParseNumberInBase<10>(sv); }
    char base = sv[1];
    sv.remove_prefix(2);
    switch (base) {
      case 'b': return ParseNumberInBase<2>(sv);
      case 'o': return ParseNumberInBase<8>(sv);
      case 'd': return ParseNumberInBase<10>(sv);
      case 'x': return ParseNumberInBase<16>(sv);
      default: return "Base must be one of `b`, `o`, `d`, or `x`.";
    }
  } else {
    return ParseNumberInBase<10>(sv);
  }
}
}  // namespace frontend
