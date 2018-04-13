#include "frontend/numbers.h"

#include <algorithm>
#include "base/debug.h"

template <int Base>
static inline bool IntRepresentableInBase(const std::string &s);

template <>
inline bool IntRepresentableInBase<2>(const std::string &s) {
  return s.size() > 31;
}

template <>
inline bool IntRepresentableInBase<8>(const std::string &s) {
  return (s.size() > 11 || (s.size() == 11 && s[0] > '1'));
}

template <>
inline bool IntRepresentableInBase<10>(const std::string &s) {
  return s.size() > 10 || (s.size() == 10 && s > "2147483647");
}

template <>
inline bool IntRepresentableInBase<16>(const std::string &s) {
  return s.size() > 8 || (s.size() == 8 && s[0] > '7');
}

template <int Base>
NumberOrError ParseIntInBase(const std::string &s) {
  if (IntRepresentableInBase<Base>(s)) {
    return "Number is too large to fit in a 32-bit signed integer";
  }
  i32 result = 0;
  for (char c : s) {
    i32 digit = DigitInBase<Base>(c);
    if (digit == -1) { return "Number contains an invalid digit."; }
    result = result * Base + digit;
  }
  return result;
}

template <int Base>
NumberOrError ParseRealInBase(const std::string &s, int dot) {
  i64 int_part = 0;
  for (int i = 0; i < dot; ++i) {
    i32 digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return "Number contains an invalid digit."; }
    int_part = int_part * Base + digit;
  }

  i64 frac_part = 0;
  i64 exp       = 1;
  for (size_t i = dot + 1; i < s.size(); ++i) {
    i32 digit = DigitInBase<Base>(s[i]);
    if (digit == -1) { return "Number contains an invalid digit."; }
    exp *= Base;
    frac_part = frac_part * Base + digit;
  }
  return int_part + static_cast<double>(frac_part) / exp;
}

template <int Base>
NumberOrError ParseNumberInBase(std::string_view sv) {
  std::string copy(sv.data());
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
    default: NOT_YET();
  }
  UNREACHABLE();
}

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
      default: NOT_YET(base);
    }
  } else {
    return ParseNumberInBase<10>(sv);
  }
}
