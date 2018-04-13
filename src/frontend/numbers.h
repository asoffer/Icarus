#ifndef ICARUS_FRONTEND_NUMBERS_H
#define ICARUS_FRONTEND_NUMBERS_H

#include <string>
#include <string_view>
#include <variant>

#include "base/types.h"

template <int Base> inline i32 DigitInBase(char c);
template <> inline i32 DigitInBase<10>(char c) {
  return ('0' <= c && c <= '9') ? (c - '0') : -1;
}
template <> inline i32 DigitInBase<2>(char c) {
  return ((c | 1) == '1') ? (c - '0') : -1;
}
template <> inline i32 DigitInBase<8>(char c) {
  return ((c | 7) == '7') ? (c - '0') : -1;
}
template <> inline i32 DigitInBase<16>(char c) {
  int digit = DigitInBase<10>(c);
  if (digit != -1) { return digit; }
  if ('A' <= c && c <= 'F') { return c - 'A' + 10; }
  if ('a' <= c && c <= 'f') { return c - 'a' + 10; }
  return -1;
}

using NumberOrError = std::variant<i32, double, std::string>;

NumberOrError ParseNumber(std::string_view sv);

#endif  // ICARUS_FRONTEND_NUMBERS_H
