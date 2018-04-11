#ifndef ICARUS_FRONTEND_NUMBERS_H
#define ICARUS_FRONTEND_NUMBERS_H

#include <sstream>
#include <cmath>
#include <vector>
#include "base/debug.h"

template <int Base>
bool RepresentableAsIntInBase(const std::vector<i32> &);

template <>
bool RepresentableAsIntInBase<2>(const std::vector<i32> &digits) {
  return digits.size() < 32;
}

template <>
bool RepresentableAsIntInBase<8>(const std::vector<i32> &digits) {
  return digits.size() < 11 || (digits.size() == 11 && digits[0] < 2);
}

template <>
bool RepresentableAsIntInBase<16>(const std::vector<i32> &digits) {
  return digits.size() < 8 || (digits.size() == 8 && digits[0] < 8);
}

template <>
bool RepresentableAsIntInBase<10>(const std::vector<i32> &digits) {
  return !(digits.size() > 10) &&
         (digits.size() < 10 ||
          digits < std::vector<i32>{2, 1, 4, 7, 4, 8, 3, 6, 4, 8});
}

template <int Base>
i32 RepresentationAsIntInBase(const std::vector<i32> &digits) {
  if (!RepresentableAsIntInBase<Base>(digits)) { return -1; }
  i32 result = 0;
  for (i32 digit : digits) {
    ASSERT_LT(digit, Base);
    result = result * Base + digit;
  }
  return result;
}

template <int Base>
double RepresentationAsRealInBase(const std::vector<i32> &, i32);

template <>
double RepresentationAsRealInBase<10>(const std::vector<i32> &digits,
                                      i32 dot_offset) {
  std::stringstream ss;
  // TODO this is an awful terrible hack and I feel lazy and gross
  for (i32 digit : digits) { ss << digit; }
  return std::stod(ss.str());
}

template <>
double RepresentationAsRealInBase<2>(const std::vector<i32> &digits,
                                     i32 dot_offset) {
  if (digits.size() > 52) { return NAN; }
  if (dot_offset > 1023 || dot_offset < -1022) { return NAN; }
  NOT_YET();
}

template <>
double RepresentationAsRealInBase<16>(const std::vector<i32> &digits,
                                      i32 dot_offset) {
  if (digits.size() > 13) { return NAN; }
  i32 exponent = dot_offset * 4 - 3;
  if (dot_offset >= 0) { exponent += 4; }
  if (digits[0] >= 8) { ++exponent; }
  if (digits[0] >= 4) { ++exponent; }
  if (digits[0] >= 2) { ++exponent; }

  if (exponent > 1023 || exponent < -1022) { return NAN; }
  NOT_YET();
}

template <>
double RepresentationAsRealInBase<8>(const std::vector<i32> &digits,
                                     i32 dot_offset) {
  if (digits.size() > 13) { return NAN; }
  i32 exponent = dot_offset * 3 - 2;
  if (dot_offset >= 0) { exponent += 3; }
  if (digits[0] >= 2) { ++exponent; }
  if (digits[0] >= 4) { ++exponent; }

  if (exponent > 1023 || exponent < -1022) { return NAN; }
  NOT_YET();
}

#endif  // ICARUS_FRONTEND_NUMBERS_H
