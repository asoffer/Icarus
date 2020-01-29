#ifndef ICARUS_FRONTEND_LEX_NUMBERS_H
#define ICARUS_FRONTEND_LEX_NUMBERS_H

#include <string_view>
#include <variant>
#include "base/expected.h"

namespace frontend {

enum class NumberParsingError {
  kUnknownBase,
  kTooManyDots,
  kNoDigits,
  kInvalidDigit,
  kTooLarge,
};

std::variant<int64_t, double, NumberParsingError> ParseNumber(
    std::string_view sv);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_NUMBERS_H
