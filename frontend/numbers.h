#ifndef ICARUS_FRONTEND_NUMBERS_H
#define ICARUS_FRONTEND_NUMBERS_H

#include <string_view>
#include <variant>

#include "base/types.h"

namespace frontend {
using NumberOrError = std::variant<i32, double, std::string_view>;
NumberOrError ParseNumber(std::string_view sv);
}  // namespace frontend

#endif  // ICARUS_FRONTEND_NUMBERS_H
