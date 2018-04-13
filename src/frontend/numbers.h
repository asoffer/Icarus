#ifndef ICARUS_FRONTEND_NUMBERS_H
#define ICARUS_FRONTEND_NUMBERS_H

#include <string>
#include <string_view>
#include <variant>

#include "base/types.h"

using NumberOrError = std::variant<i32, double, std::string>;
NumberOrError ParseNumber(std::string_view sv);

#endif  // ICARUS_FRONTEND_NUMBERS_H
