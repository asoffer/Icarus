#ifndef ICARUS_FRONTEND_LEX_NUMBERS_H
#define ICARUS_FRONTEND_LEX_NUMBERS_H

#include <string_view>
#include <variant>
#include "base/expected.h"

namespace frontend {

base::expected<std::variant<int64_t, double>> ParseNumber(std::string_view sv);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_NUMBERS_H
