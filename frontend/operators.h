#ifndef ICARUS_OPERATORS_H
#define ICARUS_OPERATORS_H

#include <unordered_map>
#include <string>

#include "base/debug.h"

namespace frontend {
enum class Operator : uint64_t {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc) name,
#include "frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
};

inline std::string stringify(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  case Operator::name:                                                         \
    return symbol;
#include "frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  UNREACHABLE();
}

}  // namespace frontend

#endif  // ICARUS_OPERATORS_H
