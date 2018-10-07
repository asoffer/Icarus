#ifndef ICARUS_OPERATORS_H
#define ICARUS_OPERATORS_H

namespace Language {
enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
};
}  // namespace Language

#endif  // ICARUS_OPERATORS_H
