#ifndef ICARUS_OPERATORS_H
#define ICARUS_OPERATORS_H

#include <ostream>

#include "frontend/lex/tag.h"

namespace frontend {

enum class Operator : uint64_t {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc) name,
#include "frontend/lex/operators.xmacro.h"
#undef OPERATOR_MACRO
};

inline std::ostream& operator<<(std::ostream& os, Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  case Operator::name:                                                         \
    return os << symbol;
#include "frontend/lex/operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  return os << "<<!>>";
}

inline Tag TagFrom(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  case Operator::name:                                                         \
    return tag;
#include "frontend/lex/operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  UNREACHABLE();
}

}  // namespace frontend

#endif  // ICARUS_OPERATORS_H
