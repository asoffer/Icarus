#ifndef ICARUS_SYNTAX_H
#define ICARUS_SYNTAX_H

#include <string>

#include "base/debug.h"
#include "frontend/lex/tag.h"

namespace frontend {

// Represents syntax that the parser must understand but never makes it in to
// the syntax tree explicitly. For example, parentheses will dictate the shape
// of the tree, but no node in the tree actually stores these. Or the `.` which
// will cause the parser to generate ast::Access nodes, but will never be stored
// explicitly on the tree.
enum class Syntax : uint64_t {
#define SYNTAX_MACRO(name, symbol, tag) name,
#include "frontend/lex/syntax.xmacro.h"
#undef SYNTAX_MACRO
};

inline Tag TagFrom(Syntax s) {
  switch (s) {
#define SYNTAX_MACRO(name, symbol, tag)                                        \
  case Syntax::name:                                                           \
    return tag;
#include "frontend/lex/syntax.xmacro.h"
#undef SYNTAX_MACRO
  }
  UNREACHABLE();
}

inline std::ostream& operator<<(std::ostream& os, Syntax s) {
  switch (s) {
#define SYNTAX_MACRO(name, symbol, tag)                                        \
  case Syntax::name:                                                           \
    return os << symbol;
#include "frontend/lex/syntax.xmacro.h"
#undef SYNTAX_MACRO
  }
  return os << "<<!>>";
}

}  // namespace frontend

#endif  // ICARUS_SYNTAX_H
