#ifndef ICARUS_SYNTAX_H
#define ICARUS_SYNTAX_H

#include <string>

#include "base/debug.h"
#include "frontend/tag.h"

namespace frontend {

// Represents syntax that the parser must understand but never makes it in to
// the syntax tree explicitly. For example, parentheses will dictate the shape
// of the tree, but no node in the tree actually stores these. Or the `.` which
// will cause the parser to generate ast::Access nodes, but will never be stored
// explicitly on the tree.
enum class Syntax : uint64_t {
#define SYNTAX_MACRO(name, symbol, tag) name,
#include "frontend/syntax.xmacro.h"
#undef SYNTAX_MACRO
};

inline Tag TagFrom(Syntax s) {
  switch (s) {
#define SYNTAX_MACRO(name, symbol, tag)                                        \
  case Syntax::name:                                                           \
    return tag;
#include "frontend/syntax.xmacro.h"
#undef SYNTAX_MACRO
  }
  UNREACHABLE();
}

inline std::string stringify(Syntax s) {
  switch (s) {
#define SYNTAX_MACRO(name, symbol, tag)                                        \
  case Syntax::name:                                                           \
    return symbol;
#include "frontend/syntax.xmacro.h"
#undef SYNTAX_MACRO
  }
  return "<<!>>";
}

}  // namespace frontend

#endif  // ICARUS_SYNTAX_H
