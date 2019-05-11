#ifndef ICARUS_AST_AST_FWD_H
#define ICARUS_AST_AST_FWD_H

namespace ast {
#define ICARUS_AST_NODE_X(name) struct name;
#include "node.xmacro.h"
#undef ICARUS_AST_NODE_X
}  // namespace ast

#endif  // ICARUS_AST_AST_FWD_H
