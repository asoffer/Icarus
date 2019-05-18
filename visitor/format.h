#ifndef ICARUS_VISITOR_FORMAT_H
#define ICARUS_VISITOR_FORMAT_H

#include "ast/ast_fwd.h"

namespace visitor {

struct Format {
  void operator()(ast::Node const *node) const;

  // #define ICARUS_AST_NODE_X(name)
  // //   void operator()(ast::name *node, core::Scope *scope);
  // #include "ast/node.xmacro.h"
  // #undef ICARUS_AST_NODE_X
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_FORMAT_H
