#ifndef ICARUS_VISITOR_DUMP_AST_H
#define ICARUS_VISITOR_DUMP_AST_H

#include <string>

#include "ast/ast_fwd.h"

namespace visitor {

struct DumpAst {
  static std::string ToString(ast::Node const *);

  constexpr DumpAst(std::string *out) : out_(out) {}
  void operator()(ast::Node const *node) { out_->append("[unknown node]"); }
#define ICARUS_AST_NODE_X(name) void operator()(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  std::string indent() const { return std::string(2 * indentation_, ' '); }

  std::string *out_   = nullptr;
  size_t indentation_ = 0;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_DUMP_AST_H
