#ifndef ICARUS_AST_METHODS_DUMP_H
#define ICARUS_AST_METHODS_DUMP_H

#include <string>

#include "ast/ast_fwd.h"

namespace frontend {
struct Token;
}  // namespace frontend

namespace ast {

struct Dump {
  static std::string ToString(ast::Node const *);

  constexpr Dump(std::string *out) : out_(out) {}
  void operator()(ast::Node const *node) { out_->append("[unknown node]"); }
#define ICARUS_AST_NODE_X(name) void operator()(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  void operator()(frontend::Token const *node);

  std::string indent() const { return std::string(2 * indentation_, ' '); }

  std::string *out_   = nullptr;
  size_t indentation_ = 0;
};

}  // namespace ast

#endif  // ICARUS_AST_METHODS_DUMP_H
