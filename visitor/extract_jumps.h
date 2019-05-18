#ifndef ICARUS_VISITOR_EXTRACT_JUMPS_H
#define ICARUS_VISITOR_EXTRACT_JUMPS_H

#include <array>
#include <vector>

#include "ast/ast_fwd.h"

namespace visitor {

struct ExtractJumps {
#define ICARUS_AST_NODE_X(name) void operator()(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  enum class Kind { Return, Yield, Jump };
  std::vector<ast::Expression const *> const &exprs(Kind k) const;

 private:
  std::array<std::vector<ast::Expression const *>, 3> data_;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_EXTRACT_JUMPS_H
