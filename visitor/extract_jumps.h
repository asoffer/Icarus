#ifndef ICARUS_VISITOR_EXTRACT_JUMPS_H
#define ICARUS_VISITOR_EXTRACT_JUMPS_H

#include <array>
#include <vector>

#include "ast/ast_fwd.h"
#include "base/debug.h"

namespace visitor {

struct ExtractJumps {
  void operator()(ast::Node const *node) { UNREACHABLE(); }
#define ICARUS_AST_NODE_X(name) void operator()(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  enum class Kind { Return, Yield, Jump };
  absl::Span<ast::Node const *const> jumps(Kind k) const;

 private:
  std::array<std::vector<ast::Node const *>, 3> data_;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_EXTRACT_JUMPS_H
