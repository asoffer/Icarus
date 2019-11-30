#ifndef ICARUS_COMPILER_EXTRACT_JUMPS_H
#define ICARUS_COMPILER_EXTRACT_JUMPS_H

#include <array>
#include <vector>

#include "ast/visitor.h"
#include "base/debug.h"

namespace compiler {

struct ExtractJumps : ast::Visitor<void()> {
  void Visit(ast::Node const *node) { ast::Visitor<void()>::Visit(node); }

#define ICARUS_AST_NODE_X(name) void Visit(ast::name const *node) final;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  enum class Kind { Return, Yield, Jump };
  absl::Span<ast::Node const *const> jumps(Kind k) const;

 private:
  std::array<std::vector<ast::Node const *>, 3> data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXTRACT_JUMPS_H
