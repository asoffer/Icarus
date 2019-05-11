#include "ast/block_literal.h"

#include "core/scope.h"
#include "ir/block.h"
#include "misc/context.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ast {
BlockLiteral::BlockLiteral(bool required) : required_(required) {}

std::string BlockLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "block" << (required_ ? "" : "?") << " {\n";
  for (auto const &b : before_) {
    ss << std::string(2 * (n + 1), ' ') << b.to_string(n + 1) << "\n";
  }
  for (auto const &a : after_) {
    ss << std::string(2 * (n + 1), ' ') << a.to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

ir::Results BlockLiteral::EmitIr(Context *ctx) {
  ir::BlockSequence seq;
  seq.append(ir::Block(this));
  return ir::Results{seq};
}

}  // namespace ast
