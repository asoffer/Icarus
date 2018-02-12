#ifndef ICARUS_AST_CODEBLOCK_H
#define ICARUS_AST_CODEBLOCK_H

#include <variant>

#include "expression.h"
#include "statements.h"

namespace AST {
struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  CodeBlock()                      = default;
  CodeBlock(const CodeBlock &)     = default;
  CodeBlock(CodeBlock &&) noexcept = default;
  CodeBlock &operator=(const CodeBlock &) = default;
  CodeBlock &operator=(CodeBlock &&) = default;

  static_assert(std::is_copy_constructible_v<Statements>);
  static_assert(std::is_copy_assignable_v<Statements>);
  static_assert(std::is_move_constructible_v<Statements>);
  static_assert(std::is_move_assignable_v<Statements>);

  std::variant<Statements, std::string> content_;

  CodeBlock *Clone() const override;
  virtual IR::Val EmitIR(const BoundConstants &);
};

inline bool operator==(const CodeBlock &lhs, const CodeBlock &rhs) {
  // TODO do this for real.
  return &lhs == &rhs;
}

inline bool operator<(const CodeBlock &lhs, const CodeBlock &rhs) {
  // TODO do this for real.
  return &lhs < &rhs;
}

inline bool operator>(const CodeBlock &lhs, const CodeBlock &rhs) {
  return rhs < lhs;
}
} // namespace AST
#endif // ICARUS_AST_CODEBLOCK_H
