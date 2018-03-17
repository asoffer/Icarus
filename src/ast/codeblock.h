#ifndef ICARUS_AST_CODEBLOCK_H
#define ICARUS_AST_CODEBLOCK_H

#include <variant>

#include "expression.h"
#include "statements.h"

extern type::Type *Code;
namespace AST {
struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  CodeBlock();
  CodeBlock(std::string s);

  CodeBlock(const CodeBlock &)     = default;
  CodeBlock(CodeBlock &&) noexcept = default;
  CodeBlock &operator=(const CodeBlock &) = default;
  CodeBlock &operator=(CodeBlock &&) = default;

  std::variant<Statements, std::string> content_;

  CodeBlock *Clone() const override;
  IR::Val EmitIR(Context *) override;
};

inline bool operator==(const CodeBlock &lhs, const CodeBlock &rhs) {
  if (auto* lhs_stmts = std::get_if<Statements>(&lhs.content_)) {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return lhs_stmts->content_ == rhs_stmts->content_;
    } else {
      return false;
    }
  } else {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return false;
    } else {
      return std::get<std::string>(lhs.content_) ==
             std::get<std::string>(rhs.content_);
    }
  }
}

inline bool operator<(const CodeBlock &lhs, const CodeBlock &rhs) {
  if (auto* lhs_stmts = std::get_if<Statements>(&lhs.content_)) {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return lhs_stmts->content_ < rhs_stmts->content_;
    } else {
      return true;
    }
  } else {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return false;
    } else {
      return std::get<std::string>(lhs.content_) <
             std::get<std::string>(rhs.content_);
    }
  }
}

inline bool operator>(const CodeBlock &lhs, const CodeBlock &rhs) {
  return rhs < lhs;
}
} // namespace AST
#endif // ICARUS_AST_CODEBLOCK_H
