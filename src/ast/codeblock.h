#ifndef ICARUS_AST_CODEBLOCK_H
#define ICARUS_AST_CODEBLOCK_H

#include <variant>

#include "expression.h"
#include "statements.h"

extern type::Type* Code;

namespace AST {
struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  CodeBlock() {
    lvalue = Assign::Const;
    type   = Code;
  }
  CodeBlock(std::string s) : content_(std::move(s)) {
    lvalue = Assign::RVal;
    type = Code;
  }

  CodeBlock(const CodeBlock &)     = default;
  CodeBlock(CodeBlock &&) noexcept = default;
  CodeBlock &operator=(const CodeBlock &) = default;
  CodeBlock &operator=(CodeBlock &&) = default;

  std::variant<Statements, std::string> content_;

  CodeBlock *Clone() const override;
  virtual IR::Val EmitIR(Context *);
};

inline bool operator==(const CodeBlock &lhs, const CodeBlock &rhs) {
  if (auto* lhs_stmts = std::get_if<Statements>(&lhs.content_)) {
    if (auto *rhs_stmts = std::get_if<Statements>(&rhs.content_)) {
      return std::get<Statements>(lhs.content_).content_ ==
             std::get<Statements>(rhs.content_).content_;
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
      return std::get<Statements>(lhs.content_).content_ <
             std::get<Statements>(rhs.content_).content_;
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
