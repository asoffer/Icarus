#ifndef ICARUS_AST_CODEBLOCK_H
#define ICARUS_AST_CODEBLOCK_H

#include <variant>

#include "expression.h"
#include "statements.h"

namespace type {
extern Type *Code;
}  // namespace type

namespace AST {
struct CodeBlock : public Expression {
  CodeBlock() {
    lvalue = Assign::Const;
    type   = type::Code;
  }
  CodeBlock(std::string_view s) : CodeBlock() { content_ = std::string(s); }

  CodeBlock(const CodeBlock &)     = default;
  CodeBlock(CodeBlock &&) noexcept = default;
  CodeBlock &operator=(const CodeBlock &) = default;
  CodeBlock &operator=(CodeBlock &&) noexcept = default;
  ~CodeBlock() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override {}
  void ClearIdDecls() override { stage_range_ = StageRange{}; }
  CodeBlock *Clone() const { return new CodeBlock(*this); }

  void VerifyType(Context *) override {}
  virtual void Validate(Context *) override {}
  void SaveReferences(Scope *, std::vector<IR::Val> *) override {}
  void contextualize(
      const Node *,
      const std::unordered_map<const Expression *, IR::Val> &) override {}

  void ExtractReturns(std::vector<const Expression *> *) const override {}

  std::variant<Statements, std::string> content_;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;
};

bool operator==(const CodeBlock &lhs, const CodeBlock &rhs);
bool operator<(const CodeBlock &lhs, const CodeBlock &rhs);
bool operator>(const CodeBlock &lhs, const CodeBlock &rhs);
} // namespace AST

#endif // ICARUS_AST_CODEBLOCK_H
