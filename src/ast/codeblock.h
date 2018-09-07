#ifndef ICARUS_AST_CODEBLOCK_H
#define ICARUS_AST_CODEBLOCK_H

#include <variant>

#include "expression.h"
#include "statements.h"

namespace type {
extern Type const *Code;
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
  CodeBlock *Clone() const { return new CodeBlock(*this); }

  void VerifyType(Context *) override {}
  virtual void Validate(Context *) override {}
  void SaveReferences(Scope *, base::vector<IR::Val> *) override {}
  void contextualize(
      const Node *,
      const base::unordered_map<const Expression *, IR::Val> &) override {}

  void ExtractReturns(base::vector<const Expression *> *) const override {}

  std::variant<Statements, std::string> content_;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Val> EmitLVal(Context *) override;
};

bool operator==(const CodeBlock &lhs, const CodeBlock &rhs);
// TODO delete these. they're only around so we can put them in maps which we
// shouldn't be doing anyway.
bool operator<(const CodeBlock &lhs, const CodeBlock &rhs);
bool operator>(const CodeBlock &lhs, const CodeBlock &rhs);
} // namespace AST

#endif // ICARUS_AST_CODEBLOCK_H
