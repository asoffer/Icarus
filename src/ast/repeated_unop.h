#ifndef ICARUS_AST_REPEATED_UNOP_H
#define ICARUS_AST_REPEATED_UNOP_H

#include "ast/comma_list.h"
#include "ast/dispatch.h"
#include "ast/node.h"
#include "frontend/operators.h"

namespace AST {
struct RepeatedUnop : public Node {
  ~RepeatedUnop() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  IR::Val EmitIR(Context *) override;

  RepeatedUnop *Clone() const override;

  Language::Operator op_;
  CommaList args_;
  std::vector<DispatchTable> dispatch_tables_;
};
} //
#endif  // ICARUS_AST_REPEATED_UNOP_H
