#ifndef ICARUS_AST_IMPORT_H
#define ICARUS_AST_IMPORT_H

#include <memory>
#include <optional>

#include "ast/literal.h"
#include "misc/module.h"

namespace ast {
struct Import : public Literal {
  Import(std::unique_ptr<Expression> expr) : operand_(std::move(expr)) {}
  ~Import() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override {
    operand_->DependentDecls(g, d);
  }
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override;

  // TODO optimization: if the operand_ is a string literal, schedule it
  // immediately.
  // TODO this could definitely be dependent on context/bound constants:
  // f ::= (version :: string) -> {
  //   lib ::= import "library." + version + ".ic"
  //   return lib.some_function()
  // }
  mutable PendingModule module_;
  std::unique_ptr<Expression> operand_;
};
}  // namespace ast
#endif  // ICARUS_AST_IMPORT_H
