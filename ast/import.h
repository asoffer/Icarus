#ifndef ICARUS_AST_IMPORT_H
#define ICARUS_AST_IMPORT_H

#include <memory>
#include <optional>

#include "ast/expression.h"
#include "misc/module.h"

namespace ast {
struct Import : public Expression {
  Import(std::unique_ptr<Expression> expr) : operand_(std::move(expr)) {}
  ~Import() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    return "import " + operand_->to_string(n);
  }

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
