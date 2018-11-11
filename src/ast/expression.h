#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/node.h"
#include "ir/register.h"

struct Context;

namespace ir {
struct Val;
}  // namespace ir

namespace ast {
struct Expression : public Node {
  Expression(const TextSpan &span = TextSpan()) : Node(span) {}
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const                          = 0;
  virtual void assign_scope(Scope *scope)                                = 0;
  virtual type::Type const *VerifyType(Context *ctx)                     = 0;
  virtual void Validate(Context *ctx)                                    = 0;
  virtual base::vector<ir::Val> EmitIR(Context *)                        = 0;
  virtual base::vector<ir::Register> EmitLVal(Context *)                 = 0;
};
}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H
