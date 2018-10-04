#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/node.h"
#include "ir/register.h"

struct Context;

namespace IR {
struct Val;
}  // namespace IR

namespace AST {
struct Expression : public Node {
  Expression(const TextSpan &span = TextSpan()) : Node(span) {}
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const                          = 0;
  virtual void assign_scope(Scope *scope)                                = 0;
  virtual type::Type const *VerifyType(Context *ctx)                     = 0;
  virtual void Validate(Context *ctx)                                    = 0;
  virtual void SaveReferences(Scope *scope, base::vector<IR::Val> *args) = 0;
  virtual Expression *Clone() const                                      = 0;
  virtual base::vector<IR::Val> EmitIR(Context *)                        = 0;
  virtual base::vector<IR::Register> EmitLVal(Context *)                 = 0;

  virtual void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) = 0;
};
} // namespace AST

#endif // ICARUS_AST_EXPRESSION_H
