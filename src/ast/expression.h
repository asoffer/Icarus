#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/node.h"

struct Context;

enum class Assign : char { Unset, Const, LVal, RVal };

namespace IR {
struct Val;
}  // namespace IR

namespace AST {
struct Expression : public Node {
  Expression(const TextSpan &span = TextSpan()) : Node(span) {}
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const                         = 0;
  virtual void assign_scope(Scope *scope)                               = 0;
  virtual void VerifyType(Context *ctx)                                 = 0;
  virtual void Validate(Context *ctx)                                   = 0;
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  virtual Expression *Clone() const                                     = 0;
  virtual std::vector<IR::Val> EmitIR(Context *)                        = 0;
  virtual std::vector<IR::Val> EmitLVal(Context *)                      = 0;

  virtual void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) = 0;

  // Use these two functions to verify that an identifier can be declared using
  // these expressions. We pass in a string representing the identifier being
  // declared to be used in error messages.
  //
  // VerifyTypeForDeclaration verifies that the expresison represents a type and
  // returns the type it represents (or Error if the type is invalid). An
  // expression could be invalid if it doesn't represent a type or it represents
  // void.
  type::Type *VerifyTypeForDeclaration(const std::string &id_tok, Context *ctx);

  // VerifyValueForDeclaration verifies that the expression's type can be used
  // for a declaration. In practice, it is typically used on initial values for
  // a declaration. That is, when we see "foo := bar", we verify that the type
  // of bar is valid. This function has the same return characteristics as
  // VerifyTypeForDeclaration. Specifically, it returns the type or Error if the
  // type is invalid.
  type::Type *VerifyValueForDeclaration(const std::string &id_tok);

  Assign lvalue          = Assign::Unset;
  const type::Type *type = nullptr;
};
} // namespace AST

#endif // ICARUS_AST_EXPRESSION_H
