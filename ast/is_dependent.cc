#include "ast/ast.h"

namespace ast {
namespace {

template <typename NodeType>
bool AnyIsDependent(absl::Span<NodeType const> span) {
  for (auto const &n : span) {
    if (n.IsDependent()) { return true; }
  }
  return false;
}

template <typename NodeType>
bool AnyIsDependent(base::PtrSpan<NodeType const> span) {
  for (auto const *p : span) {
    if (p->IsDependent()) { return true; }
  }
  return false;
}

}  // namespace

bool Access::IsDependent() const { return operand()->IsDependent(); }

bool ArgumentType::IsDependent() const { return true; }

bool ArrayLiteral::IsDependent() const { return AnyIsDependent(elems()); }

bool ArrayType::IsDependent() const {
  if (data_type()->IsDependent()) { return true; }
  return AnyIsDependent(lengths());
}

bool Assignment::IsDependent() const {
  return AnyIsDependent(lhs()) or AnyIsDependent(rhs());
}

bool InterfaceLiteral::IsDependent() const {
  for (auto const &[name, expr] : entries()) {
    if (name->IsDependent() or expr->IsDependent()) { return true; }
  }
  return false;
}

bool BinaryOperator::IsDependent() const {
  return lhs()->IsDependent() or rhs()->IsDependent();
}

bool BlockLiteral::IsDependent() const {
  return AnyIsDependent(before()) or AnyIsDependent(after());
}

bool BlockNode::IsDependent() const { return AnyIsDependent(stmts()); }

bool Jump::IsDependent() const {
  if (auto const *s = state(); s and s->IsDependent()) { return true; }
  return AnyIsDependent(stmts());
}

bool BuiltinFn::IsDependent() const { return false; }

bool Call::IsDependent() const {
  if (callee()->IsDependent()) { return true; }

  for (auto const &[name, expr] : arguments()) {
    if (expr->IsDependent()) { return true; }
  }

  return false;
}

bool Cast::IsDependent() const {
  return expr()->IsDependent() or type()->IsDependent();
}

bool ComparisonOperator::IsDependent() const { return AnyIsDependent(exprs()); }

bool Declaration::IsDependent() const {
  if (auto *t = type_expr(); t and t->IsDependent()) { return true; }
  if (auto *i = init_val(); i and i->IsDependent()) { return true; }
  return false;
}

bool DesignatedInitializer::IsDependent() const {
  return type()->IsDependent() or AnyIsDependent(assignments());
}

bool EnumLiteral::IsDependent() const {
  for (auto const &[name, value] : specified_values()) {
    if (value->IsDependent()) { return true; }
  }
  return false;
}

bool FunctionLiteral::IsDependent() const {
  // TODO
  return false;
}

bool FunctionType::IsDependent() const {
  // TODO
  return false;
}

bool Identifier::IsDependent() const { return false; }

bool Import::IsDependent() const { return operand()->IsDependent(); }

bool Index::IsDependent() const {
  return lhs()->IsDependent() or rhs()->IsDependent();
}

bool ConditionalGoto::IsDependent() const {
  // TODO: Jump options could also be dependent.
  return condition()->IsDependent();
}

bool UnconditionalGoto::IsDependent() const {
  // TODO: Jump options could also be dependent.
  return false;
}

bool Label::IsDependent() const { return false; }

bool ReturnStmt::IsDependent() const { return AnyIsDependent(exprs()); }

bool YieldStmt::IsDependent() const { return AnyIsDependent(exprs()); }

bool ScopeLiteral::IsDependent() const {
  if (auto const *s = state_type(); s and s->IsDependent()) { return true; }
  return AnyIsDependent(decls());
}

bool ScopeNode::IsDependent() const {
  // TODO: Scope node arguments
  return name()->IsDependent() or label()->IsDependent() or
         AnyIsDependent(blocks());
}

bool ShortFunctionLiteral::IsDependent() const {
  // TODO
  return false;
}

bool SliceType::IsDependent() const { return data_type()->IsDependent(); }

bool StructLiteral::IsDependent() const {
  // TODO
  return false;
}

bool ParameterizedStructLiteral::IsDependent() const {
  // TODO
  return false;
}

bool Terminal::IsDependent() const { return false; }

bool UnaryOperator::IsDependent() const { return operand()->IsDependent(); }

}  // namespace ast
