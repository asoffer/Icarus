#include "ast/ast.h"

namespace AST {
void Unop::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  operand->ExtractReturnTypes(types);
  if (op == Language::Operator::Return) { types->insert(operand->type); }
}

void Import::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *) const {}

void Access::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  operand->ExtractReturnTypes(types);
}

void Identifier::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *) const {}
void Terminal::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *) const {}

void ArrayType::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  // TODO length needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length->ExtractReturnTypes(types);
  data_type->ExtractReturnTypes(types);
}

void For::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &iter : iterators) { iter->ExtractReturnTypes(types); }
  statements->ExtractReturnTypes(types);
}

void ArrayLiteral::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &el : elems) { el->ExtractReturnTypes(types); }
}

void Binop::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  lhs->ExtractReturnTypes(types);
  rhs->ExtractReturnTypes(types);
}

void InDecl::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  identifier->ExtractReturnTypes(types);
  container->ExtractReturnTypes(types);
}

void Declaration::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  identifier->ExtractReturnTypes(types);
  if (type_expr) { type_expr->ExtractReturnTypes(types); }
  if (init_val) { init_val->ExtractReturnTypes(types); }
}

void ChainOp::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &expr : exprs) { expr->ExtractReturnTypes(types); }
}

void CommaList::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &expr : exprs) { expr->ExtractReturnTypes(types); }
}

void Statements::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &stmt : content_) { stmt->ExtractReturnTypes(types); }
}

void GenericFunctionLiteral::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  FunctionLiteral::ExtractReturnTypes(types);
}

void FunctionLiteral::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &in : inputs) { in->ExtractReturnTypes(types); }
  for (auto &out : outputs) { out->ExtractReturnTypes(types); }
}

void Jump::ExtractReturnTypes(std::unordered_set<const type::Type *> *) const {}
void CodeBlock::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *) const {}

void Call::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  fn_->ExtractReturnTypes(types);
  for (const auto &val : args_.pos_) { val->ExtractReturnTypes(types); }
  for (const auto & [ key, val ] : args_.named_) {
    val->ExtractReturnTypes(types);
  }
}

void ScopeNode::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  scope_expr->ExtractReturnTypes(types);
  if (expr) { expr->ExtractReturnTypes(types); }
  stmts->ExtractReturnTypes(types);
}

void ScopeLiteral::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  if (enter_fn) { enter_fn->ExtractReturnTypes(types); }
  if (exit_fn) { exit_fn->ExtractReturnTypes(types); }
}

void StructLiteral::ExtractReturnTypes(
    std::unordered_set<const type::Type *> *types) const {
  for (auto &f : fields_) { f->ExtractReturnTypes(types); }
}

void Hole::ExtractReturnTypes(std::unordered_set<const type::Type *> *) const {}
} // namespace AST
