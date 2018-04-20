#include "ast/ast.h"

#include <set>
namespace AST {
void Unop::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  operand->ExtractReturnTypes(types);
  if (op == Language::Operator::Return) {
    if (operand->is<CommaList>()) {
      std::vector<const type::Type *> type_vec;
      type_vec.reserve(operand->as<CommaList>().exprs.size());
      for (const auto &expr : operand->as<CommaList>().exprs) {
        type_vec.push_back(expr->type);
      }
      types->insert(std::move(type_vec));
    } else {
      types->insert({operand->type});
    }
  }
}

void Import::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}

void Access::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  operand->ExtractReturnTypes(types);
}

void Identifier::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}
void Terminal::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}

void ArrayType::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  // TODO length needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length->ExtractReturnTypes(types);
  data_type->ExtractReturnTypes(types);
}

void For::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &iter : iterators) { iter->ExtractReturnTypes(types); }
  statements->ExtractReturnTypes(types);
}

void ArrayLiteral::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &el : elems) { el->ExtractReturnTypes(types); }
}

void Binop::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  lhs->ExtractReturnTypes(types);
  rhs->ExtractReturnTypes(types);
}

void InDecl::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  identifier->ExtractReturnTypes(types);
  container->ExtractReturnTypes(types);
}

void Declaration::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  identifier->ExtractReturnTypes(types);
  if (type_expr) { type_expr->ExtractReturnTypes(types); }
  if (init_val) { init_val->ExtractReturnTypes(types); }
}

void ChainOp::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &expr : exprs) { expr->ExtractReturnTypes(types); }
}

void CommaList::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &expr : exprs) { expr->ExtractReturnTypes(types); }
}

void Statements::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &stmt : content_) { stmt->ExtractReturnTypes(types); }
}

void GenericFunctionLiteral::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  FunctionLiteral::ExtractReturnTypes(types);
}

void FunctionLiteral::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &in : inputs) { in->ExtractReturnTypes(types); }
  for (auto &out : outputs) { out->ExtractReturnTypes(types); }
}

void Jump::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}
void CodeBlock::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}

void Call::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  fn_->ExtractReturnTypes(types);
  for (const auto &val : args_.pos_) { val->ExtractReturnTypes(types); }
  for (const auto & [ key, val ] : args_.named_) {
    val->ExtractReturnTypes(types);
  }
}

void ScopeNode::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  scope_expr->ExtractReturnTypes(types);
  if (expr) { expr->ExtractReturnTypes(types); }
  stmts->ExtractReturnTypes(types);
}

void ScopeLiteral::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  if (enter_fn) { enter_fn->ExtractReturnTypes(types); }
  if (exit_fn) { exit_fn->ExtractReturnTypes(types); }
}

void StructLiteral::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *types) const {
  for (auto &f : fields_) { f->ExtractReturnTypes(types); }
}

void Hole::ExtractReturnTypes(
    std::set<std::vector<const type::Type *>> *) const {}
}  // namespace AST
