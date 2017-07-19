#include "ast.h"

namespace AST {
void Terminal::contextualize(Scope *, std::vector<IR::Val> *) {}
void Identifier::contextualize(Scope *, std::vector<IR::Val> *) {}
void CodeBlock::contextualize(Scope *, std::vector<IR::Val> *) {}
void Jump::contextualize(Scope *, std::vector<IR::Val> *) {}

void Binop::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  lhs->contextualize(scope, args);
  rhs->contextualize(scope, args);
}

void Declaration::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  identifier->contextualize(scope, args);
  type_expr->contextualize(scope, args);
  init_val->contextualize(scope, args);
}

void Generic::contextualize(Scope *, std::vector<IR::Val> *) { NOT_YET; }
void InDecl::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  container->contextualize(scope, args);
}

void Statements::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &stmt : statements) { stmt->contextualize(scope, args); }
}

void Unop::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  if (op == Language::Operator::Ref) {
    assign_scope(scope);

    auto val = operand->EmitIR();
    args->push_back(val);
    args->push_back(IR::Val::Ref(this));
  } else {
    operand->contextualize(scope, args);
  }
}

void Access::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  operand->contextualize(scope, args);
}

void ChainOp::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->contextualize(scope, args); }
}

void CommaList::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->contextualize(scope, args); }
}

void ArrayLiteral::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &elem : elems) { elem->contextualize(scope, args); }
}

void ArrayType::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  length->contextualize(scope, args);
  data_type->contextualize(scope, args);
}

void Case::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &kv : key_vals) {
    kv.first->contextualize(scope, args);
    kv.second->contextualize(scope, args);
  }
}

void FunctionLiteral::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  return_type_expr->contextualize(scope, args);
  for (auto &input : inputs) { input->contextualize(scope, args); }
  statements->contextualize(fn_scope.get(), args);
}

void For::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  statements->contextualize(scope, args);
  for (auto &iter : iterators) { iter->contextualize(scope, args); }
}

void ScopeNode::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  expr->contextualize(scope, args);
  scope_expr->contextualize(scope, args);
  stmts->contextualize(scope, args);
}

void ScopeLiteral::contextualize(Scope *scope, std::vector<IR::Val> *args) {
  enter_fn->contextualize(scope, args);
  exit_fn->contextualize(scope, args);
}

////////////////////////////////////////////////
base::owned_ptr<Node> Terminal::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &) const {
  return copy_stub();
}
base::owned_ptr<Node> Identifier::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &) const {
  return copy_stub();
}
base::owned_ptr<Node> CodeBlock::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &) const {
  return copy_stub();
}
base::owned_ptr<Node> Jump::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &) const {
  return copy_stub();
}

base::owned_ptr<Node> Binop::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->lhs = base::move<Expression>(lhs->contextualize(replacements));
  result->rhs = base::move<Expression>(rhs->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> Declaration::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->identifier =
      base::move<Identifier>(identifier->contextualize(replacements));
  result->type_expr =
      base::move<Expression>(type_expr->contextualize(replacements));
  result->init_val =
      base::move<Expression>(init_val->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> Generic::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &) const {
  NOT_YET;
}

base::owned_ptr<Node> InDecl::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->container =
      base::move<Expression>(container->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> Statements::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->statements.reserve(statements.size());
  for (auto &stmt : statements) {
    result->statements.push_back(stmt->contextualize(replacements));
  }
  return result;
}

base::owned_ptr<Node> Unop::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  if (op == Language::Operator::Ref) {
    auto iter = replacements.find(this);
    ASSERT(iter != replacements.end(), "");
    auto terminal           = base::own(new Terminal);
    terminal->scope_        = scope_; // TODO Eh? Do I care?
    terminal->loc           = loc;
    terminal->precedence    = precedence;
    terminal->lvalue        = lvalue; // TODO????
    terminal->type          = iter->second.type;
    terminal->value         = iter->second;
    return terminal;
  } else {
    auto result = copy_stub();
    result->operand =
        base::move<Expression>(operand->contextualize(replacements));
    return result;
  }
}

base::owned_ptr<Node> Access::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->operand =
      base::move<Expression>(operand->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> ChainOp::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) {
    result->exprs.push_back(
        base::move<Expression>(expr->contextualize(replacements)));
  }
  return result;
}

base::owned_ptr<Node> CommaList::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) {
    result->exprs.push_back(
        base::move<Expression>(expr->contextualize(replacements)));
  }
  return result;
}

base::owned_ptr<Node> ArrayLiteral::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->elems.reserve(elems.size());
  for (const auto &elem : elems) {
    result->elems.push_back(
        base::move<Expression>(elem->contextualize(replacements)));
  }
  return result;
}

base::owned_ptr<Node> ArrayType::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result    = copy_stub();
  result->length = base::move<Expression>(length->contextualize(replacements));
  result->data_type =
      base::move<Expression>(data_type->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> Case::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  for (auto &kv : key_vals) {
    result->key_vals.push_back(std::make_pair(
        base::move<Expression>(kv.first->contextualize(replacements)),
        base::move<Expression>(kv.second->contextualize(replacements))));
  }
  return result;
}

base::owned_ptr<Node> FunctionLiteral::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->return_type_expr =
      base::move<Expression>(return_type_expr->contextualize(replacements));
  result->inputs.reserve(inputs.size());
  for (auto &input : inputs) {
    result->inputs.push_back(
        base::move<Declaration>(input->contextualize(replacements)));
  }
  result->statements =
      base::move<Statements>(statements->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> For::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->statements =
      base::move<Statements>(statements->contextualize(replacements));
  result->iterators.reserve(iterators.size());
  for (auto &iter : iterators) {
    result->iterators.push_back(
        base::move<InDecl>(iter->contextualize(replacements)));
  }
  return result;
}

base::owned_ptr<Node> ScopeNode::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result  = copy_stub();
  result->expr = base::move<Expression>(expr->contextualize(replacements));
  result->scope_expr =
      base::move<Expression>(scope_expr->contextualize(replacements));
  result->stmts = base::move<Statements>(stmts->contextualize(replacements));
  return result;
}

base::owned_ptr<Node> ScopeLiteral::contextualize(
    const std::unordered_map<const Expression *, IR::Val> &replacements) const {
  auto result = copy_stub();
  result->enter_fn =
      base::move<Declaration>(enter_fn->contextualize(replacements));
  result->exit_fn =
      base::move<Declaration>(exit_fn->contextualize(replacements));
  return result;
}

} // namespace AST
