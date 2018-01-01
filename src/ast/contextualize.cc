#include "ast.h"

namespace AST {
void Terminal::SaveReferences(Scope *, std::vector<IR::Val> *) {}
void Identifier::SaveReferences(Scope *, std::vector<IR::Val> *) {}
void CodeBlock::SaveReferences(Scope *, std::vector<IR::Val> *) {}
void Jump::SaveReferences(Scope *, std::vector<IR::Val> *) {}
void Hole::SaveReferences(Scope *, std::vector<IR::Val> *) {}

void Binop::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  lhs->SaveReferences(scope, args);
  rhs->SaveReferences(scope, args);
}

void Declaration::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  identifier->SaveReferences(scope, args);
  if (type_expr) { type_expr->SaveReferences(scope, args); }
  if (init_val) { init_val->SaveReferences(scope, args); }
}

void InDecl::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  container->SaveReferences(scope, args);
}

void Call::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &pos : pos_) { pos->SaveReferences(scope, args); }
  for (auto & [ name, expr ] : named_) { expr->SaveReferences(scope, args); }
}

void Statements::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &stmt : statements) { stmt->SaveReferences(scope, args); }
}

void Unop::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  if (op == Language::Operator::Ref) {
    assign_scope(scope);

    auto val = operand->EmitIR(IR::Cmd::Kind::Exec);
    args->push_back(val);
    args->push_back(IR::Val::Ref(this));
  } else {
    operand->SaveReferences(scope, args);
  }
}

void Access::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  operand->SaveReferences(scope, args);
}

void ChainOp::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}

void CommaList::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}

void ArrayLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &elem : elems) { elem->SaveReferences(scope, args); }
}

void ArrayType::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  length->SaveReferences(scope, args);
  data_type->SaveReferences(scope, args);
}

void Case::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto & [ key, val ] : key_vals) {
    key->SaveReferences(scope, args);
    val->SaveReferences(scope, args);
  }
}

void FunctionLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  return_type_expr->SaveReferences(scope, args);
  for (auto &input : inputs) { input->SaveReferences(scope, args); }
  statements->SaveReferences(fn_scope.get(), args);
}

void For::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  statements->SaveReferences(scope, args);
  for (auto &iter : iterators) { iter->SaveReferences(scope, args); }
}

void ScopeNode::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  expr->SaveReferences(scope, args);
  scope_expr->SaveReferences(scope, args);
  stmts->SaveReferences(scope, args);
}

void ScopeLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  enter_fn->SaveReferences(scope, args);
  exit_fn->SaveReferences(scope, args);
}

using RefMap = std::unordered_map<const Expression *, IR::Val>;
void Terminal::contextualize(const RefMap &) {}
void Identifier::contextualize(const RefMap &) {}
void CodeBlock::contextualize(const RefMap &) {}
void Jump::contextualize(const RefMap &) {}
void Hole::contextualize(const RefMap &) {}
void TokenNode::contextualize(const RefMap &) { UNREACHABLE(); }

void Binop::contextualize(const RefMap &replacements) {
  lhs->contextualize(replacements);
  rhs->contextualize(replacements);
}

void Declaration::contextualize(const RefMap &replacements) {
  identifier->contextualize(replacements);
  if (type_expr) { type_expr->contextualize(replacements); }
  if (init_val) { init_val->contextualize(replacements); }
}

void InDecl::contextualize(const RefMap &replacements) {
  container->contextualize(replacements);
}

void Statements::contextualize(const RefMap &replacements) {
  for (auto &stmt : statements) { stmt->contextualize(replacements); }
}

void Unop::contextualize(const RefMap &replacements) {
  if (op == Language::Operator::Ref) {
    auto iter = replacements.find(this);
    LOG << "(" << (uintptr_t) this << ")" << this;
    for (const auto& [k,v] : replacements) {
      LOG << "(" << (uintptr_t)k << ")" << k << " => " << v;
    }
    ASSERT(iter != replacements.end(), "");
    auto terminal        = base::make_owned<Terminal>();
    terminal->scope_     = scope_; // TODO Eh? Do I care?
    terminal->span       = span;
    terminal->precedence = precedence;
    terminal->lvalue     = lvalue; // TODO????
    terminal->type       = iter->second.type;
    terminal->value      = iter->second;
    operand              = std::move(terminal);
    op                   = Language::Operator::Nop;
  } else {
    operand->contextualize(replacements);
  }
}

void Access::contextualize(const RefMap &replacements) {
  operand->contextualize(replacements);
}

void ChainOp::contextualize(const RefMap &replacements) {
  for (const auto &expr : exprs) { expr->contextualize(replacements); }
}

void CommaList::contextualize(const RefMap &replacements) {
  for (const auto &expr : exprs) { expr->contextualize(replacements); }
}

void ArrayLiteral::contextualize(const RefMap &replacements) {
  for (const auto &elem : elems) { elem->contextualize(replacements); }
}

void ArrayType::contextualize(const RefMap &replacements) {
  length->contextualize(replacements);
  data_type->contextualize(replacements);
}

void Case::contextualize(const RefMap &replacements) {
  for (auto & [ key, val ] : key_vals) {
    key->contextualize(replacements);
    val->contextualize(replacements);
  }
}

void FunctionLiteral::contextualize(const RefMap &replacements) {
  return_type_expr->contextualize(replacements);
  for (auto &input : inputs) { input->contextualize(replacements); }
  statements->contextualize(replacements);
}

void For::contextualize(const RefMap &replacements) {
  statements->contextualize(replacements);
  for (auto &iter : iterators) { iter->contextualize(replacements); }
}

void ScopeNode::contextualize(const RefMap &replacements) {
  expr->contextualize(replacements);
  scope_expr->contextualize(replacements);
  stmts->contextualize(replacements);
}

void ScopeLiteral::contextualize(const RefMap &replacements) {
  enter_fn->contextualize(replacements);
  exit_fn->contextualize(replacements);
}

void Call::contextualize(const RefMap & /* replacements */) { NOT_YET(); }

} // namespace AST
