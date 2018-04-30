#include "ast.h"
#include "context.h"

namespace AST {
void Terminal::SaveReferences(Scope *, std::vector<IR::Val> *) {}
void Identifier::SaveReferences(Scope *, std::vector<IR::Val> *) {}

void Binop::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  lhs->SaveReferences(scope, args);
  rhs->SaveReferences(scope, args);
}

void Declaration::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  identifier->SaveReferences(scope, args);
  if (type_expr) { type_expr->SaveReferences(scope, args); }
  if (init_val) { init_val->SaveReferences(scope, args); }
}

void Call::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &pos : args_.pos_) { pos->SaveReferences(scope, args); }
  for (auto & [ name, expr ] : args_.named_) {
    expr->SaveReferences(scope, args);
  }
}

void Statements::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &stmt : content_) { stmt->SaveReferences(scope, args); }
}

void StructLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &f: fields_) { f->SaveReferences(scope, args); }
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

void FunctionLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &input : inputs) { input->SaveReferences(scope, args); }
  for (auto &output : outputs) { output->SaveReferences(scope, args); }
  statements->SaveReferences(fn_scope.get(), args);
}

void GenericFunctionLiteral::SaveReferences(Scope *scope,
                                            std::vector<IR::Val> *args) {
  FunctionLiteral::SaveReferences(scope, args);
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

#define CONTEXTUALIZE(thing)                                                   \
  (thing)->contextualize(                                                      \
      correspondant->as<std::decay_t<decltype(*this)>>().thing.get(),          \
      replacements)

using RefMap = std::unordered_map<const Expression *, IR::Val>;
void Terminal::contextualize(const Node *, const RefMap &) {}
void Identifier::contextualize(const Node *, const RefMap &) {}
void Binop::contextualize(const Node *correspondant,
                          const RefMap &replacements) {
  CONTEXTUALIZE(lhs);
  CONTEXTUALIZE(rhs);
}

void Declaration::contextualize(const Node *correspondant,
                                const RefMap &replacements) {
  CONTEXTUALIZE(identifier);
  if (type_expr) { CONTEXTUALIZE(type_expr); }
  if (init_val) { CONTEXTUALIZE(init_val); }
}

void Statements::contextualize(const Node *correspondant,
                               const RefMap &replacements) {
  for (size_t i = 0; i < content_.size(); ++i) { CONTEXTUALIZE(content_[i]); }
}

void Access::contextualize(const Node *correspondant,
                           const RefMap &replacements) {
  CONTEXTUALIZE(operand);
}

void ChainOp::contextualize(const Node *correspondant,
                            const RefMap &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) { CONTEXTUALIZE(exprs[i]); }
}

void StructLiteral::contextualize(const Node *correspondant,
                            const RefMap &replacements) {
  for (size_t i = 0; i < fields_.size(); ++i) { CONTEXTUALIZE(fields_[i]); }
}

void CommaList::contextualize(const Node *correspondant,
                              const RefMap &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) { CONTEXTUALIZE(exprs[i]); }
}

void FunctionLiteral::contextualize(const Node *correspondant,
                                    const RefMap &replacements) {
  for (size_t i = 0; i < inputs.size(); ++i) { CONTEXTUALIZE(inputs[i]); }
  for (size_t i = 0; i < outputs.size(); ++i) { CONTEXTUALIZE(outputs[i]); }
  CONTEXTUALIZE(statements);
}

void GenericFunctionLiteral::contextualize(const Node *correspondant,
                                           const RefMap &replacements) {
  FunctionLiteral::contextualize(correspondant, replacements);
}

void ScopeNode::contextualize(const Node *correspondant,
                              const RefMap &replacements) {
  CONTEXTUALIZE(expr);
  CONTEXTUALIZE(scope_expr);
  CONTEXTUALIZE(stmts);
}

void ScopeLiteral::contextualize(const Node *correspondant,
                                 const RefMap &replacements) {
  CONTEXTUALIZE(enter_fn);
  CONTEXTUALIZE(exit_fn);
}

void Call::contextualize(const Node *correspondant,
                         const RefMap &replacements) {
  CONTEXTUALIZE(fn_);
  for (size_t i = 0; i < args_.pos_.size(); ++i) {
    CONTEXTUALIZE(args_.pos_[i]);
  }
  for (auto && [ name, expr ] : args_.named_) {
    expr->contextualize(correspondant->as<std::decay_t<decltype(*this)>>()
                            .args_.named_.find(name)
                            ->second.get(),
                        replacements);
  }
}
#undef CONTEXTUALIZE
} // namespace AST
