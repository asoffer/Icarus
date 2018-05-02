#include "ast.h"
#include "context.h"

namespace AST {
void Binop::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  lhs->SaveReferences(scope, args);
  rhs->SaveReferences(scope, args);
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

#define CONTEXTUALIZE(thing)                                                   \
  (thing)->contextualize(                                                      \
      correspondant->as<std::decay_t<decltype(*this)>>().thing.get(),          \
      replacements)

using RefMap = std::unordered_map<const Expression *, IR::Val>;
void Binop::contextualize(const Node *correspondant,
                          const RefMap &replacements) {
  CONTEXTUALIZE(lhs);
  CONTEXTUALIZE(rhs);
}
void Statements::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &stmt : content_) { stmt->SaveReferences(scope, args); }
}


void Access::contextualize(const Node *correspondant,
                           const RefMap &replacements) {
  CONTEXTUALIZE(operand);
}

void ChainOp::contextualize(const Node *correspondant,
                            const RefMap &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) { CONTEXTUALIZE(exprs[i]); }
}


void CommaList::contextualize(const Node *correspondant,
                              const RefMap &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) { CONTEXTUALIZE(exprs[i]); }
}

#undef CONTEXTUALIZE
} // namespace AST
