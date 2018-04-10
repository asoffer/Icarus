#include "ast.h"
#include "context.h"

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

void Unop::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  if (op == Language::Operator::Ref) {
    // TODO need to extract the right module here
    Context ctx(nullptr);
    operand->assign_scope(scope);
    operand->VerifyType(&ctx);
    operand->Validate(&ctx);
    auto val = operand->EmitIR(&ctx);

    args->push_back(val);
    args->push_back(IR::Val::Ref(this));
  } else {
    operand->SaveReferences(scope, args);
  }
}

void Import::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  operand_->SaveReferences(scope, args);
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

void FunctionLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  if (return_type_expr) { return_type_expr->SaveReferences(scope, args); }
  for (auto &input : inputs) { input->SaveReferences(scope, args); }
  statements->SaveReferences(fn_scope.get(), args);
}

void GenericFunctionLiteral::SaveReferences(Scope *scope,
                                            std::vector<IR::Val> *args) {
  FunctionLiteral::SaveReferences(scope, args);
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

#define CONTEXTUALIZE(thing)                                                   \
  (thing)->contextualize(                                                      \
      correspondant->as<std::decay_t<decltype(*this)>>().thing.get(),          \
      replacements)

using RefMap = std::unordered_map<const Expression *, IR::Val>;
void Terminal::contextualize(const Node *, const RefMap &) {}
void Identifier::contextualize(const Node *, const RefMap &) {}
void CodeBlock::contextualize(const Node *, const RefMap &) {}
void Jump::contextualize(const Node *, const RefMap &) {}
void Hole::contextualize(const Node *, const RefMap &) {}
void TokenNode::contextualize(const Node *, const RefMap &) { UNREACHABLE(); }

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

void InDecl::contextualize(const Node *correspondant,
                           const RefMap &replacements) {
  CONTEXTUALIZE(container);
}

void Statements::contextualize(const Node *correspondant,
                               const RefMap &replacements) {
  for (size_t i = 0; i < content_.size(); ++i) { CONTEXTUALIZE(content_[i]); }
}

void Unop::contextualize(const Node *correspondant,
                         const RefMap &replacements) {
  if (op == Language::Operator::Ref) {
    auto iter = replacements.find(&correspondant->as<Unop>());
    ASSERT(iter != replacements.end(), "");
    auto terminal    = std::make_unique<Terminal>();
    terminal->scope_ = scope_; // TODO Eh? Do I care?
    terminal->span   = span;
    terminal->lvalue = lvalue; // TODO????
    terminal->type   = iter->second.type;
    terminal->value  = iter->second;
    operand          = std::move(terminal);
    op               = Language::Operator::Pass;
  } else {
    CONTEXTUALIZE(operand);
  }
}

void Import::contextualize(const Node *correspondant,
                         const RefMap &replacements) {
  CONTEXTUALIZE(operand_);
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

void ArrayLiteral::contextualize(const Node *correspondant,
                                 const RefMap &replacements) {
  for (size_t i = 0; i < elems.size(); ++i) { CONTEXTUALIZE(elems[i]); }
}

void ArrayType::contextualize(const Node *correspondant,
                              const RefMap &replacements) {
  CONTEXTUALIZE(length);
  CONTEXTUALIZE(data_type);
}

void FunctionLiteral::contextualize(const Node *correspondant,
                                    const RefMap &replacements) {
  if (return_type_expr) { CONTEXTUALIZE(return_type_expr); }
  for (size_t i = 0; i < inputs.size(); ++i) { CONTEXTUALIZE(inputs[i]); }
  CONTEXTUALIZE(statements);
}

void GenericFunctionLiteral::contextualize(const Node *correspondant,
                                           const RefMap &replacements) {
  FunctionLiteral::contextualize(correspondant, replacements);
}

void For::contextualize(const Node *correspondant, const RefMap &replacements) {
  CONTEXTUALIZE(statements);
  for (size_t i = 0; i < iterators.size(); ++i) { CONTEXTUALIZE(iterators[i]); }
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
