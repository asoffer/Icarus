#include "AST.h"
#include "Context.h"
#include "DependencySystem.h"

// TODO shouldn't we just be calling build functions here mostly?

#define CLONE clone(num_entries, lookup_key, lookup_val)
#define LOOKUP_ARGS                                                            \
  size_t num_entries, TypeVariable **lookup_key, TypePtr *lookup_val

namespace AST {
StructLiteral *StructLiteral::CloneStructLiteral(StructLiteral *&cache_loc,
                                                 Context &ctx) {
  cache_loc->declarations.reserve(declarations.size());

  for (auto decl : declarations) {
    Dependency::PtrWithTorV ptr_with_torv(decl, false);
    Dependency::traverse_from(ptr_with_torv);

    auto type_expr_ptr   = decl->type_expr->evaluate(ctx).as_type;
    auto new_decl        = new Declaration;
    new_decl->identifier = new Identifier(line_num, decl->identifier->token());
    new_decl->line_num   = decl->line_num;
    new_decl->decl_type  = decl->decl_type;

    new_decl->type_expr = new DummyTypeExpr(decl->line_num, type_expr_ptr);

    Scope::Stack.push(type_scope);
    new_decl->assign_scope();
    Scope::Stack.pop();

    // no need to do type verification
    new_decl->type = type_expr_ptr;

    if (!type_expr_ptr->has_vars) { type_expr_ptr->generate_llvm(); }

    cache_loc->declarations.push_back(new_decl);
  }

  // we need to generate it's dependencies.
  Dependency::mark_as_done(cache_loc);
  return cache_loc;
}

Node *Expression::clone(LOOKUP_ARGS) { assert(false); }
Node *Binop::clone(LOOKUP_ARGS) { assert(false); }
Node *ChainOp::clone(LOOKUP_ARGS) { assert(false); }
Node *Case::clone(LOOKUP_ARGS) { assert(false); }
Node *Access::clone(LOOKUP_ARGS) { assert(false); }
Node *ArrayType::clone(LOOKUP_ARGS) { assert(false); }
Node *StructLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *EnumLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *DummyTypeExpr::clone(LOOKUP_ARGS) { assert(false); }
Node *Jump::clone(LOOKUP_ARGS) { assert(false); }
Node *KVPairList::clone(LOOKUP_ARGS) { assert(false); }
Node *For::clone(LOOKUP_ARGS) { assert(false); }
Node *While::clone(LOOKUP_ARGS) { assert(false); }
Node *ArrayLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *Conditional::clone(LOOKUP_ARGS) { assert(false); }
Node *TokenNode::clone(LOOKUP_ARGS) { assert(false); }
Node *Node::clone(LOOKUP_ARGS) { assert(false); }

Node *FunctionLiteral::clone(LOOKUP_ARGS) {
  auto fn_lit = new FunctionLiteral;

  fn_lit->return_type_expr = (Expression *)return_type_expr->CLONE;

  for (auto input : inputs) {
    fn_lit->inputs.push_back((Declaration *)input->CLONE);
  }

  fn_lit->statements = (Statements *)statements->CLONE;

  return fn_lit;
}

Node *Statements::clone(LOOKUP_ARGS) {
  auto stmts = new Statements;
  for (auto s : statements) { stmts->statements.push_back(s->CLONE); }
  return stmts;
}

Node *Unop::clone(LOOKUP_ARGS) {
  auto unop = new Unop;

  unop->op      = op;
  unop->operand = (Expression *)operand->CLONE;

  return unop;
}

Node *Terminal::clone(LOOKUP_ARGS) { 
  return this;
}

Node *Declaration::clone(LOOKUP_ARGS) { 
  if (decl_type == DeclType::Tick) {
    for (size_t i = 0; i < num_entries; ++i) {
      if (identifier == lookup_key[i]->identifier) {
        return new DummyTypeExpr(line_num, lookup_val[i].get);
      }
    }
    assert(false);

  } else {
    auto decl        = new Declaration;
    decl->identifier = (Identifier *)identifier->CLONE;
    decl->hashtags   = hashtags;
    decl->op         = op;
    decl->type_expr  = (Expression *)type_expr->CLONE;
    return decl;
  }
}

Node *Identifier::clone(LOOKUP_ARGS) {
  for (size_t i = 0; i < num_entries; ++i) {
    if (this == lookup_key[i]->identifier) {
      return new DummyTypeExpr(line_num, lookup_val[i].get);
    }
  }

  return new Identifier(line_num, token_);
}
} // namespace AST
#undef CLONE
#undef LOOKUP_ARGS
