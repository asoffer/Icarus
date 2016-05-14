#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
#include "Context.h"
#endif

// TODO shouldn't we just be calling build functions here mostly?

#define CLONE clone(num_entries, lookup_key, lookup_val)
#define LOOKUP_ARGS                                                            \
  size_t num_entries, TypeVariable **lookup_key, Type **lookup_val

namespace AST {
StructLiteral *StructLiteral::CloneStructLiteral(StructLiteral *&cache_loc,
                                                 Context &ctx) {
  cache_loc->declarations.reserve(declarations.size());

  for (auto decl : declarations) {
    decl->verify_types();
//    Dependency::PtrWithTorV ptr_with_torv(decl, false);
//    Dependency::traverse_from(ptr_with_torv);

    auto expr_ptr        = decl->expr->evaluate(ctx).as_type;
    auto new_decl        = new Declaration;
    new_decl->identifier = new Identifier(loc, decl->identifier->token());
    new_decl->loc        = decl->loc;
    new_decl->decl_type  = decl->decl_type;

    new_decl->expr = new DummyTypeExpr(decl->loc, expr_ptr);

    Scope::Stack.push(type_scope);
    new_decl->assign_scope();
    Scope::Stack.pop();

    // Set the scope of this cloned object to be the same as the original.
    cache_loc->scope_ = scope_;

    // no need to do type verification
    new_decl->type = expr_ptr;

    if (!expr_ptr->has_vars) { expr_ptr->generate_llvm(); }

    cache_loc->declarations.push_back(new_decl);
  }

  // we need to generate it's dependencies.
  // Dependency::mark_as_done(cache_loc);
  return cache_loc;
}

Node *Expression::clone(LOOKUP_ARGS) { assert(false); }
Node *Binop::clone(LOOKUP_ARGS) { assert(false); }
Node *ArrayType::clone(LOOKUP_ARGS) { assert(false); }
Node *StructLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *EnumLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *DummyTypeExpr::clone(LOOKUP_ARGS) { assert(false); }
Node *Jump::clone(LOOKUP_ARGS) { assert(false); }
Node *For::clone(LOOKUP_ARGS) { assert(false); }
Node *While::clone(LOOKUP_ARGS) { assert(false); }
Node *ArrayLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *TokenNode::clone(LOOKUP_ARGS) { assert(false); }
Node *Node::clone(LOOKUP_ARGS) { assert(false); }

Node *FunctionLiteral::clone(LOOKUP_ARGS) {
  auto fn_lit              = new FunctionLiteral;
  fn_lit->return_type_expr = (Expression *)return_type_expr->CLONE;
  fn_lit->statements       = (Statements *)statements->CLONE;
  fn_lit->code_gened       = code_gened;

  for (auto input : inputs) {
    fn_lit->inputs.push_back((Declaration *)input->CLONE);
  }


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

Node *Conditional::clone(LOOKUP_ARGS) {
  auto cond_node           = new Conditional;
  cond_node->else_line_num = else_line_num;

  cond_node->conditions.reserve(conditions.size());
  for (auto c : conditions) {
    cond_node->conditions.push_back((Expression *)c->CLONE);
  }

  cond_node->statements.reserve(conditions.size());
  for (auto s : statements) {
    cond_node->statements.push_back((Statements *)s->CLONE);
  }

  auto num_body_scopes = body_scopes.size();
  cond_node->body_scopes.reserve(num_body_scopes);

  for (size_t i = 0; i < num_body_scopes; ++i) {
    cond_node->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  }

  return cond_node;
}

Node *ChainOp::clone(LOOKUP_ARGS) {
  auto chain_node = new ChainOp;
  chain_node->ops = ops;

  chain_node->exprs.reserve(exprs.size());
  for (auto e : exprs) { chain_node->exprs.push_back((Expression *)e->CLONE); }

  return chain_node;
}

Node *Terminal::clone(LOOKUP_ARGS) { return this; }

Node *Case::clone(LOOKUP_ARGS) {
  auto case_node = new Case;
  for (auto& kv : key_vals) {
    case_node->key_vals.emplace_back((Expression *)kv.first->CLONE,
                                     (Expression *)kv.second->CLONE);
  }

  return case_node;
}

Node *Access::clone(LOOKUP_ARGS) {
  auto access_node         = new Access;
  access_node->member_name = member_name;
  access_node->operand     = (Expression *)operand->CLONE;
  return access_node;
}

Node *InDecl::clone(LOOKUP_ARGS) {
  auto in_decl        = new InDecl;
  in_decl->identifier = (Identifier *)identifier->CLONE;
  in_decl->container  = (Expression *)container->CLONE;
  return in_decl;
}

Node *Declaration::clone(LOOKUP_ARGS) { 
  if (decl_type == DeclType::Tick) {
    for (size_t i = 0; i < num_entries; ++i) {
      if (identifier == lookup_key[i]->identifier) {
        return new DummyTypeExpr(loc, lookup_val[i]);
      }
    }
    assert(false);

  } else {
    auto decl        = new Declaration;
    decl->identifier = (Identifier *)identifier->CLONE;
    decl->hashtags   = hashtags;
    decl->op         = op;
    decl->expr       = (Expression *)expr->CLONE;
    return decl;
  }
}

Node *Identifier::clone(LOOKUP_ARGS) {
  for (size_t i = 0; i < num_entries; ++i) {
    if (this == lookup_key[i]->identifier) {
      return new DummyTypeExpr(loc, lookup_val[i]);
    }
  }

  return new Identifier(loc, token_);
}
} // namespace AST
#undef CLONE
#undef LOOKUP_ARGS
