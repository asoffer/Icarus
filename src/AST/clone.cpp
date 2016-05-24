#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
#endif

#define CLONE clone(num_entries, lookup_key, lookup_val)
#define LOOKUP_ARGS                                                            \
  size_t num_entries, TypeVariable **lookup_key, Type **lookup_val

namespace AST {
StructLiteral *
ParametricStructLiteral::CloneStructLiteral(StructLiteral *&cache_loc,
                                            bool has_vars) {
  auto num = data.ids.size();
  cache_loc->data.ids.reserve(num);
  cache_loc->data.types.reserve(num);
  cache_loc->data.init_vals.reserve(num);

  cache_loc->value.as_type->has_vars = false;
  for (size_t i = 0; i < num; ++i) {
    auto curr_id   = data.ids[i];
    auto curr_type = data.types[i];
    auto curr_init = data.init_vals[i];

    cache_loc->data.ids.push_back(curr_id);
    cache_loc->data.types.push_back(curr_type);
    cache_loc->data.init_vals.push_back(curr_init);
    // TODO Can init have a contextual dependence?

//    cache_loc->value.as_type->has_vars |=
//        cache_loc->data.types.back()->has_vars;
  }

  delete cache_loc->type_scope;
  cache_loc->type_scope = type_scope;
  cache_loc->scope_ = scope_; // Same scope as original.
  cache_loc->verify_types();

  cache_loc->value.as_type->has_vars = has_vars;
  if (!has_vars) { cache_loc->FlushOut(); }

  assert(value.as_type->is_parametric_struct());
  assert(cache_loc->value.as_type->is_struct());
  static_cast<Structure *>(cache_loc->value.as_type)->creator = this;
  return cache_loc;
}

Node *Expression::clone(LOOKUP_ARGS) { assert(false); }
Node *ParametricStructLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *StructLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *EnumLiteral::clone(LOOKUP_ARGS) { assert(false); }
Node *DummyTypeExpr::clone(LOOKUP_ARGS) { assert(false); }
Node *Jump::clone(LOOKUP_ARGS) { assert(false); }
Node *While::clone(LOOKUP_ARGS) { assert(false); }
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

Node *ArrayLiteral::clone(LOOKUP_ARGS) {
  auto array_lit = new ArrayLiteral;
  array_lit->elems.reserve(elems.size());
  for (auto el : elems) { array_lit->elems.push_back((Expression *)el->CLONE); }
  return array_lit;
}

Node *Binop::clone(LOOKUP_ARGS) {
  auto binop = new Binop;
  binop->op  = op;
  binop->lhs = (Expression *)lhs->CLONE;
  if (rhs) { binop->rhs = (Expression *)rhs->CLONE; }
  return binop;
}

Node *For::clone(LOOKUP_ARGS) {
  auto for_stmt        = new For;
  for_stmt->statements = (Statements *)statements->CLONE;

  for_stmt->iterators.reserve(iterators.size());
  for (auto i : iterators) {
    for_stmt->iterators.push_back((InDecl *)i->CLONE);
  }

  return for_stmt;
}

Node *Statements::clone(LOOKUP_ARGS) {
  auto stmts = new Statements;
  for (auto s : statements) { stmts->statements.push_back(s->CLONE); }
  return stmts;
}


Node *ArrayType::clone(LOOKUP_ARGS) {
  auto array_type       = new ArrayType;
  array_type->length    = (Expression *)length->CLONE;
  array_type->data_type = (Expression *)data_type->CLONE;
  return array_type;
}

Node *Unop::clone(LOOKUP_ARGS) {
  auto unop     = new Unop;
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
    decl->decl_type  = decl_type;
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

  return new Identifier(loc, token);
}
} // namespace AST
#undef CLONE
#undef LOOKUP_ARGS
