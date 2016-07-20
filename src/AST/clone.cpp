#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#include "IR/Stack.h"
#endif

#define CLONE clone(num_entries, lookup_key, lookup_val)
#define LOOKUP_ARGS                                                            \
  size_t num_entries, TypeVariable **lookup_key, Type **lookup_val

extern std::stack<Scope *> ScopeStack;
extern AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr,
                                                  const Ctx &ctx);

namespace AST {
void ParametricStructLiteral::CloneStructLiteral(StructLiteral *&cache_loc) {
  auto arg_vals  = reverse_cache[cache_loc];
  auto num_decls = decls.size();
  cache_loc->decls.reserve(num_decls);

  for (size_t i = 0; i < num_decls; ++i) {
    auto new_decl = new Declaration;
    new_decl->identifier =
        new Identifier(decls[i]->identifier->loc, decls[i]->identifier->token);
    new_decl->identifier->decl = new_decl;
    new_decl->hashtags         = decls[i]->hashtags;

    if (decls[i]->type_expr) {
      auto old_func  = IR::Func::Current;
      auto old_block = IR::Block::Current;

      // This is kinda hacky to get the right eval. TODO move arg_vals into
      // declaration node.
      auto fn_ptr = WrapExprIntoFunction(decls[i]->type_expr, arg_vals);
      auto local_stack = new IR::LocalStack;
      IR::Func *func = fn_ptr->EmitAnonymousIR().as_func;
      func->SetName("anonymous-func");

      IR::Func::Current  = old_func;
      IR::Block::Current = old_block;

      auto result = func->Call(local_stack, {});
      delete local_stack;

      delete fn_ptr;

      new_decl->type_expr =
          new DummyTypeExpr(decls[i]->type_expr->loc, result.as_type);
    }
    if (decls[i]->init_val) { NOT_YET; }

    cache_loc->decls.push_back(new_decl);
  }

  ScopeStack.push(scope_);
  cache_loc->assign_scope();
  ScopeStack.pop();

  assert(cache_loc == ((Structure *)cache_loc->value.as_type)->ast_expression);
  cache_loc->CompleteDefinition();

  assert(value.as_type->is_parametric_struct());
  assert(cache_loc->value.as_type->is_struct());
  ((Structure *)cache_loc->value.as_type)->creator = this;
}

Node *DummyTypeExpr::clone(LOOKUP_ARGS) {
  if (value.as_type->has_vars) {
    NOT_YET;
  } else {
    return new DummyTypeExpr(loc, value.as_type);
  }
}

Node *Expression::clone(LOOKUP_ARGS) { NOT_YET; }
Node *ParametricStructLiteral::clone(LOOKUP_ARGS) { NOT_YET; }
Node *StructLiteral::clone(LOOKUP_ARGS) { NOT_YET; }
Node *EnumLiteral::clone(LOOKUP_ARGS) { NOT_YET; }
Node *Jump::clone(LOOKUP_ARGS) { NOT_YET; }
Node *While::clone(LOOKUP_ARGS) { NOT_YET; }
Node *TokenNode::clone(LOOKUP_ARGS) { NOT_YET; }
Node *Node::clone(LOOKUP_ARGS) { NOT_YET; }

Node *FunctionLiteral::clone(LOOKUP_ARGS) {
  auto fn_lit              = new FunctionLiteral;
  fn_lit->return_type_expr = (Expression *)return_type_expr->CLONE;
  fn_lit->statements       = (Statements *)statements->CLONE;
  fn_lit->loc              = loc;

  for (auto input : inputs) {
    auto cloned_input     = (Declaration *)input->CLONE;
    cloned_input->arg_val = fn_lit;
    fn_lit->inputs.push_back(cloned_input);
  }

  return fn_lit;
}

Node *ArrayLiteral::clone(LOOKUP_ARGS) {
  auto array_lit = new ArrayLiteral;
  array_lit->loc = loc;
  array_lit->elems.reserve(elems.size());
  for (auto el : elems) { array_lit->elems.push_back((Expression *)el->CLONE); }
  return array_lit;
}

Node *Binop::clone(LOOKUP_ARGS) {
  auto binop = new Binop;
  binop->op  = op;
  binop->lhs = (Expression *)lhs->CLONE;
  binop->loc = loc;

  if (rhs) { binop->rhs = (Expression *)rhs->CLONE; }
  return binop;
}

Node *For::clone(LOOKUP_ARGS) {
  auto for_stmt        = new For;
  for_stmt->statements = (Statements *)statements->CLONE;
  for_stmt->loc        = loc;

  for_stmt->iterators.reserve(iterators.size());
  for (auto i : iterators) {
    for_stmt->iterators.push_back((InDecl *)i->CLONE);
  }

  return for_stmt;
}

Node *Statements::clone(LOOKUP_ARGS) {
  auto stmts = new Statements;
  stmts->loc = loc;

  for (auto s : statements) { stmts->statements.push_back(s->CLONE); }
  return stmts;
}


Node *ArrayType::clone(LOOKUP_ARGS) {
  auto array_type       = new ArrayType;
  array_type->length    = (Expression *)length->CLONE;
  array_type->data_type = (Expression *)data_type->CLONE;
  array_type->loc       = loc;
  return array_type;
}

Node *Unop::clone(LOOKUP_ARGS) {
  auto unop     = new Unop;
  unop->op      = op;
  unop->operand = (Expression *)operand->CLONE;
  unop->loc     = loc;

  return unop;
}

Node *Conditional::clone(LOOKUP_ARGS) {
  auto cond_node           = new Conditional;
  cond_node->else_line_num = else_line_num;
  cond_node->loc           = loc;

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
  chain_node->loc = loc;

  chain_node->exprs.reserve(exprs.size());
  for (auto e : exprs) { chain_node->exprs.push_back((Expression *)e->CLONE); }

  return chain_node;
}

Node *Terminal::clone(LOOKUP_ARGS) { return this; }

Node *Case::clone(LOOKUP_ARGS) {
  auto case_node = new Case;
  case_node->loc = loc;

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
  access_node->loc         = loc;
  return access_node;
}


Node *Generic::clone(LOOKUP_ARGS) {
  for (size_t i = 0; i < num_entries; ++i) {
    if (identifier == lookup_key[i]->identifier) {
      return new DummyTypeExpr(loc, lookup_val[i]);
    }
  }

  auto generic              = new Generic;
  generic->identifier       = (Identifier *)identifier->CLONE;
  generic->test_fn          = (Expression *)test_fn->CLONE;
  generic->identifier->decl = generic;
  generic->loc              = loc;
  return generic;
}

Node *InDecl::clone(LOOKUP_ARGS) {
  auto in_decl              = new InDecl;
  in_decl->identifier       = (Identifier *)identifier->CLONE;
  in_decl->container        = (Expression *)container->CLONE;
  in_decl->identifier->decl = in_decl;
  in_decl->loc              = loc;
  return in_decl;
}

Node *Declaration::clone(LOOKUP_ARGS) {
  auto decl              = new Declaration;
  decl->identifier       = (Identifier *)identifier->CLONE;
  decl->identifier->decl = decl;
  decl->hashtags         = hashtags;
  decl->loc              = loc;

  if (type_expr) { decl->type_expr = (Expression *)type_expr->CLONE; }
  if (init_val) { decl->init_val = (Expression *)init_val->CLONE; }

  return decl;
}

Node *Identifier::clone(LOOKUP_ARGS) {
  for (size_t i = 0; i < num_entries; ++i) {
    if (token == lookup_key[i]->identifier->token) {
      return new DummyTypeExpr(loc, lookup_val[i]);
    }
  }

  return new Identifier(loc, token);
}
} // namespace AST
#undef CLONE
#undef LOOKUP_ARGS
