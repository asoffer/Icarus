#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#endif

extern std::queue<std::string> file_queue;

template <typename T> static T *steal(AST::Expression *&n) {
  auto temp = (T *)n;
  assert(temp && "stolen pointer is null");
  n = nullptr;
  return temp;
}

template <typename T> static T *steal(AST::Node *&n) {
  auto temp = (T *)n;
  assert(temp && "stolen pointer is null");
  n = nullptr;
  return temp;
}

static void CheckEqualsNotAssignment(AST::Expression *expr,
                                     const std::string &msg) {
  if (expr->is_binop() &&
      ((AST::Binop *)expr)->op == Language::Operator::Assign) {
    Error::Log::Log(expr->loc, msg + "Did you mean '==' instead of '='?");

    // TODO allow continuation after error here?
    ((AST::Binop *)expr)->op = Language::Operator::EQ;
  }
}

// Input guarantees:
// [expr] [l_paren] [r_paren]
//
// Internal checks:
// Operand is not a declaration
AST::Node *BuildEmptyParen(NPtrVec &&nodes) {
  auto binop_ptr        = new AST::Binop;
  binop_ptr->loc        = nodes[1]->loc;
  binop_ptr->lhs        = steal<AST::Expression>(nodes[0]);
  binop_ptr->rhs        = nullptr;
  binop_ptr->op         = Language::Operator::Call;
  binop_ptr->precedence = Language::precedence(binop_ptr->op);

  if (binop_ptr->lhs->is_declaration()) {
    Error::Log::Log(binop_ptr->lhs->loc,
                  "Invalid declaration. You cannot call a declaration.");
  }
  return binop_ptr;
}

namespace AST {
// Input guarantees:
// [struct] [braced_statements]
//
// Internal checks:
// Each statement is a valid declaration
static Node *BuildStructLiteral(NPtrVec &&nodes) {
  static size_t anon_struct_counter = 0;

  auto struct_type =
      new Struct("__anon.struct" + std::to_string(anon_struct_counter++));
  assert(nodes[1]->is_statements());
  for (auto &&n : ((Statements *)nodes[1])->statements) {
    if (n->is_declaration()) {
      struct_type->decls.push_back(steal<Declaration>(n));
    } else {
      Error::Log::Log(n->loc,
                    "Each struct member must be defined using a declaration.");
    }
  }
  return new DummyTypeExpr(nodes[0]->loc, struct_type);
}

static Node *BuildParametricStructLiteral(NPtrVec &&nodes) {
  std::vector<Declaration *> params, decls;
  if (nodes[1]->is_declaration()) {
    params.push_back(steal<Declaration>(nodes[1]));

  } else if (nodes[1]->is_comma_list()) {
    auto expr_vec = steal<ChainOp>(nodes[1])->exprs;
    for (auto &&e : expr_vec) {
      AST::Node *n = (AST::Node *)e;
      params.push_back(steal<Declaration>(n));
    }
  }

  for (auto &&node : ((Statements *)nodes[2])->statements) {
    decls.push_back(steal<Declaration>(node));
  }

  static size_t anon_param_struct_counter = 0;
  auto param_struct_type = new ParamStruct(
      "__anon.param.struct" + std::to_string(anon_param_struct_counter++),
      params, decls);

  auto dummy = new DummyTypeExpr(nodes[0]->loc, param_struct_type);
  for (auto param : params) { param->arg_val = dummy; }
  return dummy;
}

// Input guarantees:
// [enum] [braced_statements]
//
// Internal checks:
// Each statement is an identifier. No identifier is repeated.
static Node *BuildEnumLiteral(NPtrVec &&nodes) {
  std::vector<std::string> members;
  if (nodes[1]->is_statements()) {
    auto stmts = (Statements *)nodes[1];
    for (auto &&stmt : stmts->statements) {
      if (!stmt->is_identifier()) {
        Error::Log::Log(stmt->loc, "Enum members must be identifiers.");
      } else {
        // TODO repeated terms?
        // TODO move the string into place
        members.push_back(((Identifier *)stmt)->token);
      }
    }
  }

  static size_t anon_enum_counter = 0;
  return new DummyTypeExpr(
      nodes[0]->loc,
      new Enum("__anon.enum" + std::to_string(anon_enum_counter++), members));
}

// Input guarantees:
// [case] [braced_statements]
//
// Internal checks:
// Each statement is a binary operator using '=>'. The last one has a left-hand
// side of 'else'
Node *Case::Build(NPtrVec &&nodes) {
  auto case_ptr = new Case;
  case_ptr->loc = nodes[0]->loc;

  assert(nodes[1]->is_statements());
  auto stmts     = (Statements *)nodes[1];
  auto num_stmts = stmts->statements.size();
  for (size_t i = 0; i < num_stmts; ++i) {
    auto stmt = stmts->statements[i];
    if (!stmt->is_binop() ||
        ((Binop *)stmt)->op != Language::Operator::Rocket) {
      Error::Log::Log(stmt->loc,
                    "Each line in case statement must be a key-value pair.");
      continue;
    }

    auto binop = (Binop *)stmt;

    // TODO check for 'else' and make sure it's the last one.
    case_ptr->key_vals.emplace_back(steal<Expression>(binop->lhs),
                                    steal<Expression>(binop->rhs));
  }
  return case_ptr;
}

// Input guarantees:
// [if] [expression] [braced_statements]
//
// Internal checks:
// expression is not an assignemnt
Node *Conditional::BuildIf(NPtrVec &&nodes) {
  auto if_stmt = new Conditional;
  auto cond    = steal<Expression>(nodes[1]);

  if_stmt->conditions = {cond};
  CheckEqualsNotAssignment(cond,
                           "Expression in while-statement is an assignment. ");

  assert(nodes[2]->is_statements());
  if_stmt->statements = {steal<Statements>(nodes[2])};
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  if_stmt->loc = nodes[0]->loc;
  return if_stmt;
}

// Input guarantees:
// [while] [expression] [braced_statements]
//
// Internal checks:
// expression is not an assignment.
Node *While::Build(NPtrVec &&nodes) {
  assert(nodes[2]->is_statements());
  auto while_stmt        = new While;
  while_stmt->statements = steal<Statements>(nodes[2]);

  while_stmt->condition = steal<Expression>(nodes[1]);
  CheckEqualsNotAssignment(while_stmt->condition,
                           "Condition in while-loop is an assignment. ");

  auto stmts = new AST::Statements;
  stmts->loc = while_stmt->loc;
  stmts->statements.push_back(while_stmt);
  return stmts;
}

static void CheckForLoopDeclaration(Expression *maybe_decl,
                                    std::vector<InDecl *> &iters) {
  if (!maybe_decl->is_in_decl()) {
    Error::Log::Log(maybe_decl->loc, "Expect 'in' declaration in for-loop.");
  } else {
    iters.push_back((InDecl *)maybe_decl);
  }
}

// Input guarantees:
// [for] [expression] [braced_statements]
//
// Internal checks:
// [expression] is either an in-declaration or a list of in-declarations
Node *For::Build(NPtrVec &&nodes) {
  auto for_stmt        = new For;
  for_stmt->loc        = nodes[0]->loc;
  for_stmt->statements = steal<Statements>(nodes[2]);

  auto iter = steal<Expression>(nodes[1]);

  if (iter->is_comma_list()) {
    auto iter_list = (ChainOp *)iter;
    for_stmt->iterators.reserve(iter_list->exprs.size());

    for (auto &ex : iter_list->exprs) {
      CheckForLoopDeclaration(ex, for_stmt->iterators);

      ex = nullptr;
    }
    delete iter_list;

  } else {
    CheckForLoopDeclaration(iter, for_stmt->iterators);
  }

  auto stmts = new AST::Statements;
  stmts->loc = for_stmt->loc;
  stmts->statements.push_back(for_stmt);
  return stmts;
}

// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration.
// Operand cannot be an assignment of any kind.
Node *Unop::BuildLeft(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = ((TokenNode *)nodes[0])->token;

  auto unop_ptr     = new AST::Unop;
  unop_ptr->operand = steal<Expression>(nodes[1]);
  unop_ptr->loc     = nodes[0]->loc;

  if (strcmp(tk, "import") == 0) {
    // TODO we can't have a '/' character, and since all our programs are in
    // the programs/ directory for now, we hard-code that. This needs to be
    // removed.
    if (unop_ptr->operand->is_terminal() &&
        ((Terminal *)unop_ptr->operand)->terminal_type ==
            Language::Terminal::StringLiteral) {
      file_queue.emplace(std::string("programs/") +
                         std::string(unop_ptr->operand->value.as_cstr));
    } else {
      Error::Log::InvalidImport(unop_ptr->operand->loc);
    }

    unop_ptr->op = Language::Operator::Import;

  } else if (strcmp(tk, "return") == 0) {
    unop_ptr->op = Language::Operator::Return;

  } else if (strcmp(tk, "break") == 0) {
    unop_ptr->op = Language::Operator::Break;
    goto id_check;

  } else if (strcmp(tk, "continue") == 0) {
    unop_ptr->op = Language::Operator::Continue;
    goto id_check;

  } else if (strcmp(tk, "restart") == 0) {
    unop_ptr->op = Language::Operator::Restart;
    goto id_check;

  } else if (strcmp(tk, "repeat") == 0) {
    unop_ptr->op = Language::Operator::Repeat;
    goto id_check;

  } else if (strcmp(tk, "free") == 0) {
    unop_ptr->op = Language::Operator::Free;

  } else if (strcmp(tk, "print") == 0) {
    unop_ptr->op = Language::Operator::Print;

  } else if (strcmp(tk, "&") == 0) {
    unop_ptr->op = Language::Operator::And;

  } else if (strcmp(tk, "-") == 0) {
    unop_ptr->op = Language::Operator::Sub;

  } else if (strcmp(tk, "!") == 0) {
    unop_ptr->op = Language::Operator::Not;

  } else if (strcmp(tk, "@") == 0) {
    unop_ptr->op = Language::Operator::At;

  } else if (strcmp(tk, "$") == 0) {
    unop_ptr->op = Language::Operator::Eval;

  } else {
    UNREACHABLE;
  }

  unop_ptr->precedence = Language::precedence(unop_ptr->op);

  if (unop_ptr->operand->is_declaration()) {
    auto decl = (Declaration *)unop_ptr->operand;
    Error::Log::Log(decl->loc, "Invalid use of declaration.");
  }

  return unop_ptr;

id_check:
  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  if (!unop_ptr->operand->is_identifier()) {
    Error::Log::Log(unop_ptr->operand->loc,
                  "Operand to '" + std::string(tk) + "' must be an identifier.");
  }
  return unop_ptr;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
Node *ChainOp::Build(NPtrVec &&nodes) {
  // We do not take ownership of op_node. Thus, we don't set nodes[1] to null.
  auto op_node = (TokenNode *)nodes[1];
  auto op_prec = Language::precedence(op_node->op);

  ChainOp *chain_ptr = nullptr;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  bool use_old_chain_op =
      nodes[0]->is_chain_op() &&
      ((ChainOp *)nodes[0])->precedence == op_prec;

  if (use_old_chain_op) {
    chain_ptr = steal<ChainOp>(nodes[0]);

  } else {
    chain_ptr      = new ChainOp;
    chain_ptr->loc = nodes[1]->loc;

    chain_ptr->exprs.push_back(steal<Expression>(nodes[0]));
    nodes[0]              = nullptr;
    chain_ptr->precedence = op_prec;
  }

  chain_ptr->ops.push_back(op_node->op);
  chain_ptr->exprs.push_back(steal<Expression>(nodes[2]));

  return chain_ptr;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
Node *Access::Build(NPtrVec &&nodes) {
  auto access_ptr     = new Access;
  access_ptr->loc     = nodes[0]->loc;
  access_ptr->operand = steal<Expression>(nodes[0]);

  if (access_ptr->operand->is_declaration()) {
    Error::Log::Log(access_ptr->operand->loc,
                  "Left-hand side cannot be a declaration");
  }

  if (!nodes[2]->is_identifier()) {
    Error::Log::Log(nodes[2]->loc, "Right-hand side must be an identifier");
  } else {
    access_ptr->member_name = ((Identifier *)nodes[2])->token;
  }
  return access_ptr;
}

static Node *BuildOperator(NPtrVec &&nodes, Language::Operator op_class,
                           Language::NodeType nt) {
  auto binop_ptr = new Binop;
  binop_ptr->loc = nodes[1]->loc;

  binop_ptr->lhs = steal<Expression>(nodes[0]);
  binop_ptr->rhs = steal<Expression>(nodes[2]);
  binop_ptr->op  = op_class;

  if (binop_ptr->lhs->is_declaration()) {
    Error::Log::Log(binop_ptr->lhs->loc,
                  "Left-hand side cannot be a declaration");
  }

  if (binop_ptr->rhs->is_declaration()) {
    Error::Log::Log(binop_ptr->rhs->loc, "Right-hand side cannot be a "
                                       "declaration other than one declared "
                                       "with '`'");
  }

  binop_ptr->precedence = Language::precedence(binop_ptr->op);

  return binop_ptr;
}

// Input guarantees
// [expr] [l_paren] [expr] [r_paren]
//
// Internal checks: (checked in BuildOperator)
// LHS is not a declaration
// RHS is not a declaration
Node *Binop::BuildCallOperator(NPtrVec &&nodes) {
  return BuildOperator(std::forward<NPtrVec &&>(nodes),
                       Language::Operator::Call, Language::op_b);
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks: (checked in BuildOperator)
// LHS is not a declaration
// RHS is not a declaration
Node *Binop::BuildIndexOperator(NPtrVec &&nodes) {
  return BuildOperator(std::forward<NPtrVec &&>(nodes),
                       Language::Operator::Index, Language::op_b);
}

// Input guarantee:
// [expression] [l_bracket] [r_bracket]
//
// Internal checks: None
Node *ArrayLiteral::BuildEmpty(NPtrVec &&nodes) {
  auto array_lit_ptr = new ArrayLiteral;
  array_lit_ptr->loc = nodes[0]->loc;
  return array_lit_ptr;
}

// Input guarantee:
// [expression] [dots]
//
// Internal checks: None
Node *Unop::BuildDots(NPtrVec &&nodes) {
  auto unop_ptr     = new Unop;
  unop_ptr->operand = steal<Expression>(nodes[0]);

  // We intentionally do not delete tk_node becasue we only want to read from
  // it. The apply() call will take care of its deletion.
  auto tk_node        = (TokenNode *)nodes[1];
  unop_ptr->loc       = tk_node->loc;
  unop_ptr->op        = tk_node->op;

  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  return unop_ptr;
}

Node *ArrayLiteral::build(NPtrVec &&nodes) {
  auto array_lit_ptr = new ArrayLiteral;
  array_lit_ptr->loc = nodes[0]->loc;

  if (nodes[1]->is_comma_list()) {
    using std::swap;
    swap(array_lit_ptr->elems, ((ChainOp *)nodes[1])->exprs);

  } else {
    array_lit_ptr->elems.push_back(steal<Expression>(nodes[1]));
  }

  return array_lit_ptr;
}

Node *ArrayType::build(NPtrVec &&nodes) {
  if (nodes[1]->is_comma_list()) {
    auto length_chain = steal<ChainOp>(nodes[1]);
    auto iter         = length_chain->exprs.rbegin();
    auto prev         = steal<Expression>(nodes[3]);

    while (iter != length_chain->exprs.rend()) {
      auto array_type_ptr       = new ArrayType;
      array_type_ptr->loc       = (*iter)->loc;
      array_type_ptr->length    = *iter;
      *iter                     = nullptr;
      array_type_ptr->data_type = prev;
      prev                      = array_type_ptr;
      ++iter;
    }
    delete length_chain;
    return prev;

  } else {
    auto array_type_ptr       = new ArrayType;
    array_type_ptr->loc       = nodes[0]->loc;
    array_type_ptr->length    = steal<Expression>(nodes[1]);
    array_type_ptr->data_type = steal<Expression>(nodes[3]);

    return array_type_ptr;
  }
}

Node *Expression::build(NPtrVec &&) {
  // This function is only here to make the macro generation simpler
  // TODO remove it
  assert(false && "Called a function that shouldn't be called.");
}

Node *Expression::AddHashtag(NPtrVec &&nodes) {
  assert(nodes[0]->is_expression());
  auto expr= steal<Expression>(nodes[0]);
  assert(nodes[1]->is_token_node());
  expr->hashtags.push_back(((TokenNode *)nodes[1])->token);

  return expr;
}

Node *InDecl::Build(NPtrVec &&nodes) {
  auto op = ((AST::TokenNode *)(nodes[1]))->op;
  assert(op == Language::Operator::In);

  auto in_decl_ptr              = new InDecl;
  in_decl_ptr->loc              = nodes[1]->loc;
  in_decl_ptr->identifier       = steal<Identifier>(nodes[0]);
  in_decl_ptr->identifier->decl = in_decl_ptr;
  in_decl_ptr->container        = steal<Expression>(nodes[2]);
  in_decl_ptr->precedence       = Language::precedence(Language::Operator::In);
  return in_decl_ptr;
}

Node *Declaration::Build(NPtrVec &&nodes) {
  auto op              = ((AST::TokenNode *)(nodes[1]))->op;
  auto decl_ptr        = new Declaration;
  decl_ptr->loc        = nodes[1]->loc;
  decl_ptr->precedence = Language::precedence(op);

  if (op == Language::Operator::Colon) {
    decl_ptr->type_expr = steal<Expression>(nodes[2]);
  } else {
    decl_ptr->init_val  = steal<Expression>(nodes[2]);
  }

  assert(nodes[0]->is_identifier());
  decl_ptr->identifier       = steal<Identifier>(nodes[0]);
  decl_ptr->identifier->decl = decl_ptr;

  return decl_ptr;
}

Node *Generic::Build(NPtrVec &&nodes) {
  auto generic        = new Generic;
  generic->loc        = nodes[1]->loc;
  generic->test_fn    = steal<Expression>(nodes[0]);
  generic->precedence = Language::precedence(Language::Operator::Tick);

  assert(nodes[2]->is_identifier());
  generic->identifier       = steal<Identifier>(nodes[2]);
  generic->identifier->decl = generic;

  return generic;
}

Node *FunctionLiteral::build(NPtrVec &&nodes) {
  auto fn_lit = new FunctionLiteral;
  fn_lit->loc = nodes[0]->loc;

  assert(nodes[1]->is_statements());
  fn_lit->statements = steal<Statements>(nodes[1]);

  auto binop_ptr = (Binop *)nodes[0];

  fn_lit->return_type_expr = steal<Expression>(binop_ptr->rhs);
  auto input_args          = steal<Expression>(binop_ptr->lhs);

  if (input_args->is_declaration()) {
    fn_lit->inputs.push_back((Declaration *)input_args);
    ((Declaration *)input_args)->arg_val = fn_lit;

  } else if (input_args->is_comma_list()) {
    auto decl_list = steal<ChainOp>(input_args);

    // resize the input arg list
    fn_lit->inputs.resize(decl_list->exprs.size(), nullptr);

    size_t index = 0;
    for (auto &&expr : decl_list->exprs) {
      assert(expr->is_declaration());
      fn_lit->inputs[index] = steal<Declaration>(expr);
      ((Declaration *)fn_lit->inputs[index])->arg_val = fn_lit;
      ++index;
    }
    delete input_args;
    input_args = nullptr;
  }

  return fn_lit;
}

Node *Statements::build_one(NPtrVec &&nodes) {
  auto output = new Statements;
  output->loc = nodes[0]->loc;
  output->statements.push_back(steal<Node>(nodes[0]));

  return output;
}

Node *Statements::build_more(NPtrVec &&nodes) {
  auto output = steal<Statements>(nodes[0]);
  output->statements.push_back(steal<Node>(nodes[1]));

  return output;
}

Node *Conditional::build_else_if(NPtrVec &&nodes) {
  auto if_stmt = steal<Conditional>(nodes[0]);
  auto else_if = steal<Conditional>(nodes[2]);

  assert(else_if->conditions.size() == 1 && else_if->statements.size() == 1 &&
         else_if->body_scopes.size() == 1 && "Else-if statement constructed by "
                                             "parser with multiple conditional "
                                             "blocks.");

  if_stmt->conditions.push_back(else_if->conditions.front());
  if_stmt->statements.push_back(else_if->statements.front());
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  return if_stmt;
}

Node *Conditional::build_else(NPtrVec &&nodes) {
  auto if_stmt           = steal<Conditional>(nodes[0]);
  if_stmt->else_line_num = nodes[1]->loc.line_num;
  if_stmt->statements.push_back(steal<Statements>(nodes[2]));
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  return if_stmt;
}

Node *Jump::build(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk   = ((TokenNode *)nodes[0])->token;
  Jump *jmp = nullptr;
  if (strcmp(tk, "break") == 0) {
    jmp = new Jump(nodes[0]->loc, JumpType::Break);

  } else if (strcmp(tk, "continue") == 0) {
    jmp = new Jump(nodes[0]->loc, JumpType::Continue);

  } else if (strcmp(tk, "return") == 0) {
    jmp = new Jump(nodes[0]->loc, JumpType::Return);

  } else if (strcmp(tk, "repeat") == 0) {
    jmp = new Jump(nodes[0]->loc, JumpType::Repeat);

  } else if (strcmp(tk, "restart") == 0) {
    jmp = new Jump(nodes[0]->loc, JumpType::Restart);
  }
  assert(jmp);

  auto stmts = new AST::Statements;
  stmts->loc = jmp->loc;
  stmts->statements.push_back(jmp);
  return stmts;
}

} // namespace AST

AST::Node *BracedStatements(NPtrVec &&nodes) {
  assert(nodes[1]->is_statements());
  return steal<AST::Node>(nodes[1]);
}

AST::Node *OneBracedStatement(NPtrVec &&nodes) {
  auto stmts = new AST::Statements;
  stmts->loc = nodes[1]->loc;
  auto single_stmt = steal<AST::Node>(nodes[1]);
  stmts->statements.push_back(single_stmt);
  return stmts;
}

AST::Node *EmptyBraces(NPtrVec &&nodes) {
  auto stmts = new AST::Statements;
  stmts->loc = nodes[0]->loc;
  return stmts;
}


AST::Node *BuildBinaryOperator(NPtrVec &&nodes) {
  static const std::map<std::string, Language::Operator> chain_ops = {
      {",", Language::Operator::Comma}, {"==", Language::Operator::EQ},
      {"!=", Language::Operator::NE},   {"<", Language::Operator::LT},
      {">", Language::Operator::GT},    {"<=", Language::Operator::LE},
      {">=", Language::Operator::GE},   {"&", Language::Operator::And},
      {"|", Language::Operator::Or},    {"^", Language::Operator::Xor},
  };

  assert(nodes[1]->is_token_node());
  auto tk = ((AST::TokenNode *)nodes[1])->token;

  for (auto op : chain_ops) {
    if (tk == op.first) {
      ((AST::TokenNode *)nodes[1])->op = op.second;
      return AST::ChainOp::Build(std::forward<NPtrVec &&>(nodes));
    }
  }

  if (strcmp(tk, ".") == 0) {
    return AST::Access::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, ":") == 0 || strcmp(tk, ":=") == 0) {
    return AST::Declaration::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "in") == 0) {
    return AST::InDecl::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "`") == 0) {
    return AST::Generic::Build(std::forward<NPtrVec &&>(nodes));
  }

  if (strcmp(tk, "=") == 0) {
    if (nodes[0]->is_declaration()) {
      // TODO Disallow a := b = c
      auto decl      = steal<AST::Declaration>(nodes[0]);
      decl->init_val = steal<AST::Expression>(nodes[2]);
      return decl;
    } else {
      auto binop_ptr = new AST::Binop;
      binop_ptr->loc = nodes[0]->loc;

      binop_ptr->lhs = steal<AST::Expression>(nodes[0]);
      binop_ptr->rhs = steal<AST::Expression>(nodes[2]);

      binop_ptr->op         = Language::Operator::Assign;
      binop_ptr->precedence = Language::precedence(binop_ptr->op);
      return binop_ptr;
    }
  }

  auto binop_ptr = new AST::Binop;
  binop_ptr->loc = nodes[0]->loc;

  binop_ptr->lhs = steal<AST::Expression>(nodes[0]);
  binop_ptr->rhs = steal<AST::Expression>(nodes[2]);

#define LOOKUP_SYMBOL(sym, name)                                               \
  if (strcmp(tk, sym) == 0) {                                                  \
    binop_ptr->op = Language::Operator::name;                                  \
    goto end;                                                                  \
  }

  LOOKUP_SYMBOL("=>", Rocket)
  LOOKUP_SYMBOL(":>", Cast)
  LOOKUP_SYMBOL("->", Arrow)
  LOOKUP_SYMBOL("|=", OrEq)
  LOOKUP_SYMBOL("&=", AndEq)
  LOOKUP_SYMBOL("^=", XorEq)
  LOOKUP_SYMBOL("+=", AddEq)
  LOOKUP_SYMBOL("-=", SubEq)
  LOOKUP_SYMBOL("*=", MulEq)
  LOOKUP_SYMBOL("/=", DivEq)
  LOOKUP_SYMBOL("%=", ModEq)
  LOOKUP_SYMBOL("..", Dots)
  LOOKUP_SYMBOL("+", Add)
  LOOKUP_SYMBOL("-", Sub)
  LOOKUP_SYMBOL("*", Mul)
  LOOKUP_SYMBOL("/", Div)
  LOOKUP_SYMBOL("%", Mod)
  LOOKUP_SYMBOL("[", Index)
  LOOKUP_SYMBOL("(", Call)
#undef LOOKUP_SYMBOL
end:
  binop_ptr->precedence = Language::precedence(binop_ptr->op);

  return binop_ptr;
}

AST::Node *BuildKWBlock(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = ((AST::TokenNode *)nodes[0])->token;

  if (strcmp(tk, "case") == 0) {
    return AST::Case::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "enum") == 0) {
    return AST::BuildEnumLiteral(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "struct") == 0) {
    return AST::BuildStructLiteral(std::forward<NPtrVec &&>(nodes));
  }

  UNREACHABLE;
}

AST::Node *BuildKWExprBlock(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = ((AST::TokenNode *)nodes[0])->token;

  if (strcmp(tk, "for") == 0) {
    return AST::For::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "while") == 0) {
    return AST::While::Build(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "if") == 0) {
    return AST::Conditional::BuildIf(std::forward<NPtrVec &&>(nodes));

  } else if (strcmp(tk, "struct") == 0) {
    return AST::BuildParametricStructLiteral(std::forward<NPtrVec &&>(nodes));
  }

  UNREACHABLE;
}

AST::Node *Parenthesize(NPtrVec &&nodes) {
  auto expr_ptr = steal<AST::Expression>(nodes[1]);
  expr_ptr->precedence =
      Language::precedence(Language::Operator::NotAnOperator);
  return expr_ptr;
}
