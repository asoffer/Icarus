#include "Type.h"
#include "Scope.h"

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

AST::Node *BuildBinaryOperator(NPtrVec &&nodes) {
  static const std::vector<std::string> chain_ops = {
      ",", "==", "!=", "<", ">", "<=", ">=", "&", "|", "^"};

  for (auto op : chain_ops) {
    if (nodes[1]->token() == op) {
      return AST::ChainOp::build(std::forward<NPtrVec &&>(nodes));
    }
  }

  if (nodes[1]->token() == ".") {
    return AST::Access::build(std::forward<NPtrVec &&>(nodes));
  }

  if (nodes[1]->token() == ":" || nodes[1]->token() == ":=" ||
      nodes[1]->token() == "in") {
    return AST::Declaration::BuildBasic(std::forward<NPtrVec &&>(nodes));
  }

  if (nodes[1]->token() == "`") {
    return AST::Declaration::BuildGenerate(std::forward<NPtrVec &&>(nodes));
  }

  auto binop_ptr = new AST::Binop;
  binop_ptr->loc = nodes[1]->loc;

  binop_ptr->lhs   = steal<AST::Expression>(nodes[0]);
  binop_ptr->rhs   = steal<AST::Expression>(nodes[2]);
  binop_ptr->type_ = nodes[1]->node_type();

#define LOOKUP_SYMBOL(sym, name)                                               \
  if (nodes[1]->token() == sym) {                                              \
    binop_ptr->op = Language::Operator::name;                                  \
    goto end;                                                                  \
  }

  LOOKUP_SYMBOL("=>", Rocket)
  LOOKUP_SYMBOL("=", Assign)
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
  if (nodes[0]->token() == "case") {
    return AST::Case::build(std::forward<NPtrVec &&>(nodes));
  }

  if (nodes[0]->token() == "enum") {
    return AST::EnumLiteral::build(std::forward<NPtrVec &&>(nodes));
  }

  if (nodes[0]->token() == "struct") {
    return AST::StructLiteral::build(std::forward<NPtrVec &&>(nodes));
  }
 
  assert(false);
}

AST::Node *BuildKWExprBlock(NPtrVec &&nodes) {
  if (nodes[0]->token() == "for") {
    return AST::For::build(std::forward<NPtrVec &&>(nodes));
  }
  if (nodes[0]->token() == "while") {
    return AST::While::build(std::forward<NPtrVec &&>(nodes));
  }
  if (nodes[0]->token() == "if") {
    return AST::Conditional::build_if(std::forward<NPtrVec &&>(nodes));
  }
  assert(false);
}

AST::Node *BuildUnaryOperator(NPtrVec &&nodes) {
  auto unop_ptr     = new AST::Unop;
  unop_ptr->operand = steal<AST::Expression>(nodes[1]);

  // We intentionally do not delete tk_node becasue we only want to read from
  // it. The apply() call will take care of its deletion.
  unop_ptr->loc   = nodes[0]->loc;
  unop_ptr->type_ = Language::expr;
  if (nodes[0]->token() == "return") {
    unop_ptr->op = Language::Operator::Return;
  } else if (nodes[0]->token() == "print") {
    unop_ptr->op = Language::Operator::Print;
  } else if (nodes[0]->token() == "import") {
    // TODO we can't have a '/' character, and since all our programs are in the
    // programs/ directory for now, we hard-code that. This needs to be removed.
    file_queue.emplace("programs/" + nodes[1]->token());
    unop_ptr->op = Language::Operator::Import;
  } else if (nodes[0]->token() == "Free") {
    unop_ptr->op = Language::Operator::Free;
  }
  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  return unop_ptr;
}



namespace AST {
Node *Identifier::build(NPtrVec &&nodes) { assert(false); }

Node *Unop::build(NPtrVec &&nodes) {
  auto unop_ptr     = new Unop;
  unop_ptr->operand = steal<Expression>(nodes[1]);

  // We intentionally do not delete tk_node becasue we only want to read from
  // it. The apply() call will take care of its deletion.
  unop_ptr->loc   = nodes[0]->loc;
  unop_ptr->type_ = Language::expr;
  if (nodes[0]->token() == "return") {
    unop_ptr->op = Language::Operator::Return;
  } else {
    unop_ptr->op = ((AST::TokenNode *)(nodes[0]))->op;
  }

  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  return unop_ptr;
}

Node *Unop::build_paren_operator(NPtrVec &&nodes) {
  auto unop_ptr        = new Unop;
  unop_ptr->loc        = nodes[1]->loc;
  unop_ptr->operand    = steal<Expression>(nodes[0]);
  unop_ptr->type_      = Language::expr;
  unop_ptr->op         = Language::Operator::Call;
  unop_ptr->precedence = Language::precedence(unop_ptr->op);

  return unop_ptr;
}

// More generally, this is correct for any right-unary operation
Node *Unop::build_dots(NPtrVec &&nodes) {
  auto unop_ptr     = new Unop;
  unop_ptr->operand = steal<Expression>(nodes[0]);

  // We intentionally do not delete tk_node becasue we only want to read from
  // it. The apply() call will take care of its deletion.
  auto tk_node    = static_cast<TokenNode *>(nodes[1]);
  unop_ptr->loc   = tk_node->loc;
  unop_ptr->type_ = Language::expr;
  unop_ptr->op    = tk_node->op;

  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  return unop_ptr;
}

Node *Access::build(NPtrVec &&nodes) {
  auto access_ptr         = new Access;
  access_ptr->member_name = nodes[2]->token();
  access_ptr->loc         = nodes[0]->loc;
  access_ptr->operand     = steal<Expression>(nodes[0]);

  return access_ptr;
}

Node *Binop::build_operator(NPtrVec &&nodes, Language::Operator op_class,
                            Language::NodeType nt) {
  auto binop_ptr = new Binop;
  binop_ptr->loc = nodes[1]->loc;

  binop_ptr->lhs   = steal<Expression>(nodes[0]);
  binop_ptr->rhs   = steal<Expression>(nodes[2]);
  binop_ptr->type_ = nt;
  binop_ptr->op    = op_class;

  binop_ptr->precedence = Language::precedence(binop_ptr->op);

  return binop_ptr;
}

Node *Binop::build_assignment(NPtrVec &&nodes) {
  auto op = static_cast<TokenNode *>(nodes[1])->op;
  return Binop::build_operator(std::forward<NPtrVec &&>(nodes), op,
                               Language::op_b);
}

Node *Binop::build(NPtrVec &&nodes) {
  auto op = static_cast<TokenNode *>(nodes[1])->op;
  return Binop::build_operator(std::forward<NPtrVec &&>(nodes), op,
                               Language::op_b);
}

Node *Binop::BuildElseRocket(NPtrVec &&nodes) {
  auto term_ptr           = new Terminal;
  term_ptr->loc           = nodes[0]->loc;
  term_ptr->terminal_type = Language::Terminal::Else;
  term_ptr->type          = Bool;
  term_ptr->token_        = "else";
  delete nodes[0];
  nodes[0] = term_ptr;
  
  return Binop::build_operator(std::forward<NPtrVec &&>(nodes),
                               Language::Operator::Rocket, Language::op_b);
}


Node *Binop::build_paren_operator(NPtrVec &&nodes) {
  return Binop::build_operator(std::forward<NPtrVec &&>(nodes),
                               Language::Operator::Call, Language::op_b);
}

Node *Binop::build_bracket_operator(NPtrVec &&nodes) {
  return Binop::build_operator(std::forward<NPtrVec &&>(nodes),
                               Language::Operator::Index, Language::op_b);
}

Node *ChainOp::join(NPtrVec &&nodes) {
  // TODO FIXME
  auto lhs_prec = static_cast<Expression *>(nodes[0])->precedence;
  auto op_node  = steal<TokenNode>(nodes[1]);
  auto op_prec  = Language::precedence(op_node->op);
  auto rhs_prec = static_cast<Expression *>(nodes[2])->precedence;

  if (op_prec != rhs_prec) { return build({nodes[0], op_node, nodes[2]}); }

  auto rhs = steal<ChainOp>(nodes[2]);

  ChainOp *chain_ptr = nullptr;
  if (lhs_prec == op_prec && op_prec == rhs_prec) {
    chain_ptr = steal<ChainOp>(nodes[0]);

    chain_ptr->ops.push_back(op_node->op);

    chain_ptr->ops.insert(chain_ptr->ops.end(), rhs->ops.begin(),
                          rhs->ops.end());

    chain_ptr->ops.insert(chain_ptr->ops.begin(), rhs->ops.begin(),
                          rhs->ops.end());

  } else { // op_prec == rhs_prec
    chain_ptr = new ChainOp;
    chain_ptr->exprs.emplace_back(steal<Expression>(nodes[0]));

    const std::string &token = nodes[1]->token();
    using Language::Operator;
    // TODO move to lookup table
    if (token == "<") {
      chain_ptr->ops.push_back(Operator::LT);
    } else if (token == "<=") {
      chain_ptr->ops.push_back(Operator::LE);
    } else if (token == "==") {
      chain_ptr->ops.push_back(Operator::EQ);
    } else if (token == "!=") {
      chain_ptr->ops.push_back(Operator::NE);
    } else if (token == ">=") {
      chain_ptr->ops.push_back(Operator::GE);
    } else if (token == ">") {
      chain_ptr->ops.push_back(Operator::GT);
    } else if (token == "|") {
      chain_ptr->ops.push_back(Operator::Or);
    } else if (token == "^") {
      chain_ptr->ops.push_back(Operator::Xor);
    } else if (token == "&") {
      chain_ptr->ops.push_back(Operator::And);
    } else if (token == ",") {
      chain_ptr->ops.push_back(Operator::Comma);
    }

    chain_ptr->ops.insert(chain_ptr->ops.end(), rhs->ops.begin(),
                          rhs->ops.end());

    chain_ptr->exprs.insert(chain_ptr->exprs.begin(), rhs->exprs.begin(),
                            rhs->exprs.end());
  }

  // NOTE: All of rhs is copied into chain, so chain now owns all of the
  // pointers that were part of rhs. Thus, there is nothing to delete.
  return chain_ptr;
}

Node *ChainOp::build(NPtrVec &&nodes) {
  // We do not take ownership of op_node. Thus, we don't set nodes[1] to null.
  auto op_node = static_cast<TokenNode *>(nodes[1]);
  auto op_prec = Language::precedence(op_node->op);

  ChainOp *chain_ptr = nullptr;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  bool use_old_chain_op =
      nodes[0]->is_chain_op() &&
      static_cast<ChainOp *>(nodes[0])->precedence == op_prec;

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

Node *ArrayLiteral::BuildEmpty(NPtrVec &&nodes) {
  auto array_lit_ptr = new ArrayLiteral;
  array_lit_ptr->loc = nodes[0]->loc;
  return array_lit_ptr;
}

Node *ArrayLiteral::build(NPtrVec &&nodes) {
  auto array_lit_ptr = new ArrayLiteral;
  array_lit_ptr->loc = nodes[0]->loc;

  if (nodes[1]->is_comma_list()) {
    using std::swap;
    swap(array_lit_ptr->elems, static_cast<ChainOp *>(nodes[1])->exprs);

  } else {
    array_lit_ptr->elems.push_back(steal<Expression>(nodes[1]));
  }

  return array_lit_ptr;
}

Node *ArrayType::build(NPtrVec &&nodes) {
  if (nodes[1]->is_terminal() &&
      static_cast<Terminal *>(nodes[1])->terminal_type ==
          Language::Terminal::Hole) {
    return build_unknown(std::forward<NPtrVec &&>(nodes));
  }
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

Node *ArrayType::build_unknown(NPtrVec &&nodes) {
  auto array_type_ptr = new ArrayType;
  array_type_ptr->loc = nodes[0]->loc;

  // length == nullptr means we do not know the length of the array can
  // change.
  array_type_ptr->length    = nullptr;
  array_type_ptr->data_type = steal<Expression>(nodes[3]);

  return array_type_ptr;
}

Node *Terminal::build(NPtrVec &&) {
  // This function is only here to make the macro generation simpler
  // TODO remove it?
  assert(false && "Called a function that shouldn't be called.");
}

Node *Expression::build(NPtrVec &&) {
  // This function is only here to make the macro generation simpler
  // TODO remove it
  assert(false && "Called a function that shouldn't be called.");
}

Node *Expression::parenthesize(NPtrVec &&nodes) {
  auto expr_ptr = steal<Expression>(nodes[1]);
  expr_ptr->precedence =
      Language::precedence(Language::Operator::NotAnOperator);
  return expr_ptr;
}

Node *Declaration::AddHashtag(NPtrVec &&nodes) {
  auto decl = steal<Declaration>(nodes[0]);
  decl->hashtags.push_back(nodes[1]->token());

  return decl;
}

Node *Declaration::build(NPtrVec &&) {
  // This function is only here to make the macro generation simpler
  // TODO remove it
  assert(false && "Called a function that shouldn't be called.");
}

Node *Declaration::build(NPtrVec &&nodes, Language::NodeType node_type,
                         DeclType dt) {
  auto decl_ptr = Scope::make_declaration(nodes[1]->loc, dt, nodes[0]->token(),
                                          steal<Expression>(nodes[2]));

  decl_ptr->type_ = node_type;

  switch (dt) {
  case DeclType::Std: decl_ptr->op   = Language::Operator::Colon; break;
  case DeclType::Infer: decl_ptr->op = Language::Operator::ColonEq; break;
  case DeclType::In: decl_ptr->op = Language::Operator::In; break;
  case DeclType::Tick: assert(false); break;
  }

  decl_ptr->precedence = Language::precedence(decl_ptr->op);

  return decl_ptr;
}

Node *Declaration::BuildBasic(NPtrVec &&nodes) {
  if (((AST::TokenNode *)(nodes[1]))->op == Language::Operator::Colon) {
    return build(std::forward<NPtrVec &&>(nodes), Language::op_b,
                 DeclType::Std);
  } else if (((AST::TokenNode *)(nodes[1]))->op == Language::Operator::ColonEq) {
    return build(std::forward<NPtrVec &&>(nodes), Language::op_b,
                 DeclType::Infer);
  } else if (((AST::TokenNode *)(nodes[1]))->op == Language::Operator::In) {
    return build(std::forward<NPtrVec &&>(nodes), Language::op_b,
                 DeclType::In);
  }
  assert(false);
}

Node *Declaration::BuildGenerate(NPtrVec &&nodes) {
  auto decl_ptr =
      Scope::make_declaration(nodes[1]->loc, DeclType::Tick, nodes[2]->token(),
                              steal<Expression>(nodes[0]));

  decl_ptr->type_      = Language::expr;
  decl_ptr->op         = Language::Operator::Tick;
  decl_ptr->precedence = Language::precedence(decl_ptr->op);

  return decl_ptr;
}

Node *FunctionLiteral::build(NPtrVec &&nodes) {
  auto fn_lit = new FunctionLiteral;
  fn_lit->loc = nodes[0]->loc;

  if (nodes[2]->node_type() == Language::stmts) {
    fn_lit->statements = steal<Statements>(nodes[2]);
  } else {
    fn_lit->statements = new Statements;
  }

  // TODO scopes inside these statements should point to fn_scope.

  auto binop_ptr = static_cast<Binop *>(nodes[0]);

  fn_lit->return_type_expr = steal<Expression>(binop_ptr->rhs);
  auto input_args          = steal<Expression>(binop_ptr->lhs);

  // TODO What if the fn_expression is more complicated, like a function
  // of the form (int -> int) -> int? I'm not sure how robust this is
  if (input_args->is_declaration()) {
    fn_lit->inputs.push_back(static_cast<Declaration *>(input_args));

  } else if (input_args->is_comma_list()) {
    auto decl_list = steal<ChainOp>(input_args);

    // resize the input arg list
    fn_lit->inputs.resize(decl_list->exprs.size(), nullptr);

    size_t index = 0;
    for (auto &&expr : decl_list->exprs) {
      fn_lit->inputs[index++] = steal<Declaration>(expr);
    }
    delete input_args;
    input_args = nullptr;
  }

  return fn_lit;
}

Node *StructLiteral::build(NPtrVec &&nodes) {
  auto struct_lit_ptr  = new StructLiteral;
  struct_lit_ptr->loc  = nodes[0]->loc;
  struct_lit_ptr->type = Type_;

  if (nodes[2]->node_type() == Language::stmts) {
    auto stmts = static_cast<Statements *>(nodes[2]);
    for (auto &&stmt : stmts->statements) {
      // TODO handle this gracefully
      assert(
          stmt->is_declaration() &&
          "Statement is not a declaration, and that case isn't handled yet.");

      struct_lit_ptr->declarations.emplace_back(steal<Declaration>(stmt));
    }
  }

  return struct_lit_ptr;
}

Node *StructLiteral::build_parametric(NPtrVec &&nodes) {
  auto struct_lit_ptr  = new StructLiteral;
  struct_lit_ptr->loc  = nodes[0]->loc;
  struct_lit_ptr->type = Type_;

  if (nodes[1]->is_declaration()) {
    struct_lit_ptr->params = {steal<Declaration>(nodes[1])};

  } else if (nodes[1]->is_comma_list()) {
    auto expr_vec = steal<ChainOp>(nodes[1])->exprs;

    assert(struct_lit_ptr->params.empty());
    struct_lit_ptr->params.resize(expr_vec.size());

    for (size_t i = 0; i < expr_vec.size(); ++i) {
      assert(expr_vec[i]->is_declaration());
      struct_lit_ptr->params[i] = static_cast<Declaration *>(expr_vec[i]);
    }
  }

  auto stmts = static_cast<Statements *>(nodes[3]);
  for (auto &&stmt : stmts->statements) {
    // TODO handle this gracefully
    assert(stmt->is_declaration() &&
           "Statement is not a declaration, and that case isn't handled yet.");

    struct_lit_ptr->declarations.emplace_back(steal<Declaration>(stmt));
  }

  return struct_lit_ptr;
}

Node *Statements::build_one(NPtrVec &&nodes) {
  auto output = new Statements;
  output->statements.push_back(steal<Node>(nodes[0]));

  return output;
}

Node *Statements::build_more(NPtrVec &&nodes) {
  auto output = steal<Statements>(nodes[0]);
  output->statements.push_back(steal<Node>(nodes[1]));

  return output;
}

Node *Statements::build_double_expression_error(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Adjacent expressions");

  auto output = new Statements;
  output->loc = nodes[0]->loc;
  output->statements.push_back(steal<Node>(nodes[0]));
  output->statements.push_back(steal<Node>(nodes[1]));

  return output;
}

Node *Statements::build_extra_expression_error(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Adjacent expressions");

  auto output = steal<Statements>(nodes[0]);
  output->statements.push_back(steal<Node>(nodes[1]));

  return output;
}

Node *Conditional::build_if(NPtrVec &&nodes) {
  auto if_stmt        = new Conditional;
  if_stmt->conditions = {steal<Expression>(nodes[1])};
  if_stmt->statements = {steal<Statements>(nodes[3])};
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  return if_stmt;
}

Node *Conditional::build_extra_else_error(NPtrVec &&nodes) {
  auto if_stmt = static_cast<Conditional *>(nodes[0]);
  error_log.log(nodes[1]->loc, "If-statement already has an else-branch. "
                               "The first else-branch is on line " +
                                   std::to_string(if_stmt->else_line_num) +
                                   ".");

  return steal<Node>(nodes[0]);
}

Node *Conditional::build_extra_else_if_error(NPtrVec &&nodes) {
  auto if_stmt = static_cast<Conditional *>(nodes[0]);
  error_log.log(nodes[1]->loc,
                "Else-if block is unreachable because it follows an else "
                "block. The else-block is on line " +
                    std::to_string(if_stmt->else_line_num) + ".");

  return steal<Node>(nodes[0]);
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
  if_stmt->statements.push_back(steal<Statements>(nodes[3]));
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  return if_stmt;
}

Node *Conditional::build_if_assignment_error(NPtrVec &&nodes) {
  nodes[1] = error_log.assignment_vs_equality(nodes[1]);
  return build_if(std::forward<NPtrVec &&>(nodes));
}

Node *EnumLiteral::build(NPtrVec &&nodes) {
  auto enum_lit_ptr  = new EnumLiteral;
  enum_lit_ptr->loc  = nodes[0]->loc;
  enum_lit_ptr->type = Type_;

  if (nodes[2]->node_type() == Language::stmts) {
    auto stmts = static_cast<Statements *>(nodes[2]);
    for (auto &&stmt : stmts->statements) {
      if (!stmt->is_identifier()) {
        error_log.log(stmt->loc, "Enum members must be identifiers.");
      }

      // TODO repeated terms?
      // TODO move the string into place
      enum_lit_ptr->members.emplace_back(
          static_cast<Identifier *>(stmt)->token());
    }
  }

  return enum_lit_ptr;
}

Node *Jump::build(NPtrVec &&nodes) {
  if (nodes[0]->token() == "break") {
    return new Jump(nodes[0]->loc, JumpType::Break);

  } else if (nodes[0]->token() == "continue") {
    return new Jump(nodes[0]->loc, JumpType::Continue);

  } else if (nodes[0]->token() == "return") {
    return new Jump(nodes[0]->loc, JumpType::Return);

  } else if (nodes[0]->token() == "repeat") {
    return new Jump(nodes[0]->loc, JumpType::Repeat);

  } else if (nodes[0]->token() == "restart") {
    return new Jump(nodes[0]->loc, JumpType::Restart);
  }
  assert(false && "No other options");
}

Node *While::build(NPtrVec &&nodes) {
  auto while_stmt        = new While;
  while_stmt->condition  = steal<Expression>(nodes[1]);
  while_stmt->statements = steal<Statements>(nodes[3]);
  return while_stmt;
}

Node *For::build(NPtrVec &&nodes) {
  auto for_stmt        = new For;
  for_stmt->loc        = nodes[0]->loc;
  for_stmt->statements = steal<Statements>(nodes[3]);

  auto iter_or_iter_list = steal<Expression>(nodes[1]);
  if (iter_or_iter_list->is_comma_list()) {
    // Copy the exprs out
    auto iter_list = static_cast<ChainOp *>(iter_or_iter_list);
    for_stmt->iterators.reserve(iter_list->exprs.size());

    for (auto &ex : iter_list->exprs) {
      assert(ex->is_declaration());
      for_stmt->iterators.push_back(static_cast<Declaration *>(ex));
      ex = nullptr;
    }
    delete iter_list;

  } else {
    for_stmt->iterators = {static_cast<Declaration *>(iter_or_iter_list)};
  }

  return for_stmt;
}

Node *While::build_assignment_error(NPtrVec &&nodes) {
  nodes[1] = error_log.assignment_vs_equality(nodes[1]);
  return build(std::forward<NPtrVec &&>(nodes));
}

Node *Case::build(NPtrVec &&nodes) {
  auto case_ptr = new Case;
  case_ptr->loc = nodes[0]->loc;

  auto stmts = (Statements *)(nodes[2]);
  for (auto stmt : stmts->statements) {
    if (!stmt->is_binop() ||
        ((Binop *)(stmt))->op != Language::Operator::Rocket) {
      error_log.log(stmt->loc,
                    "Each line in case statement must be a key-value pair.");
    } else {
      assert(stmt->is_binop());
      auto binop = (Binop *)stmt;
      case_ptr->key_vals.emplace_back(steal<Expression>(binop->lhs),
                                      steal<Expression>(binop->rhs));
    }
  }
  return case_ptr;
}
} // namespace AST
