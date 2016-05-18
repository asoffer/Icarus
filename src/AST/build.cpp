#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
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
      static_cast<AST::Binop *>(expr)->op == Language::Operator::Assign) {
    error_log.log(expr->loc, msg + "Did you mean '==' instead of '='?");

    // TODO allow continuation after error here?
    static_cast<AST::Binop *>(expr)->op = Language::Operator::EQ;
  }
}

static void CheckStructMembers(AST::Statements *stmts,
                               std::vector<AST::Declaration *> &decls) {
  for (auto &&stmt : stmts->statements) {
    if (stmt->is_declaration()) {
      auto decl = static_cast<AST::Declaration *>(stmt);
      if (decl->decl_type != AST::DeclType::Std &&
          decl->decl_type != AST::DeclType::Infer) {
        error_log.log(decl->loc, "Declaration must be either ':' or ':='");
        continue;
      }
    } else if (stmt->is_binop()) {
      auto binop = static_cast<AST::Binop *>(stmt);
      if (binop->op == Language::Operator::Assign) {
        if (!binop->lhs->is_declaration()) {
          // TODO better error message
          error_log.log(binop->loc, "Nothing declared here.");
          continue;
        } else {
          auto decl = static_cast<AST::Declaration *>(binop->lhs);
          if (decl->decl_type != AST::DeclType::Std &&
              decl->decl_type != AST::DeclType::Infer) {
            error_log.log(decl->loc, "Declaration must be either ':' or ':='");
            continue;
          }
        }
      } else {
        // TODO better error message here.
        error_log.log(stmt->loc,
                      "Each struct member must be defined using "
                      "a declaration or an initialized declaration.");
        continue;
      }
    } else {
      // TODO better error message here.
      error_log.log(stmt->loc, "Each struct member must be defined using "
                               "a declaration or an initialized declaration.");
      continue;
    }

    decls.emplace_back(steal<AST::Declaration>(stmt));
  }
}

namespace AST {
// Input guarantees:
// [struct] [l_brace] [statements] [r_brace]
//
// Internal checks (checked in CheckStructMembers):
// Each statement is either a declaration using ':' or ':=', or it's an
// assignment where the left-hand side is a declaration using ':'. That is, we
// allow only the following:
//     a := b
//     a : b
//     a : b = c
Node *StructLiteral::Build(NPtrVec &&nodes) {
  static size_t anon_struct_counter = 0;

  auto struct_lit_ptr   = new StructLiteral;
  struct_lit_ptr->loc   = nodes[0]->loc;
  struct_lit_ptr->type  = Type_;
  struct_lit_ptr->value = Context::Value(Struct(
      "__anon.struct" + std::to_string(anon_struct_counter++), struct_lit_ptr));
  CheckStructMembers(static_cast<Statements *>(nodes[2]), struct_lit_ptr->declarations);
  return struct_lit_ptr;
}

Node *ParametricStructLiteral::Build(NPtrVec &&nodes) {
  static size_t anon_struct_counter = 0;

  auto struct_lit_ptr   = new ParametricStructLiteral;
  struct_lit_ptr->loc   = nodes[0]->loc;
  struct_lit_ptr->type  = Type_;
  struct_lit_ptr->value = Context::Value(
      ParamStruct("__anon.param.struct" + std::to_string(anon_struct_counter++),
                  struct_lit_ptr));

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

  CheckStructMembers(static_cast<Statements *>(nodes[3]), struct_lit_ptr->declarations);

  return struct_lit_ptr;
}

// Input guarantees:
// [enum] [l_brace] [statements] [r_brace]
//
// Internal checks:
// Each statement is an identifier. No identifier is repeated.
Node *EnumLiteral::Build(NPtrVec &&nodes) {
  auto enum_lit_ptr  = new EnumLiteral;
  enum_lit_ptr->loc  = nodes[0]->loc;
  enum_lit_ptr->type = Type_;

  if (nodes[2]->is_statements()) {
    auto stmts = static_cast<Statements *>(nodes[2]);
    for (auto &&stmt : stmts->statements) {
      if (!stmt->is_identifier()) {
        error_log.log(stmt->loc, "Enum members must be identifiers.");
      } else {
        // TODO repeated terms?
        // TODO move the string into place
        enum_lit_ptr->members.emplace_back(
            static_cast<Identifier *>(stmt)->token);
      }
    }
  }

  return enum_lit_ptr;
}

// Input guarantees:
// [case] [l_brace] [statements] [r_brace]
//
// Internal checks:
// Each statement is a binary operator using '=>'. The last one has a left-hand
// side of 'else'
Node *Case::Build(NPtrVec &&nodes) {
  auto case_ptr = new Case;
  case_ptr->loc = nodes[0]->loc;

  auto stmts     = static_cast<Statements *>(nodes[2]);
  auto num_stmts = stmts->statements.size();
  for (size_t i = 0; i < num_stmts; ++i) {
    auto stmt = stmts->statements[i];
    if (!stmt->is_binop() ||
        static_cast<Binop *>(stmt)->op != Language::Operator::Rocket) {
      error_log.log(stmt->loc,
                    "Each line in case statement must be a key-value pair.");
      continue;
    }

    auto binop = static_cast<Binop *>(stmt);

    // TODO check for 'else' and make sure it's the last one.
    case_ptr->key_vals.emplace_back(steal<Expression>(binop->lhs),
                                    steal<Expression>(binop->rhs));
  }
  return case_ptr;
}

// Input guarantees:
// [if] [expression] [l_brace] [statements] [r_brace]
//
// Internal checks:
// expression is not an assignemnt
Node *Conditional::BuildIf(NPtrVec &&nodes) {
  auto if_stmt = new Conditional;
  auto cond    = steal<Expression>(nodes[1]);

  if_stmt->conditions = {cond};
  CheckEqualsNotAssignment(cond,
                           "Expression in while-statement is an assignment. ");

  if_stmt->statements = {steal<Statements>(nodes[3])};
  if_stmt->body_scopes.push_back(new BlockScope(ScopeType::Conditional));
  return if_stmt;
}

// Input guarantees:
// [while] [expression] [l_brace] [statements] [r_brace]
//
// Internal checks:
// expression is not an assignment.
Node *While::Build(NPtrVec &&nodes) {
  auto while_stmt        = new While;
  while_stmt->statements = steal<Statements>(nodes[3]);

  while_stmt->condition = steal<Expression>(nodes[1]);
  CheckEqualsNotAssignment(while_stmt->condition,
                           "Condition in while-loop is an assignment. ");

  return while_stmt;
}

static void CheckForLoopDeclaration(Expression *maybe_decl,
                                    std::vector<InDecl *> &iters) {
  if (!maybe_decl->is_in_decl()) {
    error_log.log(maybe_decl->loc, "Expect 'in' declaration in for-loop.");
  } else {
    iters.push_back(static_cast<InDecl *>(maybe_decl));
  }
}

// Input guarantees:
// [for] [expression] [l_brace] [statements] [r_brace]
//
// Internal checks:
// [expression] is either an in-declaration or a list of in-declarations
Node *For::Build(NPtrVec &&nodes) {
  auto for_stmt        = new For;
  for_stmt->loc        = nodes[0]->loc;
  for_stmt->statements = steal<Statements>(nodes[3]);

  auto iter = steal<Expression>(nodes[1]);

  if (iter->is_comma_list()) {
    auto iter_list = static_cast<ChainOp *>(iter);
    for_stmt->iterators.reserve(iter_list->exprs.size());

    for (auto &ex : iter_list->exprs) {
      CheckForLoopDeclaration(ex, for_stmt->iterators);

      ex = nullptr;
    }
    delete iter_list;

  } else {
    CheckForLoopDeclaration(iter, for_stmt->iterators);
  }

  return for_stmt;
}

// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration unless it's a tick.
// Operand cannot be an assignment of any kind.
Node *Unop::BuildLeft(NPtrVec &&nodes) {
  auto unop_ptr     = new AST::Unop;
  unop_ptr->operand = steal<AST::Expression>(nodes[1]);

  // We intentionally do not delete tk_node becasue we only want to read from
  // it. The apply() call will take care of its deletion.
  unop_ptr->loc = nodes[0]->loc;

  assert(nodes[0]->is_token_node());
  auto tk = static_cast<TokenNode *>(nodes[0])->token;

  if (tk == "import") {
    // TODO we can't have a '/' character, and since all our programs are in
    // the programs/ directory for now, we hard-code that. This needs to be
    // removed.
    assert(unop_ptr->operand->is_terminal() &&
           static_cast<Terminal *>(unop_ptr->operand)->terminal_type ==
               Language::Terminal::StringLiteral);
    file_queue.emplace(std::string("programs/") +
                       std::string(unop_ptr->operand->value.as_str));

    unop_ptr->op = Language::Operator::Import;

  } else if (tk == "return") {
    unop_ptr->op = Language::Operator::Return;

  } else if (tk == "break") {
    unop_ptr->op = Language::Operator::Break;
    goto id_check;

  } else if (tk == "continue") {
    unop_ptr->op = Language::Operator::Continue;
    goto id_check;

  } else if (tk == "restart") {
    unop_ptr->op = Language::Operator::Restart;
    goto id_check;

  } else if (tk == "repeat") {
    unop_ptr->op = Language::Operator::Repeat;
    goto id_check;

  } else if (tk == "free") {
    unop_ptr->op = Language::Operator::Free;

  } else if (tk == "print") {
    unop_ptr->op = Language::Operator::Print;

  } else if (tk == "&") {
    unop_ptr->op = Language::Operator::And;

  } else if (tk == "-") {
    unop_ptr->op = Language::Operator::Sub;

  } else if (tk == "!") {
    unop_ptr->op = Language::Operator::Not;

  } else if (tk == "@") {
    unop_ptr->op = Language::Operator::At;

  } else {
    assert(false);
  }

  unop_ptr->precedence = Language::precedence(unop_ptr->op);

  if (unop_ptr->operand->is_declaration()) {
    auto decl = static_cast<Declaration *>(unop_ptr->operand);
    if (decl->decl_type != DeclType::Tick) {
      error_log.log(decl->loc, "Invalid use of declaration.");
    }
  }

  return unop_ptr;

id_check:
  unop_ptr->precedence = Language::precedence(unop_ptr->op);
  if (!unop_ptr->operand->is_identifier()) {
    error_log.log(unop_ptr->operand->loc,
                  "Operand to '" + tk + "' must be an identifier.");
  }
  return unop_ptr;
}

// Input guarantees:
// [expr] [l_paren] [r_paren]
//
// Internal checks:
// Operand is not a declaration
Node *Unop::BuildParen(NPtrVec &&nodes) {

  auto unop_ptr        = new Unop;
  unop_ptr->loc        = nodes[1]->loc;
  unop_ptr->operand    = steal<Expression>(nodes[0]);
  unop_ptr->op         = Language::Operator::Call;
  unop_ptr->precedence = Language::precedence(unop_ptr->op);

  if (unop_ptr->operand->is_declaration()) {
    error_log.log(unop_ptr->operand->loc,
                  "Invalid declaration. You cannot call a declaration.");
  }
  return unop_ptr;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
Node *ChainOp::Build(NPtrVec &&nodes) {
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
    error_log.log(access_ptr->operand->loc,
                  "Left-hand side cannot be a declaration");
  }

  if (!nodes[2]->is_identifier()) {
    error_log.log(nodes[2]->loc, "Right-hand side must be an identifier");
  } else {
    access_ptr->member_name = static_cast<Identifier *>(nodes[2])->token;
  }
  return access_ptr;
}

static Node *BuildOperator(NPtrVec &&nodes, Language::Operator op_class,
                           Language::NodeType nt) {
  auto binop_ptr = new Binop;
  binop_ptr->loc = nodes[1]->loc;

  binop_ptr->lhs       = steal<Expression>(nodes[0]);
  binop_ptr->rhs       = steal<Expression>(nodes[2]);
  binop_ptr->op        = op_class;

  if (binop_ptr->lhs->is_declaration()) {
    error_log.log(binop_ptr->lhs->loc,
                  "Left-hand side cannot be a declaration");
  }

  if (binop_ptr->rhs->is_declaration()) {
    auto decl = static_cast<Declaration *>(binop_ptr->rhs);
    if (decl->decl_type != DeclType::Tick) {
      error_log.log(binop_ptr->rhs->loc, "Right-hand side cannot be a "
                                         "declaration other than one declared "
                                         "with '`'");
    }
  }

  binop_ptr->precedence = Language::precedence(binop_ptr->op);

  return binop_ptr;
}

// Input guarantees
// [expr] [l_paren] [expr] [r_paren]
//
// Internal checks: (checked in BuildOperator)
// LHS is not a declaration
// RHS is not a declaration unless it's a tick
Node *Binop::BuildCallOperator(NPtrVec &&nodes) {
  return BuildOperator(std::forward<NPtrVec &&>(nodes),
                       Language::Operator::Call, Language::op_b);
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks: (checked in BuildOperator)
// LHS is not a declaration
// RHS is not a declaration unless it's a tick
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
  auto tk_node        = static_cast<TokenNode *>(nodes[1]);
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
    swap(array_lit_ptr->elems, static_cast<ChainOp *>(nodes[1])->exprs);

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

Node *Declaration::AddHashtag(NPtrVec &&nodes) {
  auto decl = steal<Declaration>(nodes[0]);
  assert(nodes[1]->is_token_node());
  decl->hashtags.push_back(static_cast<TokenNode *>(nodes[1])->token);

  return decl;
}

Node *InDecl::Build(NPtrVec &&nodes) {
  auto op = ((AST::TokenNode *)(nodes[1]))->op;
  assert(op == Language::Operator::In);

  auto in_decl_ptr        = new InDecl;
  in_decl_ptr->loc        = nodes[1]->loc;
  in_decl_ptr->identifier = steal<Identifier>(nodes[0]);
  in_decl_ptr->container  = steal<Expression>(nodes[2]);
  in_decl_ptr->precedence = Language::precedence(Language::Operator::In);
  return in_decl_ptr;
}

Node *Declaration::BuildBasic(NPtrVec &&nodes) {
  auto decl_ptr        = new Declaration;
  decl_ptr->loc        = nodes[1]->loc;
  decl_ptr->op         = ((AST::TokenNode *)(nodes[1]))->op;
  decl_ptr->precedence = Language::precedence(decl_ptr->op);
  decl_ptr->expr       = steal<Expression>(nodes[2]);
  decl_ptr->decl_type  = (decl_ptr->op == Language::Operator::Colon)
                            ? DeclType::Std
                            : DeclType::Infer;

  assert(nodes[0]->is_identifier());
  decl_ptr->identifier = new Identifier(
      nodes[0]->loc, static_cast<Identifier *>(nodes[0])->token);

  assert(decl_ptr->op == Language::Operator::Colon ||
         decl_ptr->op == Language::Operator::ColonEq);


  Scope::decl_registry_.emplace_back(decl_ptr);

  return decl_ptr;
}

Node *Declaration::BuildGenerate(NPtrVec &&nodes) {
  auto decl_ptr = new AST::Declaration;
  Scope::decl_registry_.emplace_back(decl_ptr);
  decl_ptr->loc        = nodes[1]->loc;
  decl_ptr->decl_type  = DeclType::Tick;
  decl_ptr->expr       = steal<Expression>(nodes[0]);
  decl_ptr->op         = Language::Operator::Tick;
  decl_ptr->precedence = Language::precedence(decl_ptr->op);

  assert(nodes[2]->is_identifier());
  decl_ptr->identifier = new Identifier(
      nodes[2]->loc, static_cast<Identifier *>(nodes[2])->token);


  return decl_ptr;
}

Node *FunctionLiteral::build(NPtrVec &&nodes) {
  auto fn_lit = new FunctionLiteral;
  fn_lit->loc = nodes[0]->loc;

  if (nodes[2]->is_statements()) {
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

Node *Jump::build(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = static_cast<TokenNode *>(nodes[0])->token;
  if (tk == "break") {
    return new Jump(nodes[0]->loc, JumpType::Break);

  } else if (tk == "continue") {
    return new Jump(nodes[0]->loc, JumpType::Continue);

  } else if (tk == "return") {
    return new Jump(nodes[0]->loc, JumpType::Return);

  } else if (tk == "repeat") {
    return new Jump(nodes[0]->loc, JumpType::Repeat);

  } else if (tk == "restart") {
    return new Jump(nodes[0]->loc, JumpType::Restart);
  }
  assert(false && "No other options");
}

} // namespace AST

AST::Node *BuildBinaryOperator(NPtrVec &&nodes) {
  static const std::map<std::string, Language::Operator> chain_ops = {
      {",", Language::Operator::Comma}, {"==", Language::Operator::EQ},
      {"!=", Language::Operator::NE},   {"<", Language::Operator::LT},
      {">", Language::Operator::GT},    {"<=", Language::Operator::LE},
      {">=", Language::Operator::GE},   {"&", Language::Operator::And},
      {"|", Language::Operator::Or},    {"^", Language::Operator::Xor},
  };

  assert(nodes[1]->is_token_node());
  auto tk = static_cast<AST::TokenNode *>(nodes[1])->token;

  for (auto op : chain_ops) {
    if (tk == op.first) {
      static_cast<AST::TokenNode *>(nodes[1])->op = op.second;
      return AST::ChainOp::Build(std::forward<NPtrVec &&>(nodes));
    }
  }

  if (tk == ".") {
    return AST::Access::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == ":" || tk == ":=") {
    return AST::Declaration::BuildBasic(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "in") {
    return AST::InDecl::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "`") {
    return AST::Declaration::BuildGenerate(std::forward<NPtrVec &&>(nodes));
  }

  auto binop_ptr = new AST::Binop;
  binop_ptr->loc = nodes[1]->loc;

  binop_ptr->lhs       = steal<AST::Expression>(nodes[0]);
  binop_ptr->rhs       = steal<AST::Expression>(nodes[2]);

#define LOOKUP_SYMBOL(sym, name)                                               \
  if (tk == sym) {                                              \
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
  assert(nodes[0]->is_token_node());
  auto tk = static_cast<AST::TokenNode *>(nodes[0])->token;


  if (tk == "case") {
    return AST::Case::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "enum") {
    return AST::EnumLiteral::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "struct") {
    return AST::StructLiteral::Build(std::forward<NPtrVec &&>(nodes));
  }

  assert(false);
}

AST::Node *BuildKWExprBlock(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = static_cast<AST::TokenNode *>(nodes[0])->token;

  if (tk == "for") {
    return AST::For::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "while") {
    return AST::While::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "if") {
    return AST::Conditional::BuildIf(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "struct") {
    return AST::ParametricStructLiteral::Build(std::forward<NPtrVec &&>(nodes));
  }

  assert(false);
}

AST::Node *Parenthesize(NPtrVec &&nodes) {
  auto expr_ptr = steal<AST::Expression>(nodes[1]);
  expr_ptr->precedence =
      Language::precedence(Language::Operator::NotAnOperator);
  return expr_ptr;
}
