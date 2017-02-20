#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#endif

namespace Hashtag {
size_t Get(const std::string &tag);
} // namespace Hashtag

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
    ErrorLog::CallingDeclaration(binop_ptr->lhs->loc);
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
      // TODO show the entire struct declaration and point to the problematic
      // lines.
      ErrorLog::NonDeclInStructDecl(n->loc);
    }
  }
  return new DummyTypeExpr(nodes[0]->loc, struct_type);
}

static Node *BuildScopeLiteral(NPtrVec &&nodes) {
  auto scope_lit = new ScopeLiteral(nodes[0]->loc);

  // TODO take arguments as well
  assert(nodes[1]->is_statements());
  for (auto &&n : ((Statements *)nodes[1])->statements) {
    if (!n->is_declaration()) { continue; } // TODO leaking
    auto d = (Declaration *)n;
    if (d->identifier->token == "enter") {
      scope_lit->enter_fn = steal<Declaration>(n);
    } else if (d->identifier->token == "exit") {
      scope_lit->exit_fn = steal<Declaration>(n);
    }
  }

  if (!scope_lit->enter_fn) {
    // TODO log an error
  }

  if (!scope_lit->exit_fn) {
    // TODO log an error
  }

  return scope_lit;
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
        ErrorLog::EnumNeedsIds(stmt->loc);
      } else {
        const std::string &val_name = ((Identifier *)stmt)->token;
        for (const auto mem : members) {
          if (mem == val_name) {
            ErrorLog::RepeatedEnumName(stmt->loc);
            goto skip_adding_member;
          }
        }

        members.push_back(std::move(val_name));
      }
    skip_adding_member:;
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
      ErrorLog::NonKVInCase(stmt->loc);
      continue;
    }

    auto binop = (Binop *)stmt;

    // TODO check for 'else' and make sure it's the last one.
    case_ptr->key_vals.emplace_back(steal<Expression>(binop->lhs),
                                    steal<Expression>(binop->rhs));
  }
  return case_ptr;
}

static void CheckForLoopDeclaration(Expression *maybe_decl,
                                    std::vector<InDecl *> &iters) {
  if (!maybe_decl->is_in_decl()) {
    ErrorLog::NonInDeclInForLoop(maybe_decl->loc);
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

  bool check_id = false;
  if (tk == "require") {
    // TODO we can't have a '/' character, and since all our programs are in
    // the programs/ directory for now, we hard-code that. This needs to be
    // removed.
    if (unop_ptr->operand->is_terminal() &&
        ((Terminal *)unop_ptr->operand)->terminal_type ==
            Language::Terminal::StringLiteral) {
      file_queue.emplace(std::string("programs/") +
                         std::string(unop_ptr->operand->value.as_cstr +
                                     1)); // NOTE: + 1 because we prefix our
                                          // strings. Probably should fix this.
    } else {
      ErrorLog::InvalidRequirement(unop_ptr->operand->loc);
    }

    unop_ptr->op = Language::Operator::Require;

  } else {
    const static std::map<std::string, std::pair<Language::Operator, bool>>
        UnopMap = {{"return", {Language::Operator::Return, false}},
                   {"break", {Language::Operator::Break, true}},
                   {"continue", {Language::Operator::Continue, true}},
                   {"restart", {Language::Operator::Restart, true}},
                   {"repeat", {Language::Operator::Repeat, true}},
                   {"free", {Language::Operator::Free, false}},
                   {"generate", {Language::Operator::Generate, false}},
                   {"print", {Language::Operator::Print, false}},
                   {"&", {Language::Operator::And, false}},
                   {"-", {Language::Operator::Sub, false}},
                   {"!", {Language::Operator::Not, false}},
                   {"@", {Language::Operator::At, false}},
                   {"$", {Language::Operator::Eval, false}}};
    auto iter = UnopMap.find(tk);
    assert(iter != UnopMap.end());
    std::tie(unop_ptr->op, check_id) = iter->second;
  }

  unop_ptr->precedence = Language::precedence(unop_ptr->op);

  if (check_id) {
    if (!unop_ptr->operand->is_identifier()) {
      // TODO clean up error message
      ErrorLog::NonIdJumpOperand(unop_ptr->operand->loc);
    }
  } else {
    if (unop_ptr->operand->is_declaration()) {
      auto decl = (Declaration *)unop_ptr->operand;
      // TODO clean up this error message
      ErrorLog::InvalidDecl(decl->loc);
    }
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
      nodes[0]->is_chain_op() && ((ChainOp *)nodes[0])->precedence == op_prec;

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
    ErrorLog::LHSDecl(access_ptr->operand->loc);
  }

  if (!nodes[2]->is_identifier()) {
    ErrorLog::RHSNonIdInAccess(nodes[2]->loc);
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
    ErrorLog::LHSDecl(binop_ptr->lhs->loc);
  }

  if (binop_ptr->rhs->is_declaration()) {
    ErrorLog::RHSNonTickDecl(binop_ptr->rhs->loc);
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
  auto tk_node  = (TokenNode *)nodes[1];
  unop_ptr->loc = tk_node->loc;
  unop_ptr->op  = tk_node->op;

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

Node *Expression::AddHashtag(NPtrVec &&nodes) {
  assert(nodes[0]->is_expression());
  auto expr = steal<Expression>(nodes[0]);
  assert(nodes[1]->is_token_node());
  expr->hashtag_indices.push_back(Hashtag::Get(((TokenNode *)nodes[1])->token));

  return expr;
}

Node *InDecl::Build(NPtrVec &&nodes) {
  auto op = ((AST::TokenNode *)(nodes[1]))->op;
  assert(op == Language::Operator::In);

  auto in_decl_ptr              = new InDecl;
  in_decl_ptr->loc              = nodes[0]->loc;
  in_decl_ptr->identifier       = steal<Identifier>(nodes[0]);
  in_decl_ptr->identifier->decl = in_decl_ptr;
  in_decl_ptr->container        = steal<Expression>(nodes[2]);
  in_decl_ptr->precedence       = Language::precedence(Language::Operator::In);
  return in_decl_ptr;
}

Node *Declaration::Build(NPtrVec &&nodes) {
  auto op              = ((AST::TokenNode *)(nodes[1]))->op;
  auto decl_ptr        = new Declaration;
  decl_ptr->loc        = nodes[0]->loc;
  decl_ptr->precedence = Language::precedence(op);

  if (op == Language::Operator::Colon) {
    decl_ptr->type_expr = steal<Expression>(nodes[2]);
  } else {
    decl_ptr->init_val = steal<Expression>(nodes[2]);
  }

  assert(nodes[0]->is_identifier());
  decl_ptr->identifier       = steal<Identifier>(nodes[0]);
  decl_ptr->identifier->decl = decl_ptr;

  return decl_ptr;
}

Node *Generic::Build(NPtrVec &&nodes) {
  auto generic        = new Generic;
  generic->loc        = nodes[0]->loc;
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

Node *Jump::build(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk   = ((TokenNode *)nodes[0])->token;
  Jump *jmp = nullptr;
  if (tk == "break") {
    jmp = new Jump(nodes[0]->loc, JumpType::Break);

  } else if (tk == "continue") {
    jmp = new Jump(nodes[0]->loc, JumpType::Continue);

  } else if (tk == "return") {
    jmp = new Jump(nodes[0]->loc, JumpType::Return);

  } else if (tk == "repeat") {
    jmp = new Jump(nodes[0]->loc, JumpType::Repeat);

  } else if (tk == "restart") {
    jmp = new Jump(nodes[0]->loc, JumpType::Restart);
  }
  assert(jmp);

  auto stmts = new Statements;
  stmts->loc = jmp->loc;
  stmts->statements.push_back(jmp);
  return stmts;
}

Node *ScopeNode::BuildScopeNode(Expression *scope_name, Expression *arg_expr,
                                Statements *stmt_node) {
  auto scope_node        = new ScopeNode;
  scope_node->loc        = scope_name->loc;
  scope_node->scope_expr = scope_name;
  scope_node->expr       = arg_expr;
  scope_node->stmts      = stmt_node;
  scope_node->internal   = new BlockScope(ScopeEnum::Standard);
  return scope_node;
}

AST::Node *ScopeNode::Build(NPtrVec &&nodes) {
  return BuildScopeNode(steal<Expression>(nodes[0]),
                        steal<Expression>(nodes[1]),
                        steal<Statements>(nodes[2]));
}

AST::Node *ScopeNode::BuildVoid(NPtrVec &&nodes) {
  return BuildScopeNode(steal<Expression>(nodes[0]), nullptr,
                        steal<Statements>(nodes[1]));
}
} // namespace AST

AST::Node *BracedStatements(NPtrVec &&nodes) {
  assert(nodes[1]->is_statements());
  return steal<AST::Node>(nodes[1]);
}

AST::Node *AST::CodeBlock::BuildFromStatements(NPtrVec &&nodes) {
  auto block = new CodeBlock;
  // TODO block->value
  block->loc = nodes[0]->loc;
  block->stmts = steal<AST::Statements>(nodes[1]);
  return block;
}

AST::Node *OneBracedStatement(NPtrVec &&nodes) {
  auto stmts       = new AST::Statements;
  stmts->loc       = nodes[0]->loc;
  auto single_stmt = steal<AST::Node>(nodes[1]);
  stmts->statements.push_back(single_stmt);
  return stmts;
}

AST::Node *BracedStatementsSameLineEnd(NPtrVec &&nodes) {
  auto stmts = steal<AST::Statements>(nodes[1]);
  stmts->loc = nodes[0]->loc;
  if (nodes[2]->is_statements()) {
    auto second_stmts = (AST::Statements *)nodes[2];
    for (auto s : second_stmts->statements) {
      stmts->statements.push_back(steal<AST::Node>(s));
    }
  } else {
    stmts->statements.push_back(steal<AST::Node>(nodes[2]));
  }
  return stmts;
}

AST::Node *AST::CodeBlock::BuildFromStatementsSameLineEnd(NPtrVec &&nodes) {
  auto block = new CodeBlock;
  // TODO block->value
  block->loc   = nodes[0]->loc;
  block->stmts = static_cast<AST::Statements *>(
      BracedStatementsSameLineEnd(std::forward<NPtrVec &&>(nodes)));
  return block;
}

AST::Node *AST::CodeBlock::BuildFromOneStatement(NPtrVec &&nodes) {
  auto block = new CodeBlock;
  // TODO block->value
  block->loc   = nodes[0]->loc;
  block->stmts = static_cast<AST::Statements *>(
      OneBracedStatement(std::forward<NPtrVec &&>(nodes)));
  return block;
}

AST::Node *EmptyBraces(NPtrVec &&nodes) {
  auto stmts = new AST::Statements;
  stmts->loc = nodes[0]->loc;
  return stmts;
}

AST::Node *AST::CodeBlock::BuildEmpty(NPtrVec &&nodes) {
  auto block = new CodeBlock;
  // TODO block->value
  block->loc   = nodes[0]->loc;
  block->stmts = static_cast<AST::Statements *>(
      EmptyBraces(std::forward<NPtrVec &&>(nodes)));
  return block;
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

  if (tk == ".") {
    return AST::Access::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == ":" || tk == ":=") {
    return AST::Declaration::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "in") {
    return AST::InDecl::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "`") {
    return AST::Generic::Build(std::forward<NPtrVec &&>(nodes));
  }

  if (tk == "=") {
    if (nodes[0]->is_declaration()) {
      if (((AST::Declaration *)nodes[0])->IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        ErrorLog::DoubleDeclAssignment(nodes[0]->loc, nodes[2]->loc);
        return steal<AST::Declaration>(nodes[0]);
      }

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

  if (tk == "'") {
    std::swap(binop_ptr->lhs, binop_ptr->rhs);
    tk = "(";
  }

  static const std::map<std::string, Language::Operator> symbols = {
      {"=>", Language::Operator::Rocket}, {"", Language::Operator::Cast},
      {"->", Language::Operator::Arrow},  {"|=", Language::Operator::OrEq},
      {"&=", Language::Operator::AndEq},  {"^=", Language::Operator::XorEq},
      {"+=", Language::Operator::AddEq},  {"-=", Language::Operator::SubEq},
      {"*=", Language::Operator::MulEq},  {"/=", Language::Operator::DivEq},
      {"%=", Language::Operator::ModEq},  {"..", Language::Operator::Dots},
      {"+", Language::Operator::Add},     {"-", Language::Operator::Sub},
      {"*", Language::Operator::Mul},     {"/", Language::Operator::Div},
      {"%", Language::Operator::Mod},     {"[", Language::Operator::Index},
      {"(", Language::Operator::Call}};
  auto iter = symbols.find(tk);
  if (iter != symbols.end()) { binop_ptr->op = iter->second; }

  binop_ptr->precedence = Language::precedence(binop_ptr->op);
  return binop_ptr;
}

AST::Node *BuildKWBlock(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = ((AST::TokenNode *)nodes[0])->token;

  if (tk == "case") {
    return AST::Case::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "enum") {
    return AST::BuildEnumLiteral(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "struct") {
    return AST::BuildStructLiteral(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "scope") {
    return AST::BuildScopeLiteral(std::forward<NPtrVec &&>(nodes));
  }

  UNREACHABLE;
}

AST::Node *BuildKWExprBlock(NPtrVec &&nodes) {
  assert(nodes[0]->is_token_node());
  auto tk = ((AST::TokenNode *)nodes[0])->token;

  if (tk == "for") {
    return AST::For::Build(std::forward<NPtrVec &&>(nodes));

  } else if (tk == "struct") {
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
