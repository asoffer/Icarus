#include "ast.h"

#include <queue>
#include <unordered_map>

#include "../base/source.h"
#include "../error_log.h"
#include "../scope.h"
#include "../type/type.h"

extern std::queue<Source::Name> file_queue;

namespace Language {
extern size_t precedence(Operator op);
} // namespace Language

// Input guarantees:
// [expr] [l_paren] [r_paren]
//
// Internal checks:
// Operand is not a declaration
base::owned_ptr<AST::Node>
BuildEmptyParen(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto call  = base::make_owned<AST::Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = base::move<AST::Expression>(nodes[0]);

  if (call->fn_->is<AST::Declaration>()) {
    ErrorLog::CallingDeclaration(call->fn_->span);
  }
  return call;
}

namespace AST {
// Input guarantees:
// [struct] [braced_statements]
//
// Internal checks:
// Each statement is a valid declaration
static base::owned_ptr<Node>
BuildStructLiteral(std::vector<base::owned_ptr<Node>> nodes) {
  static size_t anon_struct_counter = 0;

  auto struct_type =
      new Struct("__anon.struct" + std::to_string(anon_struct_counter++));
  for (auto &stmt : nodes[1]->as<Statements>().statements) {
    if (stmt->is<Declaration>()) {
      struct_type->decls.push_back(ptr_cast<Declaration>(stmt.release()));
    } else {
      // TODO show the entire struct declaration and point to the problematic
      // lines.
      ErrorLog::NonDeclInStructDecl(stmt->span);
    }
  }
  return base::make_owned<Terminal>(TextSpan(nodes[0]->span, nodes[1]->span),
                                    IR::Val::Type(struct_type));
}

static base::owned_ptr<Node>
BuildScopeLiteral(std::vector<base::owned_ptr<Node>> nodes) {
  auto scope_lit =
      base::make_owned<ScopeLiteral>(TextSpan(nodes[0]->span, nodes[1]->span));

  // TODO take arguments as well
  if (nodes.size() > 1) {
    for (auto &stmt : nodes[1]->as<Statements>().statements) {
      if (!stmt->is<Declaration>()) { continue; }
      auto decl = base::move<Declaration>(stmt);
      if (decl->identifier->token == "enter") {
        scope_lit->enter_fn = std::move(decl);
      } else if (decl->identifier->token == "exit") {
        scope_lit->exit_fn = std::move(decl);
      }
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

// Input guarantees:
// [enum] [braced_statements]
//
// Internal checks:
// Each statement is an identifier. No identifier is repeated.
static base::owned_ptr<Node>
BuildEnumLiteral(std::vector<base::owned_ptr<Node>> nodes) {
  std::vector<std::string> members;
  if (nodes[1]->is<Statements>()) {
    for (auto &stmt : nodes[1]->as<Statements>().statements) {
      if (stmt->is<Identifier>()) {
        // Quadratic but we need it as a vector eventually anyway because we do
        // care about the order the user put it in.
        auto token = std::move(stmt->as<Identifier>().token);
        for (const auto &member : members) {
          if (member == token) {
            ErrorLog::RepeatedEnumName(stmt->span);
            goto skip_adding_member;
          }
        }
        members.push_back(std::move(token));

      } else {
        ErrorLog::EnumNeedsIds(stmt->span);
      }
    skip_adding_member:;
    }
  }

  static size_t anon_enum_counter = 0;
  return base::make_owned<Terminal>(
      TextSpan(nodes[0]->span, nodes[1]->span),
      IR::Val::Type(new Enum(
          "__anon.enum" + std::to_string(anon_enum_counter++), members)));
}

// Input guarantees:
// [case] [braced_statements]
//
// Internal checks:
// Each statement is a binary operator using '=>'. The last one has a
// left-hand
// side of 'else'
base::owned_ptr<Node> Case::Build(std::vector<base::owned_ptr<Node>> nodes) {
  auto case_ptr  = base::make_owned<Case>();
  case_ptr->span = TextSpan(nodes[0]->span, nodes[1]->span);

  for (auto &stmt : nodes[1]->as<Statements>().statements) {
    if (stmt->is<Binop>() &&
        stmt->as<Binop>().op == Language::Operator::Rocket) {
      auto *binop = &stmt->as<Binop>();
      // TODO check for 'else' and make sure it's the last one.
      case_ptr->key_vals.emplace_back(std::move(binop->lhs),
                                      std::move(binop->rhs));
    } else {
      ErrorLog::NonKVInCase(stmt->span);
    }
  }
  return case_ptr;
}

static void
CheckForLoopDeclaration(base::owned_ptr<Expression> maybe_decl,
                        std::vector<base::owned_ptr<InDecl>> *iters) {
  if (!maybe_decl->is<InDecl>()) {
    ErrorLog::NonInDeclInForLoop(maybe_decl->span);
  } else {
    iters->push_back(base::move<InDecl>(maybe_decl));
  }
}

// Input guarantees:
// [for] [expression] [braced_statements]
//
// Internal checks:
// [expression] is either an in-declaration or a list of in-declarations
base::owned_ptr<Node> For::Build(std::vector<base::owned_ptr<Node>> nodes) {
  auto for_stmt        = base::make_owned<For>();
  for_stmt->span       = TextSpan(nodes[0]->span, nodes[2]->span);
  for_stmt->statements = base::move<Statements>(nodes[2]);

  auto iter = ptr_cast<Expression>(nodes[1].get());
  if (iter->is<CommaList>()) {
    auto iter_list = ptr_cast<CommaList>(iter);
    for_stmt->iterators.reserve(iter_list->exprs.size());

    for (auto &expr : iter_list->exprs) {
      CheckForLoopDeclaration(std::move(expr), &for_stmt->iterators);
    }
  } else {
    CheckForLoopDeclaration(base::move<Expression>(nodes[1]),
                            &for_stmt->iterators);
  }

  auto stmts  = base::make_owned<Statements>();
  stmts->span = for_stmt->span;
  stmts->statements.push_back(std::move(for_stmt));
  return stmts;
}

// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration.
// Operand cannot be an assignment of any kind.
base::owned_ptr<Node>
Unop::BuildLeft(std::vector<base::owned_ptr<Node>> nodes) {
  const std::string &tk = nodes[0]->as<TokenNode>().token;

  auto unop     = base::make_owned<Unop>();
  unop->operand = base::move<Expression>(nodes[1]);
  unop->span    = TextSpan(nodes[0]->span, unop->operand->span);

  bool check_id = false;
  if (tk == "require") {
    if (unop->operand->is<Terminal>()) {
      file_queue.push(Source::Name(
          std::move(std::get<std::string>(unop->operand->value.value))));
    } else {
      ErrorLog::InvalidRequirement(unop->operand->span);
    }

    unop->op = Language::Operator::Require;

  } else {
    const static std::unordered_map<std::string,
                                    std::pair<Language::Operator, bool>>
        UnopMap = {{"return", {Language::Operator::Return, false}},
                   {"break", {Language::Operator::Break, true}},
                   {"continue", {Language::Operator::Continue, true}},
                   {"restart", {Language::Operator::Restart, true}},
                   {"repeat", {Language::Operator::Repeat, true}},
                   {"free", {Language::Operator::Free, false}},
                   {"generate", {Language::Operator::Generate, false}},
                   {"print", {Language::Operator::Print, false}},
                   {"needs", {Language::Operator::Needs, false}},
                   {"ensure", {Language::Operator::Ensure, false}},
                   {"*", {Language::Operator::Mul, false}},
                   {"&", {Language::Operator::And, false}},
                   {"-", {Language::Operator::Sub, false}},
                   {"!", {Language::Operator::Not, false}},
                   {"@", {Language::Operator::At, false}},
                   {"$", {Language::Operator::Eval, false}}};
    auto iter = UnopMap.find(tk);
    ASSERT(iter != UnopMap.end(),
           std::string("Failed to match token: \"") + tk + "\"");
    std::tie(unop->op, check_id) = iter->second;
  }

  unop->precedence = Language::precedence(unop->op);

  if (check_id) {
    if (!unop->operand->is<Identifier>()) {
      // TODO clean up error message
      ErrorLog::NonIdJumpOperand(unop->operand->span);
    }
  } else {
    if (unop->operand->is<Declaration>()) {
      // TODO clean up this error message
      ErrorLog::InvalidDecl(unop->operand->span);
    }
  }
  return unop;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
base::owned_ptr<Node> ChainOp::Build(std::vector<base::owned_ptr<Node>> nodes) {
  auto op      = nodes[1]->as<TokenNode>().op;
  auto op_prec = Language::precedence(op);
  base::owned_ptr<ChainOp> chain;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  if (nodes[0]->is<ChainOp>() &&
      nodes[0]->as<ChainOp>().precedence == op_prec) {

    chain = base::move<ChainOp>(nodes[0]);

  } else {
    chain       = base::make_owned<ChainOp>();
    chain->span = TextSpan(nodes[0]->span, nodes[2]->span);

    chain->exprs.push_back(base::move<Expression>(nodes[0]));
    chain->precedence = op_prec;
  }

  chain->ops.push_back(op);
  chain->exprs.push_back(base::move<Expression>(nodes[2]));

  return chain;
}

base::owned_ptr<Node>
CommaList::Build(std::vector<base::owned_ptr<Node>> nodes) {
  base::owned_ptr<CommaList> comma_list;

  if (nodes[0]->is<CommaList>()) {
    comma_list = base::move<CommaList>(nodes[0]);
  } else {
    comma_list       = base::make_owned<CommaList>();
    comma_list->span = TextSpan(nodes[0]->span, nodes[2]->span);
    comma_list->exprs.push_back(base::move<Expression>(nodes[0]));
  }

  comma_list->exprs.push_back(base::move<Expression>(nodes[2]));

  return comma_list;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
base::owned_ptr<Node> Access::Build(std::vector<base::owned_ptr<Node>> nodes) {
  auto access     = base::make_owned<Access>();
  access->span    = TextSpan(nodes[0]->span, nodes[2]->span);
  access->operand = base::move<Expression>(nodes[0]);

  if (access->operand->is<Declaration>()) {
    ErrorLog::LHSDecl(access->operand->span);
  }

  if (!nodes[2]->is<Identifier>()) {
    ErrorLog::RHSNonIdInAccess(nodes[2]->span);
  } else {
    access->member_name =
        std::move(ptr_cast<Identifier>(nodes[2].get())->token);
  }
  return access;
}

// Input guarantees
// [expr] [l_paren] [expr] [r_paren]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
base::owned_ptr<Node>
Call::Build(std::vector<base::owned_ptr<Node>> nodes) {
  auto call  = base::make_owned<Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = base::move<Expression>(nodes[0]);

  if (nodes[2]->is<CommaList>()) {
    bool seen_named = false;
    for (auto &expr : nodes[2]->as<CommaList>().exprs) {
      if (expr->is<Binop>() &&
          expr->as<Binop>().op == Language::Operator::Assign) {
        seen_named = true;
        call->named_.emplace_back(
            std::move(expr->as<Binop>().lhs->as<Identifier>().token),
            std::move(expr->as<Binop>().rhs));
      } else {
        if (seen_named) { ErrorLog::LogGeneric(TextSpan(), "TODO"); }
        call->pos_.push_back(std::move(expr));
      }
    }
  } else {
    if (nodes[2]->is<Binop>() &&
        nodes[2]->as<Binop>().op == Language::Operator::Assign) {
      call->named_.emplace_back(
          std::move(nodes[2]->as<Binop>().lhs->as<Identifier>().token),
          std::move(nodes[2]->as<Binop>().rhs));
    } else {
      call->pos_.push_back(base::move<Expression>(nodes[2]));
    }
  }
  call->precedence = Language::precedence(Language::Operator::Call);

  if (call->fn_->is<Declaration>()) { ErrorLog::LHSDecl(call->fn_->span); }
  return call;
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
base::owned_ptr<Node>
Binop::BuildIndexOperator(std::vector<base::owned_ptr<Node>> nodes) {
  auto binop        = base::make_owned<Binop>();
  binop->span       = TextSpan(nodes[0]->span, nodes[2]->span);
  binop->lhs        = base::move<Expression>(nodes[0]);
  binop->rhs        = base::move<Expression>(nodes[2]);
  binop->op         = Language::Operator::Index;
  binop->precedence = Language::precedence(binop->op);

  if (binop->lhs->is<Declaration>()) { ErrorLog::LHSDecl(binop->lhs->span); }

  if (binop->rhs->is<Declaration>()) {
    // TODO Tick is no longer a thing
    ErrorLog::RHSNonTickDecl(binop->rhs->span);
  }

  return binop;
}

// Input guarantee:
// [expression] [l_bracket] [r_bracket]
//
// Internal checks: None
base::owned_ptr<Node>
ArrayLiteral::BuildEmpty(std::vector<base::owned_ptr<Node>> nodes) {
  auto array_lit  = base::make_owned<ArrayLiteral>();
  array_lit->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return array_lit;
}

// Input guarantee:
// [expression] [dots]
//
// Internal checks: None
base::owned_ptr<Node>
Unop::BuildDots(std::vector<base::owned_ptr<Node>> nodes) {
  auto unop        = base::make_owned<Unop>();
  unop->operand    = base::move<Expression>(nodes[0]);
  unop->span       = TextSpan(nodes[0]->span, nodes[1]->span);
  unop->op         = nodes[1]->as<TokenNode>().op;
  unop->precedence = Language::precedence(unop->op);
  return unop;
}

base::owned_ptr<Node>
ArrayLiteral::build(std::vector<base::owned_ptr<Node>> nodes) {
  auto array_lit  = base::make_owned<ArrayLiteral>();
  array_lit->span = nodes[0]->span;

  if (nodes[1]->is<CommaList>()) {
    array_lit->elems = std::move(ptr_cast<CommaList>(nodes[1].get())->exprs);
  } else {
    array_lit->elems.push_back(base::move<Expression>(nodes[1]));
  }

  return array_lit;
}

base::owned_ptr<Node>
ArrayType::build(std::vector<base::owned_ptr<Node>> nodes) {
  if (nodes[1]->is<CommaList>()) {
    auto *length_chain = ptr_cast<CommaList>(nodes[1].get());
    int i              = static_cast<int>(length_chain->exprs.size() - 1);
    auto prev          = base::move<Expression>(nodes[3]);

    while (i >= 0) {
      auto array_type       = base::make_owned<ArrayType>();
      array_type->span      = length_chain->exprs[i]->span;
      array_type->length    = std::move(length_chain->exprs[i]);
      array_type->data_type = std::move(prev);
      prev                  = std::move(array_type);
      i -= 1;
    }
    return prev;

  } else {
    auto array_type       = base::make_owned<ArrayType>();
    array_type->span      = nodes[0]->span;
    array_type->length    = base::move<Expression>(nodes[1]);
    array_type->data_type = base::move<Expression>(nodes[3]);

    return array_type;
  }
}

base::owned_ptr<Node> InDecl::Build(std::vector<base::owned_ptr<Node>> nodes) {
  ASSERT(nodes[1]->as<TokenNode>().op == Language::Operator::In, "");
  auto in_decl              = base::make_owned<InDecl>();
  in_decl->span             = TextSpan(nodes[0]->span, nodes[2]->span);
  in_decl->identifier       = base::move<Identifier>(nodes[0]);
  in_decl->identifier->decl = in_decl.get();
  in_decl->container        = base::move<Expression>(nodes[2]);
  in_decl->precedence       = Language::precedence(Language::Operator::In);
  return in_decl;
}

base::owned_ptr<Node>
Declaration::Build(std::vector<base::owned_ptr<Node>> nodes, bool is_const) {
  auto op                = nodes[1]->as<TokenNode>().op;
  auto decl              = base::make_owned<Declaration>(is_const);
  decl->span             = TextSpan(nodes[0]->span, nodes[2]->span);
  decl->precedence       = Language::precedence(op);
  decl->identifier       = base::move<Identifier>(nodes[0]);
  decl->identifier->decl = decl.get();

  if (op == Language::Operator::Colon) {
    decl->type_expr = base::move<Expression>(nodes[2]);
  } else {
    decl->init_val = base::move<Expression>(nodes[2]);
  }

  return decl;
}

base::owned_ptr<Node>
FunctionLiteral::build(std::vector<base::owned_ptr<Node>> nodes) {
  auto fn_lit        = base::make_owned<FunctionLiteral>();
  fn_lit->span       = TextSpan(nodes[0]->span, nodes[1]->span);
  fn_lit->statements = base::move<Statements>(nodes[1]);

  auto binop               = ptr_cast<Binop>(nodes[0].get());
  fn_lit->return_type_expr = base::move<Expression>(binop->rhs);

  if (binop->lhs->is<Declaration>()) {
    fn_lit->inputs.push_back(base::move<Declaration>(binop->lhs));
    fn_lit->inputs.back()->arg_val = fn_lit.get();

  } else if (binop->lhs->is<CommaList>()) {
    auto decls = ptr_cast<CommaList>(binop->lhs.get());
    fn_lit->inputs.reserve(decls->exprs.size());

    for (auto &expr : decls->exprs) {
      fn_lit->inputs.push_back(base::move<Declaration>(expr));
      fn_lit->inputs.back()->arg_val = fn_lit.get();
    }
  }

  if (fn_lit->return_type_expr->is<Declaration>()) {
    fn_lit->return_type_expr->as<Declaration>().arg_val = fn_lit.get();
  }

  return fn_lit;
}

base::owned_ptr<Node>
Statements::build_one(std::vector<base::owned_ptr<Node>> nodes) {
  auto stmts  = base::make_owned<Statements>();
  stmts->span = nodes[0]->span;
  stmts->statements.push_back(std::move(nodes[0]));
  return stmts;
}

base::owned_ptr<Node>
Statements::build_more(std::vector<base::owned_ptr<Node>> nodes) {
  auto stmts = base::move<Statements>(nodes[0]);
  stmts->statements.push_back(std::move(nodes[1]));
  return stmts;
}

base::owned_ptr<Node> Jump::build(std::vector<base::owned_ptr<Node>> nodes) {
  const static std::unordered_map<std::string, JumpType> JumpTypeMap = {
      {"break", JumpType::Break},     {"continue", JumpType::Continue},
      {"return", JumpType::Return},   {"repeat", JumpType::Repeat},
      {"restart", JumpType::Restart},
  };
  auto iter = JumpTypeMap.find(ptr_cast<TokenNode>(nodes[0].get())->token);
  ASSERT(iter != JumpTypeMap.end(), "");

  auto stmts  = base::make_owned<Statements>();
  stmts->span = nodes[0]->span;
  stmts->statements.push_back(
      base::make_owned<Jump>(nodes[0]->span, iter->second));
  return stmts;
}

base::owned_ptr<Node>
ScopeNode::BuildScopeNode(base::owned_ptr<Expression> scope_name,
                          base::owned_ptr<Expression> arg_expr,
                          base::owned_ptr<Statements> stmt_node) {
  auto scope_node        = base::make_owned<ScopeNode>();
  scope_node->span       = TextSpan(scope_name->span, stmt_node->span);
  scope_node->scope_expr = std::move(scope_name);
  scope_node->expr       = std::move(arg_expr);
  scope_node->stmts      = std::move(stmt_node);
  return scope_node;
}

base::owned_ptr<Node>
ScopeNode::Build(std::vector<base::owned_ptr<Node>> nodes) {
  return BuildScopeNode(base::move<Expression>(nodes[0]),
                        base::move<Expression>(nodes[1]),
                        base::move<Statements>(nodes[2]));
}

base::owned_ptr<Node>
ScopeNode::BuildVoid(std::vector<base::owned_ptr<Node>> nodes) {
  return BuildScopeNode(base::move<Expression>(nodes[0]), nullptr,
                        base::move<Statements>(nodes[1]));
}
} // namespace AST

base::owned_ptr<AST::Node>
BracedStatements(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ASSERT(nodes[1]->is<AST::Statements>(), "");
  return std::move(nodes[1]);
}

base::owned_ptr<AST::Node> AST::CodeBlock::BuildFromStatements(
    std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto block = base::make_owned<CodeBlock>();
  // TODO block->value
  block->span  = TextSpan(nodes[0]->span, nodes[2]->span);
  block->stmts = base::move<AST::Statements>(nodes[1]);
  return block;
}

base::owned_ptr<AST::Node>
OneBracedStatement(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::make_owned<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->statements.push_back(std::move(nodes[1]));
  return stmts;
}

base::owned_ptr<AST::Node>
BracedStatementsSameLineEnd(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::move<AST::Statements>(nodes[1]);
  stmts->span = TextSpan(nodes[0]->span, nodes[3]->span);
  if (nodes[2]->is<AST::Statements>()) {
    for (auto &stmt : ptr_cast<AST::Statements>(nodes[2].get())->statements) {
      stmts->statements.push_back(std::move(stmt));
    }
  } else {
    stmts->statements.push_back(std::move(nodes[2]));
  }
  return stmts;
}

base::owned_ptr<AST::Node> AST::CodeBlock::BuildFromStatementsSameLineEnd(
    std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto block = base::make_owned<CodeBlock>();
  // TODO block->value
  block->span  = TextSpan(nodes[0]->span, nodes[3]->span);
  block->stmts = base::move<AST::Statements>(
      BracedStatementsSameLineEnd(std::move(nodes)));
  return block;
}

base::owned_ptr<AST::Node> AST::CodeBlock::BuildFromOneStatement(
    std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto block = base::make_owned<CodeBlock>();
  // TODO block->value
  block->span = TextSpan(nodes[0]->span, nodes[2]->span);
  block->stmts =
      base::move<AST::Statements>(OneBracedStatement(std::move(nodes)));
  return block;
}

base::owned_ptr<AST::Node>
EmptyBraces(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::make_owned<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return stmts;
}

base::owned_ptr<AST::Node>
AST::CodeBlock::BuildEmpty(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto block = base::make_owned<CodeBlock>();
  // TODO block->value
  block->span  = TextSpan(nodes[0]->span, nodes[1]->span);
  block->stmts = base::move<AST::Statements>(EmptyBraces(std::move(nodes)));
  return block;
}

base::owned_ptr<AST::Node>
BuildBinaryOperator(std::vector<base::owned_ptr<AST::Node>> nodes) {
  static const std::unordered_map<std::string, Language::Operator> chain_ops = {
      {",", Language::Operator::Comma}, {"==", Language::Operator::Eq},
      {"!=", Language::Operator::Ne},   {"<", Language::Operator::Lt},
      {">", Language::Operator::Gt},    {"<=", Language::Operator::Le},
      {">=", Language::Operator::Ge},   {"&", Language::Operator::And},
      {"|", Language::Operator::Or},    {"^", Language::Operator::Xor},
  };

  const std::string &tk = ptr_cast<AST::TokenNode>(nodes[1].get())->token;
  {
    auto iter = chain_ops.find(tk);
    if (iter != chain_ops.end()) {
      ptr_cast<AST::TokenNode>(nodes[1].get())->op = iter->second;
      return (iter->second == Language::Operator::Comma)
                 ? AST::CommaList::Build(std::move(nodes))
                 : AST::ChainOp::Build(std::move(nodes));
    }
  }

  if (tk == ".") {
    return AST::Access::Build(std::move(nodes));

  } else if (tk == ":" || tk == ":=") {
    return AST::Declaration::Build(std::move(nodes), /* const = */ false);

  } else if (tk == "::" || tk == "::=") {
    return AST::Declaration::Build(std::move(nodes), /* const = */ true);

  } else if (tk == "in") {
    return AST::InDecl::Build(std::move(nodes));
  }

  if (tk == "=") {
    if (nodes[0]->is<AST::Declaration>()) {
      if (ptr_cast<AST::Declaration>(nodes[0].get())->IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        ErrorLog::DoubleDeclAssignment(nodes[0]->span, nodes[2]->span);
        return base::move<AST::Declaration>(nodes[0]);
      }

      auto decl      = base::move<AST::Declaration>(nodes[0]);
      decl->init_val = base::move<AST::Expression>(nodes[2]);
      return decl;

    } else {
      auto binop  = base::make_owned<AST::Binop>();
      binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

      binop->lhs = base::move<AST::Expression>(nodes[0]);
      binop->rhs = base::move<AST::Expression>(nodes[2]);

      binop->op         = Language::Operator::Assign;
      binop->precedence = Language::precedence(binop->op);
      return binop;
    }
  }

  if (tk == "(") {  // TODO these should just generate Call::Build directly.
    return AST::Call::Build(std::move(nodes));
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[2]);
    return AST::Call::Build(std::move(nodes));
  }

  auto binop  = base::make_owned<AST::Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[1]->span);

  binop->lhs = base::move<AST::Expression>(nodes[0]);
  binop->rhs = base::move<AST::Expression>(nodes[2]);

  static const std::unordered_map<std::string, Language::Operator> symbols = {
      {"=>", Language::Operator::Rocket}, {"", Language::Operator::Cast},
      {"->", Language::Operator::Arrow},  {"|=", Language::Operator::OrEq},
      {"&=", Language::Operator::AndEq},  {"^=", Language::Operator::XorEq},
      {"+=", Language::Operator::AddEq},  {"-=", Language::Operator::SubEq},
      {"*=", Language::Operator::MulEq},  {"/=", Language::Operator::DivEq},
      {"%=", Language::Operator::ModEq},  {"..", Language::Operator::Dots},
      {"+", Language::Operator::Add},     {"-", Language::Operator::Sub},
      {"*", Language::Operator::Mul},     {"/", Language::Operator::Div},
      {"%", Language::Operator::Mod},     {"[", Language::Operator::Index}};
  {
    auto iter = symbols.find(tk);
    if (iter != symbols.end()) { binop->op = iter->second; }

    binop->precedence = Language::precedence(binop->op);
    return binop;
  }
}

base::owned_ptr<AST::Node>
BuildKWBlock(std::vector<base::owned_ptr<AST::Node>> nodes) {
  const std::string &tk = ptr_cast<AST::TokenNode>(nodes[0].get())->token;

  if (tk == "case") {
    return AST::Case::Build(std::move(nodes));

  } else if (tk == "enum") {
    return AST::BuildEnumLiteral(std::move(nodes));

  } else if (tk == "struct") {
    return AST::BuildStructLiteral(std::move(nodes));

  } else if (tk == "scope") {
    return AST::BuildScopeLiteral(std::move(nodes));
  }

  UNREACHABLE();
}

base::owned_ptr<AST::Node>
BuildKWExprBlock(std::vector<base::owned_ptr<AST::Node>> nodes) {
  const std::string &tk = ptr_cast<AST::TokenNode>(nodes[0].get())->token;

  if (tk == "for") {
    return AST::For::Build(std::move(nodes));

  } else if (tk == "struct") {
    // Parmaetric struct
    NOT_YET();
  }

  UNREACHABLE();
}

base::owned_ptr<AST::Node>
Parenthesize(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto expr        = base::move<AST::Expression>(nodes[1]);
  expr->precedence = Language::precedence(Language::Operator::NotAnOperator);
  if (ptr_cast<AST::TokenNode>(nodes[0].get())->token != "\\(") {
    return expr;

  } else {
    auto unop     = base::make_owned<AST::Unop>();
    unop->operand = std::move(expr);
    unop->span    = TextSpan(nodes[0]->span, nodes[2]->span);
    unop->op      = Language::Operator::Ref;
    return unop;
  }
}
