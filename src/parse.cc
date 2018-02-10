#include "ast/ast.h"
#include "base/debug.h"
#include "base/types.h"
#include "error_log.h"
#include "nnt.h"
#include "operators.h"
#include "util/timer.h"
#include <cstdio>
#include <queue>
#include <array>
#include <vector>
#include <iosfwd>

extern std::queue<Source::Name> file_queue;

static void ValidateStatementSyntax(AST::Node *node) {
  if (node->is<AST::CommaList>()) {
    ErrorLog::CommaListStatement(node->as<AST::CommaList>().span);
    node->limit_to(AST::StageRange::NoEmitIR());
  }
}

namespace Language {
size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Operator::name:                                                         \
    return (((prec) << 2) + (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
  default: UNREACHABLE();
  }
}
} // namespace Language

static base::owned_ptr<AST::Node>
OneBracedStatement(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::make_owned<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->statements.push_back(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->statements.back().get());
  return stmts;
}

static base::owned_ptr<AST::Node>
EmptyBraces(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::make_owned<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return stmts;
}

static base::owned_ptr<AST::Node>
BracedStatementsSameLineEnd(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::move<AST::Statements>(nodes[1]);
  stmts->span = TextSpan(nodes[0]->span, nodes[3]->span);
  if (nodes[2]->is<AST::Statements>()) {
    for (auto &stmt : nodes[2]->as<AST::Statements>().statements) {
      stmts->statements.push_back(std::move(stmt));
      ValidateStatementSyntax(stmts->statements.back().get());
    }
  } else {
    stmts->statements.push_back(std::move(nodes[2]));
    ValidateStatementSyntax(stmts->statements.back().get());
  }
  return stmts;
}

namespace AST {
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
static base::owned_ptr<Node>
BuildFor(std::vector<base::owned_ptr<Node>> nodes) {
  auto for_stmt        = base::make_owned<For>();
  for_stmt->span       = TextSpan(nodes[0]->span, nodes[2]->span);
  for_stmt->statements = base::move<Statements>(nodes[2]);

  auto iter = &nodes[1]->as<Expression>();
  if (iter->is<CommaList>()) {
    auto iter_list = &iter->as<CommaList>();
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
static base::owned_ptr<Node>
BuildLeftUnop(std::vector<base::owned_ptr<Node>> nodes) {
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
    auto iter   = UnopMap.find(tk);
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
static base::owned_ptr<Node>
BuildChainOp(std::vector<base::owned_ptr<Node>> nodes) {
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

static base::owned_ptr<Node>
BuildCommaList(std::vector<base::owned_ptr<Node>> nodes) {
  base::owned_ptr<CommaList> comma_list;

  if (nodes[0]->is<CommaList>()) {
    comma_list = base::move<CommaList>(nodes[0]);
  } else {
    comma_list       = base::make_owned<CommaList>();
    comma_list->span = TextSpan(nodes[0]->span, nodes[2]->span);
    comma_list->exprs.push_back(base::move<Expression>(nodes[0]));
  }

  comma_list->exprs.push_back(base::move<Expression>(nodes[2]));
  comma_list->span.finish = comma_list->exprs.back()->span.finish;

  return comma_list;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
base::owned_ptr<Node> BuildAccess(std::vector<base::owned_ptr<Node>> nodes) {
  auto access     = base::make_owned<Access>();
  access->span    = TextSpan(nodes[0]->span, nodes[2]->span);
  access->operand = base::move<Expression>(nodes[0]);

  if (access->operand->is<Declaration>()) {
    ErrorLog::DeclNotAllowed(access->operand->span);
  }

  if (!nodes[2]->is<Identifier>()) {
    ErrorLog::RHSNonIdInAccess(nodes[2]->span);
  } else {
    access->member_name = std::move(nodes[2]->as<Identifier>().token);
  }
  return access;
}

// Input guarantees
// [expr] [l_paren] [expr] [r_paren]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
base::owned_ptr<Node> BuildCall(std::vector<base::owned_ptr<Node>> nodes) {
  auto call  = base::make_owned<Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = base::move<Expression>(nodes[0]);

  if (nodes[2]->is<CommaList>()) {
    bool seen_named = false;
    for (auto &expr : nodes[2]->as<CommaList>().exprs) {
      if (expr->is<Binop>() &&
          expr->as<Binop>().op == Language::Operator::Assign) {
        seen_named = true;
        call->args_.named_.emplace(
            std::move(expr->as<Binop>().lhs->as<Identifier>().token),
            std::move(expr->as<Binop>().rhs));
      } else {
        if (seen_named) { ErrorLog::LogGeneric(TextSpan(), "TODO"); }
        call->args_.pos_.push_back(std::move(expr));
      }
    }
  } else {
    if (nodes[2]->is<Binop>() &&
        nodes[2]->as<Binop>().op == Language::Operator::Assign) {
      call->args_.named_.emplace(
          std::move(nodes[2]->as<Binop>().lhs->as<Identifier>().token),
          std::move(nodes[2]->as<Binop>().rhs));
    } else {
      call->args_.pos_.push_back(base::move<Expression>(nodes[2]));
    }
  }
  call->precedence = Language::precedence(Language::Operator::Call);

  if (call->fn_->is<Declaration>()) {
    ErrorLog::DeclNotAllowed(call->fn_->span);
  }
  return call;
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
static base::owned_ptr<Node>
BuildIndexOperator(std::vector<base::owned_ptr<Node>> nodes) {
  auto binop        = base::make_owned<Binop>();
  binop->span       = TextSpan(nodes[0]->span, nodes[2]->span);
  binop->lhs        = base::move<Expression>(nodes[0]);
  binop->rhs        = base::move<Expression>(nodes[2]);
  binop->op         = Language::Operator::Index;
  binop->precedence = Language::precedence(binop->op);

  if (binop->lhs->is<Declaration>()) {
    ErrorLog::DeclNotAllowed(binop->lhs->span);
  }

  if (binop->rhs->is<Declaration>()) {
    ErrorLog::DeclNotAllowed(binop->rhs->span);
  }

  return binop;
}

// Input guarantee:
// [expression] [l_bracket] [r_bracket]
//
// Internal checks: None
static base::owned_ptr<Node>
BuildEmptyArray(std::vector<base::owned_ptr<Node>> nodes) {
  auto array_lit  = base::make_owned<ArrayLiteral>();
  array_lit->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return array_lit;
}

// Input guarantee:
// [expression] [dots]
//
// Internal checks: None
static base::owned_ptr<Node>
BuildDots(std::vector<base::owned_ptr<Node>> nodes) {
  auto unop        = base::make_owned<Unop>();
  unop->operand    = base::move<Expression>(nodes[0]);
  unop->span       = TextSpan(nodes[0]->span, nodes[1]->span);
  unop->op         = nodes[1]->as<TokenNode>().op;
  unop->precedence = Language::precedence(unop->op);
  return unop;
}

static base::owned_ptr<Node>
BuildArrayLiteral(std::vector<base::owned_ptr<Node>> nodes) {
  auto array_lit  = base::make_owned<ArrayLiteral>();
  array_lit->span = nodes[0]->span;

  if (nodes[1]->is<CommaList>()) {
    array_lit->elems = std::move(nodes[1]->as<CommaList>().exprs);
  } else {
    array_lit->elems.push_back(base::move<Expression>(nodes[1]));
  }

  return array_lit;
}

static base::owned_ptr<Node>
BuildArrayType(std::vector<base::owned_ptr<Node>> nodes) {
  if (nodes[1]->is<CommaList>()) {
    auto *length_chain = &nodes[1]->as<CommaList>();
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

static base::owned_ptr<Node>
BuildInDecl(std::vector<base::owned_ptr<Node>> nodes) {
  ASSERT(nodes[1]->as<TokenNode>().op == Language::Operator::In, "");
  auto in_decl              = base::make_owned<InDecl>();
  in_decl->span             = TextSpan(nodes[0]->span, nodes[2]->span);
  in_decl->identifier       = base::move<Identifier>(nodes[0]);
  in_decl->identifier->decl = in_decl.get();
  in_decl->container        = base::move<Expression>(nodes[2]);
  in_decl->precedence       = Language::precedence(Language::Operator::In);
  return in_decl;
}

static base::owned_ptr<Node>
BuildDeclaration(std::vector<base::owned_ptr<Node>> nodes, bool is_const) {
  auto op                = nodes[1]->as<TokenNode>().op;
  auto decl              = base::make_owned<Declaration>(is_const);
  decl->span             = TextSpan(nodes[0]->span, nodes[2]->span);
  decl->precedence       = Language::precedence(op);
  decl->identifier       = base::move<Identifier>(nodes[0]);
  decl->identifier->decl = decl.get();

  if (op == Language::Operator::Colon ||
      op == Language::Operator::DoubleColon) {
    decl->type_expr = base::move<Expression>(nodes[2]);
  } else {
    decl->init_val = base::move<Expression>(nodes[2]);
  }

  return decl;
}

static base::owned_ptr<Node>
BuildFunctionLiteral(std::vector<base::owned_ptr<Node>> nodes) {
  auto *binop = &nodes[0]->as<Binop>();
  std::vector<base::owned_ptr<Declaration>> inputs;
  bool generic = false;
  if (binop->lhs->is<Declaration>()) {
    inputs.push_back(base::move<Declaration>(binop->lhs));
    generic |= inputs.back()->const_;

  } else if (binop->lhs->is<CommaList>()) {
    auto *decls = &binop->lhs->as<CommaList>();
    inputs.reserve(decls->exprs.size());

    for (auto &expr : decls->exprs) {
      inputs.push_back(base::move<Declaration>(expr));
      generic |= inputs.back()->const_;
    }
  }

  FunctionLiteral *fn_lit =
      generic ? new GenericFunctionLiteral : new FunctionLiteral;
  for (auto &input : inputs) { input->arg_val = fn_lit; }

  fn_lit->inputs           = std::move(inputs);
  fn_lit->span             = TextSpan(nodes[0]->span, nodes[1]->span);
  fn_lit->statements       = base::move<Statements>(nodes[1]);
  fn_lit->return_type_expr = base::move<Expression>(binop->rhs);
  if (fn_lit->return_type_expr->is<Declaration>()) {
    fn_lit->return_type_expr->as<Declaration>().arg_val = fn_lit;
  }

  size_t i = 0;
  for (const auto &input : fn_lit->inputs) {
    fn_lit->lookup_[input->identifier->token] = i++;
  }

  return base::own(fn_lit);
}

static base::owned_ptr<Node>
BuildOneStatement(std::vector<base::owned_ptr<Node>> nodes) {
  auto stmts  = base::make_owned<Statements>();
  stmts->span = nodes[0]->span;
  stmts->statements.push_back(std::move(nodes[0]));
  ValidateStatementSyntax(stmts->statements.back().get());
  return stmts;
}

static base::owned_ptr<Node>
BuildMoreStatements(std::vector<base::owned_ptr<Node>> nodes) {
  auto stmts = base::move<Statements>(nodes[0]);
  stmts->statements.push_back(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->statements.back().get());
  return stmts;
}

static base::owned_ptr<Node>
BuildJump(std::vector<base::owned_ptr<Node>> nodes) {
  const static std::unordered_map<std::string, Jump::JumpType> JumpTypeMap = {
      {"break", Jump::JumpType::Break},
      {"continue", Jump::JumpType::Continue},
      {"return", Jump::JumpType::Return},
      {"repeat", Jump::JumpType::Repeat},
      {"restart", Jump::JumpType::Restart}};
  auto iter = JumpTypeMap.find(nodes[0]->as<TokenNode>().token);
  ASSERT(iter != JumpTypeMap.end(), "");

  auto stmts  = base::make_owned<Statements>();
  stmts->span = nodes[0]->span;
  stmts->statements.push_back(
      base::make_owned<Jump>(nodes[0]->span, iter->second));
  return stmts;
}

static base::owned_ptr<Node>
BuildScopeNode(base::owned_ptr<Expression> scope_name,
               base::owned_ptr<Expression> arg_expr,
               base::owned_ptr<Statements> stmt_node) {
  auto scope_node        = base::make_owned<ScopeNode>();
  scope_node->span       = TextSpan(scope_name->span, stmt_node->span);
  scope_node->scope_expr = std::move(scope_name);
  scope_node->expr       = std::move(arg_expr);
  scope_node->stmts      = std::move(stmt_node);
  return scope_node;
}

static base::owned_ptr<Node>
BuildScopeNode(std::vector<base::owned_ptr<Node>> nodes) {
  return BuildScopeNode(base::move<Expression>(nodes[0]),
                        base::move<Expression>(nodes[1]),
                        base::move<Statements>(nodes[2]));
}

static base::owned_ptr<Node>
BuildVoidScopeNode(std::vector<base::owned_ptr<Node>> nodes) {
  return BuildScopeNode(base::move<Expression>(nodes[0]), nullptr,
                        base::move<Statements>(nodes[1]));
}

static base::owned_ptr<Node>
BuildCodeBlockFromStatements(std::vector<base::owned_ptr<Node>> nodes) {
  auto block   = base::make_owned<CodeBlock>();
  block->span  = TextSpan(nodes[0]->span, nodes[2]->span);
  block->stmts = base::move<Statements>(nodes[1]);
  return block;
}

static base::owned_ptr<Node> BuildCodeBlockFromStatementsSameLineEnd(
    std::vector<base::owned_ptr<Node>> nodes) {
  auto block  = base::make_owned<CodeBlock>();
  block->span = TextSpan(nodes[0]->span, nodes[3]->span);
  block->stmts =
      base::move<Statements>(BracedStatementsSameLineEnd(std::move(nodes)));
  return block;
}

static base::owned_ptr<Node>
BuildCodeBlockFromOneStatement(std::vector<base::owned_ptr<Node>> nodes) {
  auto block   = base::make_owned<CodeBlock>();
  block->span  = TextSpan(nodes[0]->span, nodes[2]->span);
  block->stmts = base::move<Statements>(OneBracedStatement(std::move(nodes)));
  return block;
}

static base::owned_ptr<Node>
BuildEmptyCodeBlock(std::vector<base::owned_ptr<Node>> nodes) {
  auto block   = base::make_owned<CodeBlock>();
  block->span  = TextSpan(nodes[0]->span, nodes[1]->span);
  block->stmts = base::move<Statements>(EmptyBraces(std::move(nodes)));
  return block;
}
} // namespace AST

class Rule {
public:
  using OptVec = std::vector<u64>;
  using fnptr =
      base::owned_ptr<AST::Node> (*)(std::vector<base::owned_ptr<AST::Node>>);

  Rule(Language::NodeType output, const OptVec &input, fnptr fn)
      : output_(output), input_(input), fn_(fn) {}

  size_t size() const { return input_.size(); }

  bool match(const std::vector<Language::NodeType> &node_type_stack) const {
    // The stack needs to be long enough to match.
    if (input_.size() > node_type_stack.size()) return false;

    size_t stack_index = node_type_stack.size() - 1;
    size_t rule_index  = input_.size() - 1;

    // Iterate through backwards and exit as soon as you see a node whose
    // type does not match the rule.
    for (size_t i = 0; i < input_.size(); ++i, --rule_index, --stack_index) {
      if ((input_[rule_index] & node_type_stack[stack_index]) == 0) {
        return false;
      }
    }

    // If you complete the loop, there is a match.
    return true;
  }

  void apply(std::vector<base::owned_ptr<AST::Node>> *node_stack,
             std::vector<Language::NodeType> *node_type_stack) const {
    // Make a vector for the rule function to take as input. It will begin with
    // size() shared_ptrs.
    std::vector<base::owned_ptr<AST::Node>> nodes_to_reduce;
    nodes_to_reduce.reserve(this->size());
    for (auto i = node_stack->size() - this->size(); i < node_stack->size();
         ++i) {
      nodes_to_reduce.push_back(std::move((*node_stack)[i]));
    }
    node_type_stack->resize(node_stack->size() - this->size());
    node_stack->resize(node_stack->size() - this->size());

    node_stack->push_back(fn_(std::move(nodes_to_reduce)));
    node_type_stack->push_back(output_);
  }

private:
  Language::NodeType output_;
  OptVec input_;
  fnptr fn_;
};

namespace debug {
extern bool parser;
} // namespace debug

static base::owned_ptr<AST::Node>
BuildBinaryOperator(std::vector<base::owned_ptr<AST::Node>> nodes) {
  static const std::unordered_map<std::string, Language::Operator> chain_ops = {
      {",", Language::Operator::Comma}, {"==", Language::Operator::Eq},
      {"!=", Language::Operator::Ne},   {"<", Language::Operator::Lt},
      {">", Language::Operator::Gt},    {"<=", Language::Operator::Le},
      {">=", Language::Operator::Ge},   {"&", Language::Operator::And},
      {"|", Language::Operator::Or},    {"^", Language::Operator::Xor},
  };

  const std::string &tk = nodes[1]->as<AST::TokenNode>().token;
  {
    auto iter = chain_ops.find(tk);
    if (iter != chain_ops.end()) {
      nodes[1]->as<AST::TokenNode>().op = iter->second;
      return (iter->second == Language::Operator::Comma)
                 ? AST::BuildCommaList(std::move(nodes))
                 : AST::BuildChainOp(std::move(nodes));
    }
  }

  if (tk == ".") {
    return AST::BuildAccess(std::move(nodes));

  } else if (tk == ":" || tk == ":=") {
    return AST::BuildDeclaration(std::move(nodes), /* const = */ false);

  } else if (tk == "::" || tk == "::=") {
    return AST::BuildDeclaration(std::move(nodes), /* const = */ true);

  } else if (tk == "in") {
    return AST::BuildInDecl(std::move(nodes));
  }

  if (tk == "=") {
    if (nodes[0]->is<AST::Declaration>()) {
      if (nodes[0]->as<AST::Declaration>().IsInferred()) {
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

  if (tk == "(") { // TODO these should just generate BuildCall directly.
    return AST::BuildCall(std::move(nodes));
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[2]);
    return AST::BuildCall(std::move(nodes));
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

static base::owned_ptr<AST::Node>
BuildKWExprBlock(std::vector<base::owned_ptr<AST::Node>> nodes) {
  const std::string &tk = nodes[0]->as<AST::TokenNode>().token;

  if (tk == "for") {
    return AST::BuildFor(std::move(nodes));

  } else if (tk == "struct") {
    // Parmaetric struct
    NOT_YET();
  }

  UNREACHABLE();
}

static base::owned_ptr<AST::Node>
BuildEnumLiteral(std::vector<base::owned_ptr<AST::Node>> nodes, bool is_enum) {
  std::vector<std::string> members;
  if (nodes[1]->is<AST::Statements>()) {
    for (auto &stmt : nodes[1]->as<AST::Statements>().statements) {
      if (stmt->is<AST::Identifier>()) {
        // Quadratic but we need it as a vector eventually anyway because we do
        // care about the order the user put it in.
        auto &token = stmt->as<AST::Identifier>().token;
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
  return base::make_owned<AST::Terminal>(
      TextSpan(nodes[0]->span, nodes[1]->span),
      IR::Val::Type(
          new Enum("__anon.enum" + std::to_string(anon_enum_counter++),
                   std::move(members), is_enum)));
}

static base::owned_ptr<AST::Node>
BuildStructLiteral(std::vector<base::owned_ptr<AST::Node>> nodes) {
  static size_t anon_struct_counter = 0;

  auto struct_type =
      new Struct("__anon.struct" + std::to_string(anon_struct_counter++));
  for (auto &stmt : nodes[1]->as<AST::Statements>().statements) {
    if (stmt->is<AST::Declaration>()) {
      struct_type->decls.push_back(&stmt.release()->as<AST::Declaration>());
    } else {
      // TODO show the entire struct declaration and point to the problematic
      // lines.
      ErrorLog::NonDeclInStructDecl(stmt->span);
    }
  }
  return base::make_owned<AST::Terminal>(
      TextSpan(nodes[0]->span, nodes[1]->span), IR::Val::Type(struct_type));
}

static base::owned_ptr<AST::Node>
BuildScopeLiteral(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto scope_lit = base::make_owned<AST::ScopeLiteral>(
      TextSpan(nodes[0]->span, nodes[1]->span));

  // TODO take arguments as well
  if (nodes.size() > 1) {
    for (auto &stmt : nodes[1]->as<AST::Statements>().statements) {
      if (!stmt->is<AST::Declaration>()) { continue; }
      auto decl = base::move<AST::Declaration>(stmt);
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

static base::owned_ptr<AST::Node>
BuildCase(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto case_ptr  = base::make_owned<AST::Case>();
  case_ptr->span = TextSpan(nodes[0]->span, nodes[1]->span);

  for (auto &stmt : nodes[1]->as<AST::Statements>().statements) {
    if (stmt->is<AST::Binop>() &&
        stmt->as<AST::Binop>().op == Language::Operator::Rocket) {
      auto *binop = &stmt->as<AST::Binop>();
      // TODO check for 'else' and make sure it's the last one.
      case_ptr->key_vals.emplace_back(std::move(binop->lhs),
                                      std::move(binop->rhs));
    } else {
      ErrorLog::NonKVInCase(stmt->span);
    }
  }
  return case_ptr;
}

static base::owned_ptr<AST::Node>
BuildKWBlock(std::vector<base::owned_ptr<AST::Node>> nodes) {
  const std::string &tk = nodes[0]->as<AST::TokenNode>().token;

  if (tk == "case") {
    return BuildCase(std::move(nodes));

  } else if (bool is_enum = (tk == "enum"); is_enum || tk == "flags") {
    return BuildEnumLiteral(std::move(nodes), is_enum);

  } else if (tk == "struct") {
    return BuildStructLiteral(std::move(nodes));

  } else if (tk == "scope") {
    return BuildScopeLiteral(std::move(nodes));
  }

  UNREACHABLE();
}

static base::owned_ptr<AST::Node>
Parenthesize(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto expr        = base::move<AST::Expression>(nodes[1]);
  expr->precedence = Language::precedence(Language::Operator::NotAnOperator);
  if (nodes[0]->as<AST::TokenNode>().token != "\\(") {
    return expr;

  } else {
    auto unop     = base::make_owned<AST::Unop>();
    unop->operand = std::move(expr);
    unop->span    = TextSpan(nodes[0]->span, nodes[2]->span);
    unop->op      = Language::Operator::Ref;
    return unop;
  }
}

static base::owned_ptr<AST::Node>
BuildEmptyParen(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto call  = base::make_owned<AST::Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = base::move<AST::Expression>(nodes[0]);

  if (call->fn_->is<AST::Declaration>()) {
    ErrorLog::CallingDeclaration(call->fn_->span);
  }
  return call;
}

template <size_t N>
static base::owned_ptr<AST::Node>
drop_all_but(std::vector<base::owned_ptr<AST::Node>> nodes) {
  return std::move(nodes[N]);
}

static base::owned_ptr<AST::Node>
CombineColonEq(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto *tk_node = &nodes[0]->as<AST::TokenNode>();
  tk_node->token += "="; // Change : to := and :: to ::=
  tk_node->op = Language::Operator::ColonEq;
  return drop_all_but<0>(std::move(nodes));
}

base::owned_ptr<AST::Node>
EmptyFile(std::vector<base::owned_ptr<AST::Node>> nodes) {
  auto stmts  = base::make_owned<AST::Statements>();
  stmts->span = nodes[0]->span;
  return std::move(stmts);
}

namespace ErrMsg {
template <size_t RTN, size_t RES>
base::owned_ptr<AST::Node>
Reserved(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ErrorLog::Reserved(nodes[RES]->span, nodes[RES]->as<AST::TokenNode>().token);

  return base::make_owned<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

template <size_t RTN, size_t RES1, size_t RES2>
base::owned_ptr<AST::Node>
BothReserved(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ErrorLog::Reserved(nodes[RES1]->span,
                     nodes[RES1]->as<AST::TokenNode>().token);
  ErrorLog::Reserved(nodes[RES2]->span,
                     nodes[RES2]->as<AST::TokenNode>().token);
  return base::make_owned<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

base::owned_ptr<AST::Node>
NonBinop(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ErrorLog::NotBinary(nodes[1]->span, nodes[1]->as<AST::TokenNode>().token);
  return base::make_owned<AST::Identifier>(nodes[1]->span, "invalid_node");
}

template <size_t RTN, size_t RES>
base::owned_ptr<AST::Node>
NonBinopReserved(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ErrorLog::NotBinary(nodes[1]->span, nodes[1]->as<AST::TokenNode>().token);
  ErrorLog::Reserved(nodes[RES]->span, nodes[RES]->as<AST::TokenNode>().token);
  return base::make_owned<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

base::owned_ptr<AST::Node>
NonBinopBothReserved(std::vector<base::owned_ptr<AST::Node>> nodes) {
  ErrorLog::Reserved(nodes[0]->span, nodes[0]->as<AST::TokenNode>().token);
  ErrorLog::NotBinary(nodes[1]->span, nodes[1]->as<AST::TokenNode>().token);
  ErrorLog::Reserved(nodes[2]->span, nodes[2]->as<AST::TokenNode>().token);
  return base::make_owned<AST::Identifier>(nodes[1]->span, "invalid_node");
}
} // namespace ErrMsg

namespace Language {
static constexpr u64 OP_B = op_b | comma | dots | colon | eq;
static constexpr u64 EXPR = expr | fn_expr;
// Used in error productions only!
static constexpr u64 RESERVED = kw_expr_block | kw_block | kw_struct | op_lt;

// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
auto Rules = std::array{
    Rule(fn_expr, {EXPR, fn_arrow, EXPR}, BuildBinaryOperator),
    Rule(expr, {EXPR, (op_bl | OP_B), EXPR}, BuildBinaryOperator),
    Rule(op_b, {colon, eq}, CombineColonEq),
    Rule(fn_expr, {EXPR, fn_arrow, RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(fn_expr, {RESERVED, fn_arrow, EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(fn_expr, {RESERVED, fn_arrow, RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),
    Rule(expr, {EXPR, (OP_B | op_bl | dots), RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(expr, {RESERVED, (OP_B | op_bl), RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),
    Rule(expr, {EXPR, op_l, RESERVED}, ErrMsg::NonBinopReserved<1, 2>),
    Rule(expr, {RESERVED, op_l, RESERVED}, ErrMsg::NonBinopBothReserved),
    Rule(expr, {EXPR, l_paren, EXPR, r_paren}, AST::BuildCall),
    Rule(expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(expr, {EXPR, l_bracket, EXPR, r_bracket}, AST::BuildIndexOperator),
    Rule(expr, {l_bracket, r_bracket}, AST::BuildEmptyArray),
    Rule(expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket},
         AST::BuildArrayType),
    Rule(expr, {l_bracket, EXPR, semicolon, RESERVED, r_bracket},
         ErrMsg::Reserved<0, 3>),
    Rule(expr, {l_bracket, RESERVED, semicolon, EXPR, r_bracket},
         ErrMsg::Reserved<0, 1>),
    Rule(expr, {l_bracket, RESERVED, semicolon, RESERVED, r_bracket},
         ErrMsg::BothReserved<0, 1, 3>),
    Rule(bof, {bof, newline}, drop_all_but<0>),
    Rule(eof, {newline, eof}, drop_all_but<1>),
    Rule(prog, {bof, eof}, EmptyFile),
    Rule(prog, {bof, stmts, eof}, drop_all_but<1>),
    Rule(r_paren, {newline, r_paren}, drop_all_but<1>),
    Rule(r_bracket, {newline, r_bracket}, drop_all_but<1>),
    Rule(r_brace, {newline, r_brace}, drop_all_but<1>),
    Rule(r_double_brace, {newline, r_double_brace}, drop_all_but<1>),
    Rule(l_brace, {newline, l_brace}, drop_all_but<1>),
    Rule(l_double_brace, {newline, l_double_brace}, drop_all_but<1>),
    Rule(stmts, {newline, stmts}, drop_all_but<1>),
    Rule(r_paren, {r_paren, newline}, drop_all_but<0>),
    Rule(r_bracket, {r_bracket, newline}, drop_all_but<0>),
    Rule(r_brace, {r_brace, newline}, drop_all_but<0>),
    Rule(r_double_brace, {r_double_brace, newline}, drop_all_but<0>),
    Rule(braced_stmts, {l_brace, stmts, stmts | expr | fn_expr, r_brace},
         BracedStatementsSameLineEnd),
    Rule(braced_stmts, {l_brace, stmts, r_brace}, drop_all_but<1>),
    Rule(braced_stmts, {l_brace, r_brace}, EmptyBraces),
    Rule(braced_stmts, {l_brace, (expr | fn_expr), r_brace},
         OneBracedStatement),
    Rule(expr, {l_double_brace, stmts, stmts | expr | fn_expr, r_double_brace},
         AST::BuildCodeBlockFromStatementsSameLineEnd),
    Rule(expr, {l_double_brace, stmts, r_double_brace},
         AST::BuildCodeBlockFromStatements),
    Rule(expr, {l_double_brace, r_double_brace}, AST::BuildEmptyCodeBlock),
    Rule(expr, {l_double_brace, (expr | fn_expr), r_double_brace},
         AST::BuildCodeBlockFromOneStatement),
    Rule(expr, {fn_expr, braced_stmts}, AST::BuildFunctionLiteral),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case. For
    // example, "if(a)".
    Rule(expr, {EXPR, l_paren, RESERVED, r_paren}, ErrMsg::Reserved<0, 2>),
    Rule(expr, {EXPR, l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<0, 2>),

    Rule(expr, {(op_l | op_bl | op_lt), EXPR}, AST::BuildLeftUnop),
    Rule(expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {EXPR, dots}, AST::BuildDots),
    Rule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    Rule(expr, {l_bracket, EXPR, r_bracket}, AST::BuildArrayLiteral),
    Rule(expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),
    Rule(stmts, {stmts, (expr | fn_expr | stmts), newline},
         AST::BuildMoreStatements),
    Rule(expr, {(kw_block | kw_struct), braced_stmts}, BuildKWBlock),

    Rule(expr, {RESERVED, dots}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {(op_l | op_bl | op_lt), RESERVED}, ErrMsg::Reserved<0, 1>),
    Rule(expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    Rule(stmts, {(expr | fn_expr), (newline | eof)}, AST::BuildOneStatement),
    Rule(comma, {comma, newline}, drop_all_but<0>),
    Rule(l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(l_double_brace, {l_double_brace, newline}, drop_all_but<0>),
    Rule(stmts, {stmts, newline}, drop_all_but<0>),
    Rule(stmts, {kw_expr_block, EXPR, braced_stmts}, BuildKWExprBlock),
    Rule(expr, {kw_struct, EXPR, braced_stmts}, BuildKWExprBlock),

    Rule(expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(stmts, {op_lt}, AST::BuildJump),
    Rule(expr, {EXPR, EXPR, braced_stmts}, AST::BuildScopeNode),
    Rule(expr, {EXPR, braced_stmts}, AST::BuildVoidScopeNode),
};
} // namespace Language

extern NNT NextToken(SourceLocation &loc); // Defined in Lexer.cpp

enum class ShiftState : char { NeedMore, EndOfExpr, MustReduce };
std::ostream &operator<<(std::ostream &os, ShiftState s) {
  switch (s) {
  case ShiftState::NeedMore: return os << "NeedMore";
  case ShiftState::EndOfExpr: return os << "EndOfExpr";
  case ShiftState::MustReduce: return os << "MustReduce";
  default: UNREACHABLE();
  }
}

struct ParseState {
  ParseState(const SourceLocation &c) : lookahead_(nullptr, Language::bof) {
    lookahead_.node = std::make_unique<AST::TokenNode>(c.ToSpan());
  }

  template <size_t N> inline Language::NodeType get_type() const {
    return node_type_stack_[node_type_stack_.size() - N];
  }

  template <size_t N> inline AST::Node *get() const {
    return node_stack_[node_stack_.size() - N].get();
  }

  ShiftState shift_state() const {
    using namespace Language;
    // If the size is just 1, no rule will match so don't bother checking.
    if (node_stack_.size() < 2) { return ShiftState::NeedMore; }

    if (lookahead_.node_type == newline) {
      // TODO it's much more complicated than this. (braces?)
      return brace_count == 0 ? ShiftState::EndOfExpr : ShiftState::MustReduce;
    }

    if (get_type<1>() == dots) {
      return (lookahead_.node_type &
              (op_bl | op_l | op_lt | expr | fn_expr | l_paren | l_bracket))
                 ? ShiftState::NeedMore
                 : ShiftState::MustReduce;
    }

    if (lookahead_.node_type == l_brace && get_type<1>() == fn_expr &&
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (lookahead_.node_type == l_brace &&
        (get_type<1>() & (fn_expr | kw_struct | kw_block))) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == newline && get_type<2>() == comma) {
      return ShiftState::MustReduce;
    }

    // We require struct params to be in parentheses.
    if (lookahead_.node_type == l_paren && get_type<1>() == kw_struct) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == op_lt && lookahead_.node_type != newline) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == kw_block && lookahead_.node_type == newline) {
      return ShiftState::NeedMore;
    }

    if (get_type<2>() == kw_block && get_type<1>() == newline) {
      return ShiftState::NeedMore;
    }

    if (node_stack_.size() > 2 && get_type<3>() == kw_expr_block &&
        get_type<2>() == expr && get_type<1>() == newline) {
      return ShiftState::NeedMore;
    }

    if (lookahead_.node_type == r_paren) { return ShiftState::MustReduce; }

    constexpr u64 OP_ =
        op_l | op_b | colon | eq | comma | op_bl | dots | op_lt | fn_arrow;
    if (get_type<2>() & OP_) {
      auto left_prec = precedence(get<2>()->as<AST::TokenNode>().op);
      size_t right_prec;
      if (lookahead_.node_type & OP_) {
        right_prec = precedence(lookahead_.node->as<AST::TokenNode>().op);
      } else if (lookahead_.node_type == l_bracket) {
        right_prec = precedence(Operator::Index);

      } else if (lookahead_.node_type == l_paren) {
        right_prec = precedence(Operator::Call);
      } else {
        return ShiftState::MustReduce;
      }

      return (left_prec < right_prec) ||
                     (left_prec == right_prec &&
                      (left_prec & assoc_mask) == right_assoc)
                 ? ShiftState::NeedMore
                 : ShiftState::MustReduce;
    }
    return ShiftState::MustReduce;
  }

  std::vector<Language::NodeType> node_type_stack_;
  std::vector<base::owned_ptr<AST::Node>> node_stack_;
  NNT lookahead_;

  // We actually don't care about mathing braces. That is, we can count {[) as 1
  // open, because we are only using this to determine for the REPL if we should
  // prompt for further input. If it's wrong, we won't be able to to parse
  // anyway, so it only needs to be the correct value when the braces match.
  int brace_count = 0;
};

// Print out the debug information for the parse stack, and pause.
static void Debug(ParseState *ps, SourceLocation *loc = nullptr) {
  // Clear the screen
  fprintf(stderr, "\033[2J\033[1;1H\n");
  if (loc != nullptr) {
    fprintf(stderr, "%s", loc->line().c_str());
    fprintf(stderr, "%*s^\n(offset = %u)\n\n",
            static_cast<int>(loc->cursor.offset), "", loc->cursor.offset);
  }
  for (auto x : ps->node_type_stack_) { fprintf(stderr, "%lu, ", x); }
  fprintf(stderr, " -> %lu", ps->lookahead_.node_type);
  fputs("", stderr);

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->to_string(0).c_str(), stderr);
  }
  fgetc(stdin);
}

static void Shift(ParseState *ps, SourceLocation *c) {
  ps->node_type_stack_.push_back(ps->lookahead_.node_type);
  ps->node_stack_.push_back(base::own(ps->lookahead_.node.release()));
  ps->lookahead_ = NextToken(*c);
  if (ps->lookahead_.node_type &
      (Language::l_paren | Language::l_bracket | Language::l_brace |
       Language::l_double_brace)) {
    ++ps->brace_count;
  } else if (ps->lookahead_.node_type &
             (Language::r_paren | Language::r_bracket | Language::r_brace |
              Language::r_double_brace)) {
    --ps->brace_count;
  }
}

static bool Reduce(ParseState *ps) {
  const Rule *matched_rule_ptr = nullptr;
  for (const Rule &rule : Language::Rules) {
    if (rule.match(ps->node_type_stack_)) {
      matched_rule_ptr = &rule;
      break;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(&ps->node_stack_, &ps->node_type_stack_);

  return true;
}

void CleanUpReduction(ParseState *state, SourceLocation *loc) {
  // Reduce what you can
  while (Reduce(state)) {
    if (debug::parser) { Debug(state, loc); }
  }

  state->node_type_stack_.push_back(Language::eof);
  state->node_stack_.push_back(
      base::make_owned<AST::TokenNode>(loc->ToSpan(), ""));
  state->lookahead_ =
      NNT(std::make_unique<AST::TokenNode>(loc->ToSpan(), ""), Language::eof);

  // Reduce what you can again
  while (Reduce(state)) {
    if (debug::parser) { Debug(state, loc); }
  }
  if (debug::parser) { Debug(state, loc); }
}

base::owned_ptr<AST::Statements> Repl::Parse() {
  first_entry = true; // Show '>> ' the first time.

  SourceLocation loc;
  loc.source = this;

  auto state = ParseState(loc);
  Shift(&state, &loc);

  while (true) {
    auto shift_state = state.shift_state();
    switch (shift_state) {
    case ShiftState::NeedMore:
      Shift(&state, &loc);

      if (debug::parser) { Debug(&state, &loc); }
      continue;
    case ShiftState::EndOfExpr:
      CleanUpReduction(&state, &loc);
      return base::move<AST::Statements>(state.node_stack_.back());
    case ShiftState::MustReduce:
      Reduce(&state) || (Shift(&state, &loc), true);
      if (debug::parser) { Debug(&state, &loc); }
    }
  }
}

base::owned_ptr<AST::Statements> File::Parse() {
  SourceLocation loc;
  loc.source = this;

  auto state = ParseState(loc);
  Shift(&state, &loc);

  while (state.lookahead_.node_type != Language::eof) {
    ASSERT_EQ(state.node_type_stack_.size(), state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (state.shift_state() == ShiftState::NeedMore || !Reduce(&state)) {
      Shift(&state, &loc);
    }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup
  CleanUpReduction(&state, &loc);

  // Finish
  if (state.node_stack_.size() > 1) {
    std::vector<TextSpan> lines;

    size_t last_chosen_line = 0;
    for (size_t i = 0; i < state.node_stack_.size(); ++i) {
      if (state.node_stack_[i]->span.start.line_num == last_chosen_line) {
        continue;
      }
      if (state.node_type_stack_[i] &
          (Language::braced_stmts | Language::l_paren | Language::r_paren |
           Language::l_bracket | Language::r_bracket | Language::l_brace |
           Language::r_brace | Language::semicolon | Language::fn_arrow |
           Language::expr)) {
        lines.push_back(state.node_stack_[i]->span);
        last_chosen_line = state.node_stack_[i]->span.start.line_num;
      }
    }

    // TODO pass 'this' instead of 'name'.
    ErrorLog::UnknownParserError(name, lines);
  }

  return base::move<AST::Statements>(state.node_stack_.back());
}

extern Timer timer;
std::unordered_map<Source::Name, File *> source_map;
std::vector<base::owned_ptr<AST::Statements>> ParseAllFiles() {
  std::vector<base::owned_ptr<AST::Statements>> stmts;
  while (!file_queue.empty()) {
    auto file_name = std::move(file_queue.front());
    file_queue.pop();

    if (source_map.find(file_name) != source_map.end()) { continue; }

    RUN(timer, "Parsing a file") {
      auto source_file              = new File(std::move(file_name));
      source_map[source_file->name] = source_file;
      stmts.push_back(source_file->Parse());
    }
  }
  return stmts;
}
