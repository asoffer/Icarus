#include "ast/ast.h"
#include "base/debug.h"
#include "base/types.h"
#include "error_log.h"
#include "nnt.h"
#include "operators.h"
#include "util/timer.h"
#include <cstdio>
#include <queue>
#include <vector>
#include <iosfwd>

namespace AST {
struct Node;
} // namespace AST

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

  if (tk == "(") { // TODO these should just generate Call::Build directly.
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

static base::owned_ptr<AST::Node>
BuildKWExprBlock(std::vector<base::owned_ptr<AST::Node>> nodes) {
  const std::string &tk = nodes[0]->as<AST::TokenNode>().token;

  if (tk == "for") {
    return AST::For::Build(std::move(nodes));

  } else if (tk == "struct") {
    // Parmaetric struct
    NOT_YET();
  }

  UNREACHABLE();
}

static base::owned_ptr<AST::Node>
BuildEnumLiteral(std::vector<base::owned_ptr<AST::Node>> nodes) {
  std::vector<std::string> members;
  if (nodes[1]->is<AST::Statements>()) {
    for (auto &stmt : nodes[1]->as<AST::Statements>().statements) {
      if (stmt->is<AST::Identifier>()) {
        // Quadratic but we need it as a vector eventually anyway because we do
        // care about the order the user put it in.
        auto token = std::move(stmt->as<AST::Identifier>().token);
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
      IR::Val::Type(new Enum(
          "__anon.enum" + std::to_string(anon_enum_counter++), members)));
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

  } else if (tk == "enum") {
    return BuildEnumLiteral(std::move(nodes));

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

extern base::owned_ptr<AST::Node>
OneBracedStatement(std::vector<base::owned_ptr<AST::Node>> nodes);
extern base::owned_ptr<AST::Node>
EmptyBraces(std::vector<base::owned_ptr<AST::Node>> nodes);
extern base::owned_ptr<AST::Node>
BracedStatementsSameLineEnd(std::vector<base::owned_ptr<AST::Node>> nodes);

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
auto Rules = std::vector<Rule>{
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
    Rule(expr, {EXPR, l_paren, EXPR, r_paren}, AST::Call::Build),
    Rule(expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(expr, {EXPR, l_bracket, EXPR, r_bracket},
         AST::Binop::BuildIndexOperator),
    Rule(expr, {l_bracket, r_bracket}, AST::ArrayLiteral::BuildEmpty),
    Rule(expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket},
         AST::ArrayType::build),
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
         AST::CodeBlock::BuildFromStatementsSameLineEnd),
    Rule(expr, {l_double_brace, stmts, r_double_brace},
         AST::CodeBlock::BuildFromStatements),
    Rule(expr, {l_double_brace, r_double_brace}, AST::CodeBlock::BuildEmpty),
    Rule(expr, {l_double_brace, (expr | fn_expr), r_double_brace},
         AST::CodeBlock::BuildFromOneStatement),
    Rule(expr, {fn_expr, braced_stmts}, AST::FunctionLiteral::build),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case. For
    // example, "if(a)".
    Rule(expr, {EXPR, l_paren, RESERVED, r_paren}, ErrMsg::Reserved<0, 2>),
    Rule(expr, {EXPR, l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<0, 2>),

    Rule(expr, {(op_l | op_bl | op_lt), EXPR}, AST::Unop::BuildLeft),
    Rule(expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {EXPR, dots}, AST::Unop::BuildDots),
    Rule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    Rule(expr, {l_bracket, EXPR, r_bracket}, AST::ArrayLiteral::build),
    Rule(expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),
    Rule(stmts, {stmts, (expr | fn_expr | stmts), newline},
         AST::Statements::build_more),
    Rule(expr, {(kw_block | kw_struct), braced_stmts}, BuildKWBlock),

    Rule(expr, {RESERVED, dots}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {(op_l | op_bl | op_lt), RESERVED}, ErrMsg::Reserved<0, 1>),
    Rule(expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    Rule(stmts, {(expr | fn_expr), (newline | eof)},
         AST::Statements::build_one),
    Rule(comma, {comma, newline}, drop_all_but<0>),
    Rule(l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(l_double_brace, {l_double_brace, newline}, drop_all_but<0>),
    Rule(stmts, {stmts, newline}, drop_all_but<0>),
    Rule(stmts, {kw_expr_block, EXPR, braced_stmts}, BuildKWExprBlock),
    Rule(expr, {kw_struct, EXPR, braced_stmts}, BuildKWExprBlock),

    Rule(expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(stmts, {op_lt}, AST::Jump::build),
    Rule(expr, {EXPR, EXPR, braced_stmts}, AST::ScopeNode::Build),

    Rule(expr, {EXPR, braced_stmts}, AST::ScopeNode::BuildVoid),
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
extern std::queue<Source::Name> file_queue;
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
