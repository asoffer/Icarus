#include <array>
#include <cstdio>
#include <iosfwd>
#include <queue>
#include <unordered_map>
#include <vector>

#include "ast/access.h"
#include "ast/array_literal.h"
#include "ast/array_type.h"
#include "ast/binop.h"
#include "ast/block_literal.h"
#include "ast/call.h"
#include "ast/chainop.h"
#include "ast/comma_list.h"
#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/import.h"
#include "ast/jump.h"
#include "ast/scope_literal.h"
#include "ast/scope_node.h"
#include "ast/statements.h"
#include "ast/struct_literal.h"
#include "ast/switch.h"
#include "ast/terminal.h"
#include "ast/unop.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/types.h"
#include "error/log.h"
#include "frontend/token.h"
#include "operators.h"
#include "frontend/tagged_node.h"
#include "type/enum.h"

template <typename To, typename From>
static std::unique_ptr<To> move_as(std::unique_ptr<From> &val) {
  return std::unique_ptr<To>(static_cast<To *>(val.release()));
}

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

static void ValidateStatementSyntax(AST::Node *node, error::Log *error_log) {
  if (node->is<AST::CommaList>()) {
    error_log->CommaListStatement(node->as<AST::CommaList>().span);
    node->limit_to(AST::StageRange::NoEmitIR());
  }
}

static constexpr size_t left_assoc  = 0;
static constexpr size_t right_assoc = 1;
static constexpr size_t non_assoc   = 2;
static constexpr size_t chain_assoc = 3;
static constexpr size_t assoc_mask  = 3;

static constexpr size_t precedence(Language::Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Language::Operator::name:                                               \
    return (((prec) << 2) + (assoc));
#include "operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  __builtin_unreachable();
}

static std::unique_ptr<AST::Node> OneBracedStatement(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto stmts  = std::make_unique<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->content_.push_back(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), error_log);
  return stmts;
}

static std::unique_ptr<AST::Node> EmptyBraces(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto stmts  = std::make_unique<AST::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return stmts;
}

static std::unique_ptr<AST::Node> BracedStatementsSameLineEnd(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto stmts  = move_as<AST::Statements>(nodes[1]);
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[2]->is<AST::Statements>()) {
    for (auto &stmt : nodes[2]->as<AST::Statements>().content_) {
      stmts->content_.push_back(std::move(stmt));
      ValidateStatementSyntax(stmts->content_.back().get(), error_log);
    }
  } else {
    stmts->content_.push_back(std::move(nodes[2]));
    ValidateStatementSyntax(stmts->content_.back().get(), error_log);
  }
  return stmts;
}

namespace AST {
static std::unique_ptr<Node> BuildRightUnop(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  const std::string &tk = nodes[1]->as<frontend::Token>().token;
  if (tk == ":?") {
    auto unop      = std::make_unique<Unop>();
    unop->operand  = move_as<Expression>(nodes[0]);
    unop->op       = Language::Operator::TypeOf;
    unop->span     = TextSpan(unop->operand->span, nodes[1]->span);

    if (unop->operand->is<Declaration>()) {
      error_log->DeclarationUsedInUnop(tk, unop->operand->span);
    }

    return unop;
  } else {
    UNREACHABLE();
  }
}

// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration.
// Operand cannot be an assignment of any kind.
static std::unique_ptr<Node> BuildLeftUnop(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  const std::string &tk = nodes[0]->as<frontend::Token>().token;

  if (tk == "import") {
    auto import_node  = std::make_unique<Import>(move_as<Expression>(nodes[1]));
    import_node->span = TextSpan(nodes[0]->span, import_node->operand_->span);
    return import_node;
  }

  auto unop     = std::make_unique<Unop>();
  unop->operand = move_as<Expression>(nodes[1]);
  unop->span    = TextSpan(nodes[0]->span, unop->operand->span);

  using Language::Operator;
  const static std::unordered_map<std::string, Operator> UnopMap = {
      {"import", Operator::Import}, {"return", Operator::Return},
      {"free", Operator::Free},     {"generate", Operator::Generate},
      {"which", Operator::Which},   {"print", Operator::Print},
      {"needs", Operator::Needs},   {"ensure", Operator::Ensure},
      {"*", Operator::Mul},         {"&", Operator::And},
      {"-", Operator::Sub},         {"!", Operator::Not},
      {"@", Operator::At},          {"$", Operator::Eval}};
  auto iter = UnopMap.find(tk);
  ASSERT(iter != UnopMap.end());
  unop->op = iter->second;

  if (unop->operand->is<Declaration>()) {
    error_log->DeclarationUsedInUnop(tk, unop->operand->span);
  }
  return unop;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
static std::unique_ptr<Node> BuildChainOp(
    std::vector<std::unique_ptr<Node>> nodes) {
  auto op = nodes[1]->as<frontend::Token>().op;
  std::unique_ptr<ChainOp> chain;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  if (nodes[0]->is<ChainOp>() && nodes[0]->as<ChainOp>().ops.front() == op) {
    chain = move_as<ChainOp>(nodes[0]);

  } else {
    chain       = std::make_unique<ChainOp>();
    chain->span = TextSpan(nodes[0]->span, nodes[2]->span);

    chain->exprs.push_back(move_as<Expression>(nodes[0]));
  }

  chain->ops.push_back(op);
  chain->dispatch_tables_.emplace_back();
  chain->exprs.push_back(move_as<Expression>(nodes[2]));

  return chain;
}

static std::unique_ptr<Node> BuildCommaList(
    std::vector<std::unique_ptr<Node>> nodes) {
  std::unique_ptr<CommaList> comma_list;

  if (nodes[0]->is<CommaList>()) {
    comma_list = move_as<CommaList>(nodes[0]);
  } else {
    comma_list       = std::make_unique<CommaList>();
    comma_list->span = TextSpan(nodes[0]->span, nodes[2]->span);
    comma_list->exprs.push_back(move_as<Expression>(nodes[0]));
  }

  comma_list->exprs.push_back(move_as<Expression>(nodes[2]));
  comma_list->span.finish = comma_list->exprs.back()->span.finish;
  return comma_list;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
std::unique_ptr<Node> BuildAccess(std::vector<std::unique_ptr<Node>> nodes,
                                  error::Log *error_log) {
  auto access     = std::make_unique<Access>();
  access->span    = TextSpan(nodes[0]->span, nodes[2]->span);
  access->operand = move_as<Expression>(nodes[0]);

  if (access->operand->is<Declaration>()) {
    error_log->DeclarationInAccess(access->operand->span);
  }

  if (!nodes[2]->is<Identifier>()) {
    error_log->RHSNonIdInAccess(nodes[2]->span);
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
std::unique_ptr<Node> BuildCall(std::vector<std::unique_ptr<Node>> nodes,
                                error::Log *error_log) {
  auto call  = std::make_unique<Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = move_as<Expression>(nodes[0]);

  if (nodes[2]->is<CommaList>()) {
    std::optional<TextSpan> last_named_span_before_error = std::nullopt;
    std::vector<TextSpan> positional_error_spans;

    for (auto &expr : nodes[2]->as<CommaList>().exprs) {
      if (expr->is<Binop>() &&
          expr->as<Binop>().op == Language::Operator::Assign) {
        if (positional_error_spans.empty()) {
          last_named_span_before_error = expr->as<Binop>().lhs->span;
        }
        call->args_.named_.emplace(
            std::move(expr->as<Binop>().lhs->as<Identifier>().token),
            std::move(expr->as<Binop>().rhs));
      } else {
        if (last_named_span_before_error.has_value()) {
          positional_error_spans.push_back(expr->span);
        }
        call->args_.pos_.push_back(std::move(expr));
      }
    }

    if (!positional_error_spans.empty()) {
      error_log->PositionalArgumentFollowingNamed(
          positional_error_spans, *last_named_span_before_error);
    }
  } else {
    if (nodes[2]->is<Binop>() &&
        nodes[2]->as<Binop>().op == Language::Operator::Assign) {
      call->args_.named_.emplace(
          std::move(nodes[2]->as<Binop>().lhs->as<Identifier>().token),
          std::move(nodes[2]->as<Binop>().rhs));
    } else {
      call->args_.pos_.push_back(move_as<Expression>(nodes[2]));
    }
  }

  if (call->fn_->is<Declaration>()) {
    error_log->CallingDeclaration(call->fn_->span);
  }
  return call;
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
static std::unique_ptr<Node> BuildIndexOperator(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto binop  = std::make_unique<Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[2]->span);
  binop->lhs  = move_as<Expression>(nodes[0]);
  binop->rhs  = move_as<Expression>(nodes[2]);
  binop->op   = Language::Operator::Index;

  if (binop->lhs->is<Declaration>()) {
    error_log->IndexingDeclaration(binop->lhs->span);
  }

  if (binop->rhs->is<Declaration>()) {
    error_log->DeclarationInIndex(binop->rhs->span);
  }

  return binop;
}

// Input guarantee:
// [expression] [l_bracket] [r_bracket]
//
// Internal checks: None
static std::unique_ptr<Node> BuildEmptyArray(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto array_lit  = std::make_unique<ArrayLiteral>();
  array_lit->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return array_lit;
}

static std::unique_ptr<Node> BuildEmptyCommaList(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto comma_list  = std::make_unique<CommaList>();
  comma_list->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return comma_list;
}

static std::unique_ptr<Node> BuildArrayLiteral(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto array_lit  = std::make_unique<ArrayLiteral>();
  array_lit->span = nodes[0]->span;

  if (nodes[1]->is<CommaList>()) {
    array_lit->elems_ = std::move(nodes[1]->as<CommaList>().exprs);
  } else {
    array_lit->elems_.push_back(move_as<Expression>(nodes[1]));
  }

  return array_lit;
}

static std::unique_ptr<Node> BuildArrayType(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  if (nodes[1]->is<CommaList>()) {
    auto *length_chain = &nodes[1]->as<CommaList>();
    int i              = static_cast<int>(length_chain->exprs.size() - 1);
    auto prev          = move_as<Expression>(nodes[3]);

    while (i >= 0) {
      auto array_type        = std::make_unique<AST::ArrayType>();
      array_type->span       = length_chain->exprs[i]->span;
      array_type->length_    = std::move(length_chain->exprs[i]);
      array_type->data_type_ = std::move(prev);
      prev                   = std::move(array_type);
      i -= 1;
    }
    return prev;

  } else {
    auto array_type        = std::make_unique<AST::ArrayType>();
    array_type->span       = nodes[0]->span;
    array_type->length_    = move_as<Expression>(nodes[1]);
    array_type->data_type_ = move_as<Expression>(nodes[3]);

    return array_type;
  }
}

template <bool IsConst>
static std::unique_ptr<Node> BuildDeclaration(
    std::vector<std::unique_ptr<Node>> nodes) {
  auto op                = nodes[1]->as<frontend::Token>().op;
  auto decl              = std::make_unique<Declaration>(IsConst);
  decl->span             = TextSpan(nodes[0]->span, nodes[2]->span);
  decl->identifier       = move_as<Identifier>(nodes[0]);
  decl->identifier->decl = decl.get();

  if (op == Language::Operator::Colon ||
      op == Language::Operator::DoubleColon) {
    decl->type_expr = move_as<Expression>(nodes[2]);
  } else {
    decl->init_val = move_as<Expression>(nodes[2]);
  }

  return decl;
}

static std::pair<bool, std::vector<std::unique_ptr<Declaration>>> ExtractInputs(
    std::unique_ptr<Expression> args) {
  std::vector<std::unique_ptr<Declaration>> inputs;
  bool generic = false;
  if (args->is<Declaration>()) {
    inputs.push_back(move_as<Declaration>(args));
    generic |= inputs.back()->const_;

  } else if (args->is<CommaList>()) {
    auto *decls = &args->as<CommaList>();
    inputs.reserve(decls->exprs.size());

    for (auto &expr : decls->exprs) {
      inputs.push_back(move_as<Declaration>(expr));
      generic |= inputs.back()->const_;
    }
  } else {
    NOT_YET("log an error");
  }
  return std::pair(generic, std::move(inputs));
}

// TODO deal with duplication between this and below
static std::unique_ptr<Node> BuildShortFunctionLiteral(
    std::unique_ptr<Expression> args, std::unique_ptr<Expression> body,
    error::Log *error_log) {
  auto span             = TextSpan(args->span, body->span);
  auto[generic, inputs] = ExtractInputs(std::move(args));

  FunctionLiteral *fn_lit =
      generic ? new GenericFunctionLiteral : new FunctionLiteral;
  for (auto &input : inputs) { input->arg_val = fn_lit; }

  fn_lit->inputs     = std::move(inputs);
  fn_lit->span       = span;
  auto ret           = std::make_unique<Unop>();
  ret->op            = Language::Operator::Return;
  ret->operand       = std::move(body);
  fn_lit->statements = std::make_unique<Statements>();
  fn_lit->statements->content_.push_back(std::move(ret));

  size_t i = 0;
  for (const auto &input : fn_lit->inputs) {
    fn_lit->lookup_[input->identifier->token] = i++;
  }

  return base::wrap_unique(fn_lit);
}

static std::unique_ptr<Node> BuildFunctionLiteral(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto *binop           = &nodes[0]->as<Binop>();
  auto[generic, inputs] = ExtractInputs(std::move(binop->lhs));

  FunctionLiteral *fn_lit =
      generic ? new GenericFunctionLiteral : new FunctionLiteral;
  for (auto &input : inputs) { input->arg_val = fn_lit; }

  fn_lit->inputs     = std::move(inputs);
  fn_lit->span       = TextSpan(nodes[0]->span, nodes[1]->span);
  fn_lit->statements = move_as<Statements>(nodes[1]);

  if (binop->rhs->is<CommaList>()) {
    for (auto &expr : binop->rhs->as<CommaList>().exprs) {
      fn_lit->outputs.push_back(std::move(expr));
    }
  } else {
    fn_lit->outputs.push_back(move_as<Expression>(binop->rhs));
  }
  fn_lit->return_type_inferred_ = false;

  for (auto &expr : fn_lit->outputs) {
    if (expr->is<Declaration>()) { expr->as<Declaration>().arg_val = fn_lit; }
  }

  size_t i = 0;
  for (const auto &input : fn_lit->inputs) {
    fn_lit->lookup_[input->identifier->token] = i++;
  }

  return base::wrap_unique(fn_lit);
}

static std::unique_ptr<Node> BuildOneStatement(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto stmts  = std::make_unique<Statements>();
  stmts->span = nodes[0]->span;
  stmts->content_.push_back(std::move(nodes[0]));
  ValidateStatementSyntax(stmts->content_.back().get(), error_log);
  return stmts;
}

static std::unique_ptr<Node> BuildMoreStatements(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto stmts = move_as<Statements>(nodes[0]);
  stmts->content_.push_back(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), error_log);
  return stmts;
}

static std::unique_ptr<Node> BuildJump(std::vector<std::unique_ptr<Node>> nodes,
                                       error::Log *error_log) {
  const static std::unordered_map<std::string, Jump::Kind> JumpKindMap = {
      {"return", Jump::Kind::Return}};
  auto iter = JumpKindMap.find(nodes[0]->as<frontend::Token>().token);
  ASSERT(iter != JumpKindMap.end());

  auto stmts  = std::make_unique<Statements>();
  stmts->span = nodes[0]->span;
  stmts->content_.push_back(
      std::make_unique<Jump>(nodes[0]->span, iter->second));
  return stmts;
}

static std::unique_ptr<Node> BuildCodeBlockFromStatements(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto block      = std::make_unique<CodeBlock>();
  block->span     = TextSpan(nodes[0]->span, nodes[2]->span);
  block->content_ = std::move(*nodes[1]).as<Statements>();
  return block;
}

static std::unique_ptr<Node> BuildCodeBlockFromStatementsSameLineEnd(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto block      = std::make_unique<CodeBlock>();
  block->span     = TextSpan(nodes[0]->span, nodes[3]->span);
  block->content_ = BracedStatementsSameLineEnd(std::move(nodes), error_log)
                        ->as<Statements>();
  return block;
}

static std::unique_ptr<Node> BuildCodeBlockFromOneStatement(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto block  = std::make_unique<CodeBlock>();
  block->span = TextSpan(nodes[0]->span, nodes[2]->span);
  block->content_ =
      OneBracedStatement(std::move(nodes), error_log)->as<Statements>();
  return block;
}

static std::unique_ptr<Node> BuildEmptyCodeBlock(
    std::vector<std::unique_ptr<Node>> nodes, error::Log *error_log) {
  auto block      = std::make_unique<CodeBlock>();
  block->span     = TextSpan(nodes[0]->span, nodes[1]->span);
  block->content_ = EmptyBraces(std::move(nodes), error_log)->as<Statements>();
  return block;
}
}  // namespace AST

namespace {
std::unique_ptr<AST::Node> BuildScopeNode(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto scope_name  = move_as<AST::Expression>(nodes[0]);
  auto stmts       = nodes[4]->as<AST::Statements>();
  auto scope_node  = std::make_unique<AST::ScopeNode>();
  scope_node->span = TextSpan(scope_name->span, stmts.span);
  scope_node->blocks_.push_back(std::move(scope_name));

  scope_node->block_map_.emplace(
      scope_node->blocks_.back().get(),
      AST::ScopeNode::BlockNode{std::move(stmts),
                                move_as<AST::Expression>(nodes[2]), nullptr});
  return scope_node;
}

std::unique_ptr<AST::Node> BuildVoidScopeNode(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto scope_name  = move_as<AST::Expression>(nodes[0]);
  auto stmts       = nodes[1]->as<AST::Statements>();
  auto scope_node  = std::make_unique<AST::ScopeNode>();
  scope_node->span = TextSpan(scope_name->span, stmts.span);
  scope_node->blocks_.push_back(std::move(scope_name));

  scope_node->block_map_.emplace(
      scope_node->blocks_.back().get(),
      AST::ScopeNode::BlockNode{std::move(stmts), nullptr, nullptr});
  return scope_node;
}

std::unique_ptr<AST::Node> ExtendScopeNode(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  // TODO is this a valid node? it should probably be an identifier?
  auto &scope_node = nodes[0]->as<AST::ScopeNode>();
  auto &extension  = nodes[1]->as<AST::ScopeNode>();
  ASSERT(extension.blocks_.size() == 1u);
  scope_node.blocks_.push_back(std::move(extension.blocks_[0]));
  auto &ext_block_node =
      extension.block_map_.at(scope_node.blocks_.back().get());
  scope_node.block_map_.emplace(scope_node.blocks_.back().get(),
                                std::move(ext_block_node));

  return std::move(nodes[0]);
}

struct Rule {
 public:
  using OptVec = std::vector<u64>;
  // TODO use spans
  using fnptr = std::unique_ptr<AST::Node> (*)(
      std::vector<std::unique_ptr<AST::Node>>, error::Log *error_log);

  Rule(frontend::Tag output, const OptVec &input, fnptr fn)
      : output_(output), input_(input), fn_(fn) {}

  size_t size() const { return input_.size(); }

  bool match(const std::vector<frontend::Tag> &tag_stack) const {
    // The stack needs to be long enough to match.
    if (input_.size() > tag_stack.size()) return false;

    size_t stack_index = tag_stack.size() - 1;
    size_t rule_index  = input_.size() - 1;

    // Iterate through backwards and exit as soon as you see a node whose
    // type does not match the rule.
    for (size_t i = 0; i < input_.size(); ++i, --rule_index, --stack_index) {
      if ((input_[rule_index] & tag_stack[stack_index]) == 0) { return false; }
    }

    // If you complete the loop, there is a match.
    return true;
  }

  void apply(std::vector<std::unique_ptr<AST::Node>> *node_stack,
             std::vector<frontend::Tag> *tag_stack,
             error::Log *error_log) const {
    // Make a vector for the rule function to take as input. It will begin with
    // size() shared_ptrs.
    std::vector<std::unique_ptr<AST::Node>> nodes_to_reduce;
    nodes_to_reduce.reserve(this->size());
    for (auto i = node_stack->size() - this->size(); i < node_stack->size();
         ++i) {
      nodes_to_reduce.push_back(std::move((*node_stack)[i]));
    }
    tag_stack->resize(node_stack->size() - this->size());
    node_stack->resize(node_stack->size() - this->size());

    node_stack->push_back(fn_(std::move(nodes_to_reduce), error_log));
    tag_stack->push_back(output_);
  }

 private:
  frontend::Tag output_;
  OptVec input_;
  fnptr fn_;
};
}  // namespace

namespace debug {
extern bool parser;
}  // namespace debug

static std::unique_ptr<AST::Node> BuildBinaryOperator(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  static const std::unordered_map<std::string, Language::Operator> chain_ops = {
      {",", Language::Operator::Comma}, {"==", Language::Operator::Eq},
      {"!=", Language::Operator::Ne},   {"<", Language::Operator::Lt},
      {">", Language::Operator::Gt},    {"<=", Language::Operator::Le},
      {">=", Language::Operator::Ge},   {"&", Language::Operator::And},
      {"|", Language::Operator::Or},    {"^", Language::Operator::Xor},
  };

  const std::string &tk = nodes[1]->as<frontend::Token>().token;
  {
    auto iter = chain_ops.find(tk);
    if (iter != chain_ops.end()) {
      nodes[1]->as<frontend::Token>().op = iter->second;
      return (iter->second == Language::Operator::Comma)
                 ? AST::BuildCommaList(std::move(nodes))
                 : AST::BuildChainOp(std::move(nodes));
    }
  }

  if (tk == ".") {
    return AST::BuildAccess(std::move(nodes), error_log);

  } else if (tk == ":" || tk == ":=") {
    return AST::BuildDeclaration<false>(std::move(nodes));

  } else if (tk == "::" || tk == "::=") {
    return AST::BuildDeclaration<true>(std::move(nodes));

  } else if (tk == "=>") {
    return AST::BuildShortFunctionLiteral(move_as<AST::Expression>(nodes[0]),
                                          move_as<AST::Expression>(nodes[2]),
                                          error_log);

  } else if (tk == "=") {
    if (nodes[0]->is<AST::Declaration>()) {
      if (nodes[0]->as<AST::Declaration>().IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        error_log->DoubleDeclAssignment(nodes[0]->span, nodes[1]->span);
        return move_as<AST::Declaration>(nodes[0]);
      }

      auto decl      = move_as<AST::Declaration>(nodes[0]);
      decl->init_val = move_as<AST::Expression>(nodes[2]);
      return decl;

    } else {
      auto binop  = std::make_unique<AST::Binop>();
      binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

      binop->lhs = move_as<AST::Expression>(nodes[0]);
      binop->rhs = move_as<AST::Expression>(nodes[2]);
      binop->op  = Language::Operator::Assign;
      return binop;
    }
  }

  if (tk == "(") {  // TODO these should just generate BuildCall directly.
    return AST::BuildCall(std::move(nodes), error_log);
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[2]);
    return AST::BuildCall(std::move(nodes), error_log);
  }

  auto binop  = std::make_unique<AST::Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

  binop->lhs = move_as<AST::Expression>(nodes[0]);
  binop->rhs = move_as<AST::Expression>(nodes[2]);

  static const std::unordered_map<std::string, Language::Operator> symbols = {
      {"->", Language::Operator::Arrow}, {"|=", Language::Operator::OrEq},
      {"&=", Language::Operator::AndEq}, {"^=", Language::Operator::XorEq},
      {"+=", Language::Operator::AddEq}, {"-=", Language::Operator::SubEq},
      {"*=", Language::Operator::MulEq}, {"/=", Language::Operator::DivEq},
      {"%=", Language::Operator::ModEq}, {"+", Language::Operator::Add},
      {"-", Language::Operator::Sub},    {"*", Language::Operator::Mul},
      {"/", Language::Operator::Div},    {"%", Language::Operator::Mod},
      {"[", Language::Operator::Index},  {"when", Language::Operator::When}};
  {
    auto iter = symbols.find(tk);
    if (iter != symbols.end()) { binop->op = iter->second; }

    return binop;
  }
}

static std::unique_ptr<AST::Node> BuildEnumOrFlagLiteral(
    std::vector<std::unique_ptr<AST::Node>> nodes, bool is_enum,
    error::Log *error_log) {
  std::unordered_set<std::string> members;
  if (nodes[1]->is<AST::Statements>()) {
    for (auto &stmt : nodes[1]->as<AST::Statements>().content_) {
      if (!stmt->is<AST::Identifier>()) {
        error_log->EnumNeedsIds(stmt->span);
        continue;
      }
      // Quadratic but we need it as a vector eventually anyway because we do
      // care about the order the user put it in.
      auto &token = stmt->as<AST::Identifier>().token;
      if (auto[iter, success] = members.insert(token); !success) {
        // TODO this span is wrong.
        error_log->RepeatedEnumName(stmt->span);
      }
    }
  }

  static size_t anon_enum_counter = 0;
  std::vector<std::string> members_vec(std::make_move_iterator(members.begin()),
                                       std::make_move_iterator(members.end()));
  if (is_enum) {
    return std::make_unique<AST::Terminal>(
        TextSpan(nodes[0]->span, nodes[1]->span),
        IR::Val::Type(
            new type::Enum("__anon.enum" + std::to_string(anon_enum_counter++),
                           std::move(members_vec))));
  } else {
    return std::make_unique<AST::Terminal>(
        TextSpan(nodes[0]->span, nodes[1]->span),
        IR::Val::Type(
            new type::Enum("__anon.enum" + std::to_string(anon_enum_counter++),
                           std::move(members_vec))));
  }
}

static std::unique_ptr<AST::Node> BuildStructLiteral(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto struct_lit  = std::make_unique<AST::StructLiteral>();
  struct_lit->span = TextSpan(nodes.front()->span, nodes.back()->span);
  for (auto &stmt : nodes[1]->as<AST::Statements>().content_) {
    if (stmt->is<AST::Declaration>()) {
      struct_lit->fields_.push_back(move_as<AST::Declaration>(stmt));
    } else {
      error_log->NonDeclarationInStructDeclaration(stmt->span);
      // TODO show the entire struct declaration and point to the problematic
      // lines.
    }
  }
  return struct_lit;
}

static std::unique_ptr<AST::Node> BuildScopeLiteral(
    std::unique_ptr<AST::Statements> stmts, error::Log *error_log) {
  auto scope_lit  = std::make_unique<AST::ScopeLiteral>();
  scope_lit->span = stmts->span;  // TODO it's really bigger than this because
                                  // it involves the keyword too.
  for (auto& stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      scope_lit->decls_.push_back(std::move(stmt->as<AST::Declaration>()));
    } else {
      NOT_YET(stmt);
    }
  }
  return scope_lit;
}

static std::unique_ptr<AST::Node> BuildBlock(
    std::unique_ptr<AST::Statements> stmts, error::Log *error_log) {
  auto block_expr = std::make_unique<AST::BlockLiteral>();
  block_expr->span = stmts->span;  // TODO it's really bigger than this because
                                   // it involves the keyword too.

  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      auto decl = move_as<AST::Declaration>(stmt);
      if (decl->identifier->token == "before") {
        block_expr->before_ = std::move(decl);
      } else if (decl->identifier->token == "after") {
        block_expr->after_ = std::move(decl);
      }
    } else {
      NOT_YET();
    }
  }

  return block_expr;
}


static std::unique_ptr<AST::Node> BuildSwitch(
    std::unique_ptr<AST::Statements> stmts, error::Log *error_log) {
  auto switch_expr = std::make_unique<AST::Switch>();
  switch_expr->span = stmts->span;  // TODO it's really bigger than this because
                                    // it involves the keyword too.

  switch_expr->cases_.reserve(stmts->content_.size());
  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Binop>()) {
      auto binop = move_as<AST::Binop>(stmt);
      if (binop->op == Language::Operator::When) {
        switch_expr->cases_.emplace_back(std::move(binop->lhs),
                                         std::move(binop->rhs));
      } else {
        NOT_YET("handle error");
      }
    } else {
      NOT_YET("handle error");
    }
  }

  return switch_expr;
}

static std::unique_ptr<AST::Node> BuildKWBlock(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  const std::string &tk = nodes[0]->as<frontend::Token>().token;

  if (bool is_enum = (tk == "enum"); is_enum || tk == "flags") {
    return BuildEnumOrFlagLiteral(std::move(nodes), is_enum, error_log);

  } else if (tk == "struct") {
    return BuildStructLiteral(std::move(nodes), error_log);

  } else if (tk == "scope") {
    return BuildScopeLiteral(move_as<AST::Statements>(nodes[1]), error_log);

  } else if (tk == "switch") {
    return BuildSwitch(move_as<AST::Statements>(nodes[1]), error_log);

  } else if (tk == "block") {
    return BuildBlock(move_as<AST::Statements>(nodes[1]), error_log);
  }

  UNREACHABLE();
}

static std::unique_ptr<AST::Node> Parenthesize(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto expr = move_as<AST::Expression>(nodes[1]);
  if (nodes[0]->as<frontend::Token>().token != "\\(") {
    return expr;
  } else {
    auto unop     = std::make_unique<AST::Unop>();
    unop->operand = std::move(expr);
    unop->span    = TextSpan(nodes[0]->span, nodes[2]->span);
    unop->op      = Language::Operator::Ref;
    return unop;
  }
}

static std::unique_ptr<AST::Node> BuildEmptyParen(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto call  = std::make_unique<AST::Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = move_as<AST::Expression>(nodes[0]);

  if (call->fn_->is<AST::Declaration>()) {
    error_log->CallingDeclaration(call->fn_->span);
  }
  return call;
}

template <size_t N>
static std::unique_ptr<AST::Node> drop_all_but(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  return std::move(nodes[N]);
}

static std::unique_ptr<AST::Node> CombineColonEq(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto *tk_node = &nodes[0]->as<frontend::Token>();
  tk_node->token += "=";  // Change : to := and :: to ::=
  tk_node->op = Language::Operator::ColonEq;
  return drop_all_but<0>(std::move(nodes), error_log);
}

std::unique_ptr<AST::Node> EmptyFile(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  auto stmts  = std::make_unique<AST::Statements>();
  stmts->span = nodes[0]->span;
  return std::move(stmts);
}

namespace ErrMsg {
template <size_t RTN, size_t RES>
static std::unique_ptr<AST::Node> Reserved(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  error_log->Reserved(nodes[RES]->span,
                      nodes[RES]->as<frontend::Token>().token);

  return std::make_unique<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

template <size_t RTN, size_t RES1, size_t RES2>
static std::unique_ptr<AST::Node> BothReserved(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  error_log->Reserved(nodes[RES1]->span,
                      nodes[RES1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[RES2]->span,
                      nodes[RES2]->as<frontend::Token>().token);
  return std::make_unique<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

static std::unique_ptr<AST::Node> NonBinop(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  return std::make_unique<AST::Identifier>(nodes[1]->span, "invalid_node");
}

template <size_t RTN, size_t RES>
static std::unique_ptr<AST::Node> NonBinopReserved(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[RES]->span,
                      nodes[RES]->as<frontend::Token>().token);
  return std::make_unique<AST::Identifier>(nodes[RTN]->span, "invalid_node");
}

static std::unique_ptr<AST::Node> NonBinopBothReserved(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *error_log) {
  error_log->Reserved(nodes[0]->span, nodes[0]->as<frontend::Token>().token);
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[2]->span, nodes[2]->as<frontend::Token>().token);
  return std::make_unique<AST::Identifier>(nodes[1]->span, "invalid_node");
}
}  // namespace ErrMsg

static std::unique_ptr<AST::Node> BuildOperatorIdentifier(
    std::vector<std::unique_ptr<AST::Node>> nodes, error::Log *) {
  auto span = nodes[1]->span;
  return std::make_unique<AST::Identifier>(
      span, move_as<frontend::Token>(nodes[1])->token);
}

namespace frontend {
static constexpr u64 OP_B = op_b | comma | colon | eq;
static constexpr u64 EXPR = expr | fn_expr | scope_expr;
// Used in error productions only!
static constexpr u64 RESERVED = kw_block | op_lt;

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
    Rule(expr, {EXPR, (OP_B | op_bl), RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(expr, {RESERVED, (OP_B | op_bl), RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),
    Rule(expr, {EXPR, op_l, RESERVED}, ErrMsg::NonBinopReserved<1, 2>),
    Rule(expr, {RESERVED, op_l, RESERVED}, ErrMsg::NonBinopBothReserved),
    Rule(expr, {EXPR, l_paren, EXPR, r_paren}, AST::BuildCall),
    Rule(expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(expr, {l_paren, op_l | op_b | eq | op_bl, r_paren},
         BuildOperatorIdentifier),
    Rule(expr, {l_paren, r_paren}, AST::BuildEmptyCommaList),
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
    Rule(braced_stmts, {l_brace, stmts, stmts | EXPR, r_brace},
         BracedStatementsSameLineEnd),
    Rule(braced_stmts, {l_brace, stmts, r_brace}, drop_all_but<1>),
    Rule(braced_stmts, {l_brace, r_brace}, EmptyBraces),
    Rule(braced_stmts, {l_brace, EXPR, r_brace}, OneBracedStatement),
    Rule(expr, {l_double_brace, stmts, stmts | EXPR, r_double_brace},
         AST::BuildCodeBlockFromStatementsSameLineEnd),
    Rule(expr, {l_double_brace, stmts, r_double_brace},
         AST::BuildCodeBlockFromStatements),
    Rule(expr, {l_double_brace, r_double_brace}, AST::BuildEmptyCodeBlock),
    Rule(expr, {l_double_brace, EXPR, r_double_brace},
         AST::BuildCodeBlockFromOneStatement),
    Rule(expr, {fn_expr, braced_stmts}, AST::BuildFunctionLiteral),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case.
    Rule(expr, {EXPR, l_paren, RESERVED, r_paren}, ErrMsg::Reserved<0, 2>),
    Rule(expr, {EXPR, l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<0, 2>),

    Rule(expr, {EXPR, op_r}, AST::BuildRightUnop),
    Rule(expr, {(op_l | op_bl | op_lt), EXPR}, AST::BuildLeftUnop),
    Rule(expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    Rule(expr, {l_bracket, EXPR, r_bracket}, AST::BuildArrayLiteral),
    Rule(expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),
    Rule(stmts, {stmts, (EXPR | stmts), newline}, AST::BuildMoreStatements),
    Rule(expr, {kw_block, braced_stmts}, BuildKWBlock),

    Rule(expr, {(op_l | op_bl | op_lt), RESERVED}, ErrMsg::Reserved<0, 1>),
    Rule(expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    // TODO does this rule prevent chained scope blocks on new lines or is it
    // preceeded by a shift rule that eats newlines after a right-brace?
    Rule(stmts, {EXPR, (newline | eof)}, AST::BuildOneStatement),
    Rule(comma, {comma, newline}, drop_all_but<0>),
    Rule(l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(l_double_brace, {l_double_brace, newline}, drop_all_but<0>),
    Rule(stmts, {stmts, newline}, drop_all_but<0>),

    Rule(expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(stmts, {op_lt}, AST::BuildJump),
    Rule(scope_expr, {expr, braced_stmts}, BuildVoidScopeNode),
    Rule(scope_expr, {expr, l_paren, EXPR, r_paren, braced_stmts},
         BuildScopeNode),
    Rule(scope_expr, {scope_expr, scope_expr}, ExtendScopeNode),
};

TaggedNode NextToken(SourceLocation &loc, error::Log *error_log);

namespace {
enum class ShiftState : char { NeedMore, EndOfExpr, MustReduce };
struct ParseState {
  ParseState(SourceLocation *loc, error::Log *error_log)
      : error_log_(error_log), loc_(loc) {
    lookahead_.push(frontend::TaggedNode(
        std::make_unique<frontend::Token>(loc_->ToSpan()), bof));
  }

  template <size_t N>
  inline frontend::Tag get_type() const {
    return tag_stack_[tag_stack_.size() - N];
  }

  template <size_t N>
  inline AST::Node *get() const {
    return node_stack_[node_stack_.size() - N].get();
  }

  ShiftState shift_state() {
    using namespace Language;
    // If the size is just 1, no rule will match so don't bother checking.
    if (node_stack_.size() < 2) { return ShiftState::NeedMore; }

    const auto &ahead = Next();
    if (ahead.tag_ == newline) {
      return brace_count == 0 ? ShiftState::EndOfExpr : ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace && get_type<1>() == fn_expr &&
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace && (get_type<1>() & (fn_expr | kw_block))) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == newline && get_type<2>() == comma) {
      return ShiftState::MustReduce;
    }

    if (get_type<1>() == op_lt && ahead.tag_ != newline) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == kw_block && ahead.tag_ == newline) {
      return ShiftState::NeedMore;
    }

    if (get_type<2>() == kw_block && get_type<1>() == newline) {
      return ShiftState::NeedMore;
    }

    if (ahead.tag_ == r_paren) { return ShiftState::MustReduce; }

    if (get_type<1>() == r_paren && ahead.tag_ == l_brace) {
      size_t i = tag_stack_.size() - 1;
      while (i > 0) {
        if (tag_stack_[i] == fn_arrow) { return ShiftState::MustReduce; }
        if (tag_stack_[i] == stmts) { return ShiftState::NeedMore; }
        --i;
      }
      return ShiftState::NeedMore;
    }

    constexpr u64 OP =
        op_r | op_l | op_b | colon | eq | comma | op_bl | op_lt | fn_arrow;
    if (get_type<2>() & OP) {
      auto left_prec = precedence(get<2>()->as<frontend::Token>().op);
      size_t right_prec;
      if (ahead.tag_ & OP) {
        right_prec = precedence(ahead.node_->as<frontend::Token>().op);
      } else if (ahead.tag_ == l_bracket) {
        right_prec = precedence(Operator::Index);

      } else if (ahead.tag_ == l_paren) {
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

  void LookAhead() { lookahead_.push(NextToken(*loc_, error_log_)); }

  const TaggedNode &Next() {
    if (lookahead_.empty()) { LookAhead(); }
    return lookahead_.back();
  }

  std::vector<frontend::Tag> tag_stack_;
  std::vector<std::unique_ptr<AST::Node>> node_stack_;
  std::queue<frontend::TaggedNode> lookahead_;
  error::Log *error_log_ = nullptr;
  SourceLocation *loc_   = nullptr;

  // We actually don't care about mathing braces because we are only using this
  // to determine for the REPL if we should prompt for further input. If it's
  // wrong, we won't be able to to parse anyway, so it only needs to be the
  // correct value when the braces match.
  int brace_count = 0;
};
}  // namespace
}  // namespace frontend

// Print out the debug information for the parse stack, and pause.
static void Debug(frontend::ParseState *ps) {
  // Clear the screen
  fprintf(stderr, "\033[2J\033[1;1H\n");
  if (ps->loc_ != nullptr) {
    fprintf(stderr, "%s", ps->loc_->line().c_str());
    fprintf(stderr, "%*s^\n(offset = %u)\n\n",
            static_cast<int>(ps->loc_->cursor.offset), "",
            ps->loc_->cursor.offset);
  }
  for (auto x : ps->tag_stack_) { fprintf(stderr, "%lu, ", x); }
  fprintf(stderr, " -> %lu", ps->Next().tag_);
  fputs("", stderr);

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->to_string(0).c_str(), stderr);
  }
  fgetc(stdin);
}

static void Shift(frontend::ParseState *ps) {
  if (ps->lookahead_.empty()) { ps->LookAhead(); }
  auto ahead = std::move(ps->lookahead_.front());
  ps->lookahead_.pop();
  ps->tag_stack_.push_back(ahead.tag_);
  ps->node_stack_.push_back(std::move(ahead.node_));

  auto tag_ahead = ps->Next().tag_;
  if (tag_ahead & (frontend::l_paren | frontend::l_bracket | frontend::l_brace |
                   frontend::l_double_brace)) {
    ++ps->brace_count;
  } else if (tag_ahead & (frontend::r_paren | frontend::r_bracket |
                          frontend::r_brace | frontend::r_double_brace)) {
    --ps->brace_count;
  }
}

static bool Reduce(frontend::ParseState *ps) {
  const Rule *matched_rule_ptr = nullptr;
  for (const Rule &rule : frontend::Rules) {
    if (rule.match(ps->tag_stack_)) {
      matched_rule_ptr = &rule;
      break;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(&ps->node_stack_, &ps->tag_stack_, ps->error_log_);

  return true;
}

static void CleanUpReduction(frontend::ParseState *state) {
  // Reduce what you can
  while (Reduce(state)) {
    if (debug::parser) { Debug(state); }
  }

  Shift(state);

  // Reduce what you can again
  while (Reduce(state)) {
    if (debug::parser) { Debug(state); }
  }
  if (debug::parser) { Debug(state); }
}

std::unique_ptr<AST::Statements> Repl::Parse(error::Log *error_log) {
  first_entry = true;  // Show '>> ' the first time.

  SourceLocation loc;
  loc.source = this;

  auto state = frontend::ParseState(&loc, error_log);
  Shift(&state);

  while (true) {
    auto shift_state = state.shift_state();
    switch (shift_state) {
      case frontend::ShiftState::NeedMore:
        Shift(&state);

        if (debug::parser) { Debug(&state); }
        continue;
      case frontend::ShiftState::EndOfExpr:
        CleanUpReduction(&state);
        return move_as<AST::Statements>(state.node_stack_.back());
      case frontend::ShiftState::MustReduce:
        Reduce(&state) || (Shift(&state), true);
        if (debug::parser) { Debug(&state); }
    }
  }
}

std::unique_ptr<AST::Statements> File::Parse(error::Log *error_log) {
  SourceLocation loc;
  loc.source = this;

  auto state = frontend::ParseState(&loc, error_log);
  Shift(&state);

  while (state.Next().tag_ != frontend::eof) {
    ASSERT(state.tag_stack_.size() == state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (state.shift_state() == frontend::ShiftState::NeedMore ||
        !Reduce(&state)) {
      Shift(&state);
    }

    if (debug::parser) { Debug(&state); }
  }

  // Cleanup
  CleanUpReduction(&state);

  // Finish
  if (state.node_stack_.size() > 1) {
    std::vector<TextSpan> lines;

    for (size_t i = 0; i < state.node_stack_.size(); ++i) {
      if (state.tag_stack_[i] &
          (frontend::braced_stmts | frontend::l_paren | frontend::r_paren |
           frontend::l_bracket | frontend::r_bracket | frontend::l_brace |
           frontend::r_brace | frontend::semicolon | frontend::fn_arrow |
           frontend::expr)) {
        lines.push_back(state.node_stack_[i]->span);
      }
    }

    // This is an exceedingly crappy error message.
    error_log->UnknownParseError(lines);
  }

  return move_as<AST::Statements>(state.node_stack_.back());
}
