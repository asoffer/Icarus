#include <array>
#include <cstdio>
#include <iosfwd>
#include <queue>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"

#include "ast/access.h"
#include "ast/array_literal.h"
#include "ast/array_type.h"
#include "ast/binop.h"
#include "ast/block_literal.h"
#include "ast/call.h"
#include "ast/chainop.h"
#include "ast/comma_list.h"
#include "ast/declaration.h"
#include "ast/enum_literal.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/import.h"
#include "ast/interface.h"
#include "ast/match_declaration.h"
#include "ast/repeated_unop.h"
#include "ast/scope_literal.h"
#include "ast/scope_node.h"
#include "ast/statements.h"
#include "ast/struct_literal.h"
#include "ast/struct_type.h"
#include "ast/switch.h"
#include "ast/terminal.h"
#include "ast/unop.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/types.h"
#include "context.h"
#include "frontend/operators.h"
#include "frontend/tagged_node.h"
#include "frontend/token.h"
#include "type/enum.h"
#include "type/flags.h"

using ::base::check::Is;
template <typename To, typename From>
static std::unique_ptr<To> move_as(std::unique_ptr<From> &val) {
  ASSERT(val, Is<To>());
  return std::unique_ptr<To>(static_cast<To *>(val.release()));
}

static void ValidateStatementSyntax(ast::Node *node, Context *ctx) {
  if (node->is<ast::CommaList>()) {
    ctx->error_log_.CommaListStatement(node->as<ast::CommaList>().span);
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
#include "frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  __builtin_unreachable();
}

static std::map<std::string, ast::Hashtag::Builtin> const BuiltinHashtagMap = {
    {"{export}", ast::Hashtag::Builtin::Export},
    {"{no_default}", ast::Hashtag::Builtin::NoDefault}};
static std::unique_ptr<ast::Node> AddHashtag(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto expr = move_as<ast::Expression>(nodes.back());
  auto iter =
      BuiltinHashtagMap.find(nodes.front()->as<frontend::Token>().token);
  if (iter != BuiltinHashtagMap.end()) {
    expr->hashtags_.emplace_back(iter->second);
  } else {
    NOT_YET(nodes.front()->as<frontend::Token>().token);
  }
  return expr;
}

static std::unique_ptr<ast::Node> OneBracedStatement(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), ctx);
  return stmts;
}

static std::unique_ptr<ast::Node> EmptyBraces(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return stmts;
}

static std::unique_ptr<ast::Node> BuildJump(std::unique_ptr<ast::Node> node) {
  using ::Language::Operator;
  auto &tk   = node->as<frontend::Token>().token;
  auto jmp   = std::make_unique<ast::RepeatedUnop>(node->span);
  jmp->op_   = tk == "return" ? Operator::Return : Operator::Yield;
  auto stmts = std::make_unique<ast::Statements>();
  stmts->append(std::move(jmp));
  stmts->span = node->span;
  return stmts;
}

static std::unique_ptr<ast::Node> BracedStatementsSameLineEnd(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto stmts  = move_as<ast::Statements>(nodes[1]);
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[2]->is<ast::Statements>()) {
    for (auto &stmt : nodes[2]->as<ast::Statements>().content_) {
      stmts->append(std::move(stmt));
      ValidateStatementSyntax(stmts->content_.back().get(), ctx);
    }
  } else {
    stmts->append(std::move(nodes[2]));
    ValidateStatementSyntax(stmts->content_.back().get(), ctx);
  }
  return stmts;
}

static std::unique_ptr<ast::Node> BracedStatementsJumpSameLineEnd(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  nodes[2] = BuildJump(std::move(nodes[2]));
  return BracedStatementsSameLineEnd(std::move(nodes), ctx);
}

namespace ast {
namespace {
std::unique_ptr<Node> BuildRightUnop(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  const std::string &tk = nodes[1]->as<frontend::Token>().token;
  if (tk == ":?") {
    auto unop     = std::make_unique<Unop>();
    unop->operand = move_as<Expression>(nodes[0]);
    unop->op      = Language::Operator::TypeOf;
    unop->span    = TextSpan(unop->operand->span, nodes[1]->span);

    if (unop->operand->is<Declaration>()) {
      ctx->error_log_.DeclarationUsedInUnop(tk, unop->operand->span);
    }

    return unop;
  } else {
    UNREACHABLE();
  }
}

// Input guarantees
// [expr] [l_paren] [expr] [r_paren]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
std::unique_ptr<Node> BuildCall(base::vector<std::unique_ptr<Node>> nodes,
                                Context *ctx) {
  auto call  = std::make_unique<Call>();
  call->span = TextSpan(nodes.front()->span, nodes.back()->span);
  call->fn_  = move_as<Expression>(nodes[0]);

  if (nodes[2]->is<CommaList>()) {
    std::optional<TextSpan> last_named_span_before_error = std::nullopt;
    base::vector<TextSpan> positional_error_spans;

    for (auto &expr : nodes[2]->as<CommaList>().exprs_) {
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
      ctx->error_log_.PositionalArgumentFollowingNamed(
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
    ctx->error_log_.CallingDeclaration(call->fn_->span);
  }
  return call;
}



// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration.
// Operand cannot be an assignment of any kind.
std::unique_ptr<Node> BuildLeftUnop(base::vector<std::unique_ptr<Node>> nodes,
                                    Context *ctx) {
  const std::string &tk = nodes[0]->as<frontend::Token>().token;

  using Language::Operator;
  if (tk == "import") {
    auto import_node  = std::make_unique<Import>(move_as<Expression>(nodes[1]));
    import_node->span = TextSpan(nodes[0]->span, import_node->operand_->span);
    return import_node;
  } else if (tk == "return" || tk == "yield") {
    auto unop = std::make_unique<RepeatedUnop>(
        TextSpan(nodes.front()->span, nodes.back()->span));
    unop->op_ = tk == "return" ? Operator::Return : Operator::Yield;
    if (nodes[1]->is<CommaList>() && !nodes[1]->as<CommaList>().parenthesized_) {
      unop->args_ = std::move(nodes[1]->as<CommaList>());
    } else {
      unop->args_.exprs_.push_back(move_as<Expression>(nodes[1]));
      unop->args_.span = TextSpan(unop->args_.exprs_.front()->span,
                                  unop->args_.exprs_.back()->span);
    }
    ASSERT_NOT_NULL(unop->span.source);
    return unop;
  } else if (tk == "print") {
    // TODO Copy of above.
    std::unique_ptr<RepeatedUnop> unop;
    if (nodes[1]->is<CommaList>() && !nodes[1]->as<CommaList>().parenthesized_) {
      unop = std::make_unique<RepeatedUnop>(
          TextSpan(nodes.front()->span, nodes.back()->span));
      unop->args_ = std::move(nodes[1]->as<CommaList>());
    } else {
      ASSERT_NOT_NULL(nodes[1]->span.source);
      unop = std::make_unique<RepeatedUnop>(nodes[1]->span);
      unop->args_.exprs_.push_back(move_as<Expression>(nodes[1]));
      ASSERT_NOT_NULL(unop->span.source);
    }
    unop->op_ = Operator::Print;
    ASSERT_NOT_NULL(unop->span.source);
    return unop;
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[1]);
    nodes.push_back(std::make_unique<ast::CommaList>());
    nodes.back()->span = nodes[0]->span;
    return ast::BuildCall(std::move(nodes), ctx);
  }

  auto unop     = std::make_unique<Unop>();
  unop->operand = move_as<Expression>(nodes[1]);
  unop->span    = TextSpan(nodes[0]->span, unop->operand->span);

  const static base::unordered_map<std::string, Operator> UnopMap{
      {"*", Operator::Mul},         {"import", Operator::Import},
      {"&", Operator::And},         {"-", Operator::Sub},
      {"which", Operator::Which},   {"!", Operator::Not},
      {"needs", Operator::Needs},   {"@", Operator::At},
      {"[*]", Operator::BufPtr},    {"<<", Operator::Expand},
      {"ensure", Operator::Ensure}, {"$", Operator::Eval}};
  auto iter = UnopMap.find(tk);
  ASSERT(iter != UnopMap.end());
  unop->op = iter->second;

  if (unop->operand->is<Declaration>()) {
    ctx->error_log_.DeclarationUsedInUnop(tk, unop->operand->span);
  }
  return unop;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
std::unique_ptr<Node> BuildChainOp(base::vector<std::unique_ptr<Node>> nodes,
                                   Context *ctx) {
  auto op = nodes[1]->as<frontend::Token>().op;
  std::unique_ptr<ChainOp> chain;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  if (nodes[0]->is<ChainOp>() &&
      precedence(nodes[0]->as<ChainOp>().ops.front()) == precedence(op)) {
    chain = move_as<ChainOp>(nodes[0]);

  } else {
    chain       = std::make_unique<ChainOp>();
    chain->span = TextSpan(nodes[0]->span, nodes[2]->span);

    chain->exprs.push_back(move_as<Expression>(nodes[0]));
  }

  chain->ops.push_back(op);
  chain->exprs.push_back(move_as<Expression>(nodes[2]));
  return chain;
}

std::unique_ptr<Node> BuildCommaList(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  std::unique_ptr<CommaList> comma_list = nullptr;
  if (nodes[0]->is<CommaList>() && !nodes[0]->as<CommaList>().parenthesized_) {
    comma_list = move_as<CommaList>(nodes[0]);
  } else {
    comma_list = std::make_unique<CommaList>();
    comma_list->span = TextSpan(nodes[0]->span, nodes[2]->span);
    comma_list->exprs_.push_back(move_as<Expression>(nodes[0]));
  }
  comma_list->exprs_.push_back(move_as<Expression>(nodes[2]));
  comma_list->span.finish = comma_list->exprs_.back()->span.finish;
  return comma_list;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
std::unique_ptr<Node> BuildAccess(base::vector<std::unique_ptr<Node>> nodes,
                                  Context *ctx) {
  auto access     = std::make_unique<Access>();
  access->span    = TextSpan(nodes[0]->span, nodes[2]->span);
  access->operand = move_as<Expression>(nodes[0]);

  if (access->operand->is<Declaration>()) {
    ctx->error_log_.DeclarationInAccess(access->operand->span);
  }

  if (!nodes[2]->is<Identifier>()) {
    ctx->error_log_.RHSNonIdInAccess(nodes[2]->span);
  } else {
    access->member_name = std::move(nodes[2]->as<Identifier>().token);
  }
  return access;
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
std::unique_ptr<Node> BuildIndexOperator(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto binop  = std::make_unique<Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[2]->span);
  binop->lhs  = move_as<Expression>(nodes[0]);
  binop->rhs  = move_as<Expression>(nodes[2]);
  binop->op   = Language::Operator::Index;

  if (binop->lhs->is<Declaration>()) {
    ctx->error_log_.IndexingDeclaration(binop->lhs->span);
  }

  if (binop->rhs->is<Declaration>()) {
    ctx->error_log_.DeclarationInIndex(binop->rhs->span);
  }

  return binop;
}

// Input guarantee:
// [expression] [l_bracket] [r_bracket]
//
// Internal checks: None
std::unique_ptr<Node> BuildEmptyArray(base::vector<std::unique_ptr<Node>> nodes,
                                      Context *ctx) {
  auto array_lit  = std::make_unique<ArrayLiteral>();
  array_lit->span = TextSpan(nodes[0]->span, nodes[2]->span);
  return array_lit;
}

std::unique_ptr<Node> BuildEmptyCommaList(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto comma_list  = std::make_unique<CommaList>();
  comma_list->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return comma_list;
}

std::unique_ptr<Node> BuildArrayLiteral(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto array_lit  = std::make_unique<ArrayLiteral>();
  array_lit->span = nodes[0]->span;

  if (nodes[1]->is<CommaList>() && !nodes[1]->as<CommaList>().parenthesized_) {
    array_lit->cl_.exprs_ = std::move(nodes[1]->as<CommaList>().exprs_);
  } else {
    array_lit->cl_.exprs_.push_back(move_as<Expression>(nodes[1]));
  }

  return array_lit;
}

std::unique_ptr<Node> BuildGenericStructType(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  auto result = std::make_unique<StructType>(
      TextSpan(nodes.front()->span, nodes.back()->span));
  if (nodes[1]->is<CommaList>() && !nodes[1]->as<CommaList>().parenthesized_) {
    result->args_ = std::move(nodes[1]->as<CommaList>().exprs_);
  } else {
    result->args_.push_back(move_as<Expression>(nodes[1]));
  }

  return result;
}

std::unique_ptr<Node> BuildArrayType(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  if (nodes[1]->is<CommaList>() && !nodes[1]->as<CommaList>().parenthesized_) {
    auto *length_chain = &nodes[1]->as<CommaList>();
    int i              = static_cast<int>(length_chain->exprs_.size() - 1);
    auto prev          = move_as<Expression>(nodes[3]);

    while (i >= 0) {
      auto array_type        = std::make_unique<ast::ArrayType>();
      array_type->span       = length_chain->exprs_[i]->span;
      array_type->length_    = std::move(length_chain->exprs_[i]);
      array_type->data_type_ = std::move(prev);
      prev                   = std::move(array_type);
      i -= 1;
    }
    return prev;

  } else {
    auto array_type        = std::make_unique<ast::ArrayType>();
    array_type->span       = nodes[0]->span;
    array_type->length_    = move_as<Expression>(nodes[1]);
    array_type->data_type_ = move_as<Expression>(nodes[3]);

    return array_type;
  }
}

template <bool IsConst>
std::unique_ptr<Node> BuildDeclaration(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto op    = nodes[1]->as<frontend::Token>().op;
  auto decl  = std::make_unique<Declaration>(IsConst);
  decl->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[0]->is<Identifier>()) {
    decl->id_ = std::move(nodes[0]->as<Identifier>().token);
  }
  decl->mod_ = ctx->mod_;

  if (op == Language::Operator::Colon ||
      op == Language::Operator::DoubleColon) {
    decl->type_expr = move_as<Expression>(nodes[2]);
  } else {
    decl->init_val = move_as<Expression>(nodes[2]);
  }

  return decl;
}

std::unique_ptr<Node> BuildMatchDeclaration(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto decl  = std::make_unique<ast::MatchDeclaration>();
  decl->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[2]->is<Identifier>()) {
    decl->id_ = std::move(nodes[2]->as<Identifier>().token);
  }
  decl->mod_      = ctx->mod_;
  decl->type_expr = move_as<Expression>(nodes[0]);
  return decl;
}

base::vector<std::unique_ptr<Declaration>> ExtractInputs(
    std::unique_ptr<Expression> args) {
  base::vector<std::unique_ptr<Declaration>> inputs;
  if (args->is<Declaration>()) {
    inputs.push_back(move_as<Declaration>(args));

  } else if (args->is<CommaList>()) {
    auto *decls = &args->as<CommaList>();
    inputs.reserve(decls->exprs_.size());

    for (auto &expr : decls->exprs_) {
      inputs.push_back(move_as<Declaration>(expr));
    }
  } else {
    NOT_YET("log an error: ", args);
  }
  return inputs;
}

std::unique_ptr<Node> BuildFunctionLiteral(
    TextSpan span, base::vector<std::unique_ptr<Declaration>> inputs,
    std::unique_ptr<Expression> output, Statements &&stmts, Context *ctx) {
  auto fn     = std::make_unique<ast::FunctionLiteral>();
  fn->module_ = ASSERT_NOT_NULL(ctx->mod_);
  for (auto &input : inputs) {
    input->is_fn_param_ = true;
    // NOTE: This is safe because the declaration is behind a unique_ptr so the
    // string is never moved. You need to be careful if you ever decide to use
    // make this declaration inline because SSO might mean moving the
    // declaration (which can happen if FnParams internal vector gets
    // reallocated) could invalidate the string_view unintentionally.
    std::string_view name = input->id_;
    fn->inputs_.append(name, std::move(input));
  }

  fn->span        = std::move(span);
  fn->statements_ = std::move(stmts);

  if (output == nullptr) {
    fn->return_type_inferred_ = true;
  } else if (output->is<CommaList>()) {
    for (auto &expr : output->as<CommaList>().exprs_) {
      if (auto *decl = expr->if_as<Declaration>()) {
        decl->is_fn_param_ = true;
        decl->is_output_   = true;
      }
      fn->outputs_.push_back(std::move(expr));
    }
  } else {
    if (auto decl = output->if_as<Declaration>()) {
      decl->is_fn_param_ = true;
      decl->is_output_   = true;
    }
    fn->outputs_.push_back(std::move(output));
  }

  return fn;
}

std::unique_ptr<Node> BuildNormalFunctionLiteral(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto span   = TextSpan(nodes[0]->span, nodes.back()->span);
  auto *binop = &nodes[0]->as<Binop>();
  return BuildFunctionLiteral(
      std::move(span), ExtractInputs(std::move(binop->lhs)),
      std::move(binop->rhs), std::move(nodes[1]->as<Statements>()), ctx);
}

std::unique_ptr<Node> BuildInferredFunctionLiteral(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto span = TextSpan(nodes[0]->span, nodes.back()->span);
  return BuildFunctionLiteral(
      std::move(span), ExtractInputs(move_as<Expression>(nodes[0])), nullptr,
      std::move(nodes[2]->as<Statements>()), ctx);
}

std::unique_ptr<Node> BuildShortFunctionLiteral(
    std::unique_ptr<Expression> args, std::unique_ptr<Expression> body,
    Context *ctx) {
  auto span   = TextSpan(args->span, body->span);
  auto inputs = ExtractInputs(std::move(args));

  std::unique_ptr<RepeatedUnop> ret;
  if (body->is<CommaList>()) {
    ret = std::make_unique<RepeatedUnop>(
        TextSpan(body->as<CommaList>().exprs_.front()->span,
                 body->as<CommaList>().exprs_.back()->span));
    ret->op_   = Language::Operator::Return;
    ret->args_ = std::move(body->as<CommaList>());
  } else {
    ret      = std::make_unique<RepeatedUnop>(body->span);
    ret->op_ = Language::Operator::Return;
    ret->args_.exprs_.push_back(std::move(body));
  }

  Statements stmts;
  stmts.append(std::move(ret));
  return BuildFunctionLiteral(std::move(span), std::move(inputs), nullptr,
                              std::move(stmts), ctx);
}

std::unique_ptr<Node> BuildOneElementCommaList(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto comma_list  = std::make_unique<CommaList>();
  comma_list->span = TextSpan(nodes[0]->span, nodes[3]->span);
  comma_list->exprs_.push_back(move_as<Expression>(nodes[1]));
  comma_list->parenthesized_ = true;
  return comma_list;
}

std::unique_ptr<Node> BuildOneStatement(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto stmts  = std::make_unique<Statements>();
  stmts->span = nodes[0]->span;
  stmts->append(std::move(nodes[0]));
  ValidateStatementSyntax(stmts->content_.back().get(), ctx);
  return stmts;
}

std::unique_ptr<Node> BuildMoreStatements(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  std::unique_ptr<Statements> stmts = move_as<Statements>(nodes[0]);
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), ctx);
  return stmts;
}

std::unique_ptr<Node> OneBracedJump(base::vector<std::unique_ptr<Node>> nodes,
                                    Context *ctx) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->append(BuildJump(std::move(nodes[1])));
  ValidateStatementSyntax(stmts->content_.back().get(), ctx);
  return stmts;
}

std::unique_ptr<Node> BuildJump(base::vector<std::unique_ptr<Node>> nodes,
                                Context *) {
  return ::BuildJump(std::move(nodes[0]));
}

std::unique_ptr<Node> BuildScopeNode(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  auto scope_node  = std::make_unique<ScopeNode>();
  auto call        = move_as<Call>(nodes[0]);
  scope_node->name_ = std::move(call->fn_);
  scope_node->args_ = std::move(call->args_);

  scope_node->span = TextSpan(scope_node->name_->span, nodes[1]->span);
  scope_node->blocks_.push_back(std::move(nodes[1]->as<BlockNode>()));
  return scope_node;
}

std::unique_ptr<Node> BuildBlockNode(base::vector<std::unique_ptr<Node>> nodes,
                                     Context *ctx) {
  auto bn    = std::make_unique<BlockNode>();
  bn->span   = TextSpan(nodes[0]->span, nodes[1]->span);
  bn->name_  = move_as<Expression>(nodes[0]);
  bn->stmts_ = std::move(nodes[1]->as<Statements>());
  return bn;
}

std::unique_ptr<Node> ExtendScopeNode(base::vector<std::unique_ptr<Node>> nodes,
                                      Context *ctx) {
  auto &scope_node = nodes[0]->as<ScopeNode>();
  (scope_node.sugared_ ? scope_node.sugared_ : &scope_node)
      ->blocks_.push_back(std::move(nodes[1]->as<BlockNode>()));
  return std::move(nodes[0]);
}

std::unique_ptr<Node> SugaredExtendScopeNode(
    base::vector<std::unique_ptr<Node>> nodes, Context *ctx) {
  auto *scope_node = &nodes[0]->as<ScopeNode>();
  auto *extension_point = (scope_node->sugared_ != nullptr)
                              ? scope_node->sugared_
                              : &nodes[0]->as<ScopeNode>();

  auto &bn = extension_point->blocks_.emplace_back();
  // TODO span
  bn.name_ = move_as<Expression>(nodes[1]);
  // TODO hook this up to a yield when it exists
  scope_node->sugared_ = &nodes[2]->as<ScopeNode>();
  bn.stmts_.append(std::move(nodes[2]));
  return std::move(nodes[0]);
}

}  // namespace
}  // namespace ast

namespace {

struct Rule {
 public:
  using OptVec = base::vector<u64>;
  // TODO use spans
  using fnptr = std::unique_ptr<ast::Node> (*)(
      base::vector<std::unique_ptr<ast::Node>>, Context *ctx);

  Rule(frontend::Tag output, const OptVec &input, fnptr fn)
      : output_(output), input_(input), fn_(fn) {}

  size_t size() const { return input_.size(); }

  bool match(const base::vector<frontend::Tag> &tag_stack) const {
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

  void apply(base::vector<std::unique_ptr<ast::Node>> *node_stack,
             base::vector<frontend::Tag> *tag_stack, Context *ctx) const {
    // Make a vector for the rule function to take as input. It will begin with
    // size() shared_ptrs.
    base::vector<std::unique_ptr<ast::Node>> nodes_to_reduce;
    nodes_to_reduce.reserve(this->size());
    for (auto i = node_stack->size() - this->size(); i < node_stack->size();
         ++i) {
      nodes_to_reduce.push_back(std::move((*node_stack)[i]));
    }
    tag_stack->resize(node_stack->size() - this->size());
    node_stack->resize(node_stack->size() - this->size());

    node_stack->push_back(fn_(std::move(nodes_to_reduce), ctx));
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

static std::unique_ptr<ast::Node> BuildBinaryOperator(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  static const base::unordered_map<std::string, Language::Operator> chain_ops{
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
                 ? ast::BuildCommaList(std::move(nodes), ctx)
                 : ast::BuildChainOp(std::move(nodes), ctx);
    }
  }

  if (tk == ".") {
    return ast::BuildAccess(std::move(nodes), ctx);

  } else if (tk == "`") {
    return ast::BuildMatchDeclaration(std::move(nodes), ctx);

  } else if (tk == ":" || tk == ":=") {
    return ast::BuildDeclaration<false>(std::move(nodes), ctx);

  } else if (tk == "::" || tk == "::=") {
    return ast::BuildDeclaration<true>(std::move(nodes), ctx);

  } else if (tk == "=>") {
    return ast::BuildShortFunctionLiteral(move_as<ast::Expression>(nodes[0]),
                                          move_as<ast::Expression>(nodes[2]),
                                          ctx);
  } else if (tk == "=") {
    if (nodes[0]->is<ast::Declaration>()) {
      if (nodes[0]->as<ast::Declaration>().IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        ctx->error_log_.DoubleDeclAssignment(nodes[0]->span, nodes[1]->span);
        return move_as<ast::Declaration>(nodes[0]);
      }

      auto decl      = move_as<ast::Declaration>(nodes[0]);
      decl->init_val = move_as<ast::Expression>(nodes[2]);
      return decl;

    } else {
      auto binop  = std::make_unique<ast::Binop>();
      binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

      binop->lhs = move_as<ast::Expression>(nodes[0]);
      binop->rhs = move_as<ast::Expression>(nodes[2]);
      binop->op  = Language::Operator::Assign;
      return binop;
    }
  }

  if (tk == "(") {  // TODO these should just generate BuildCall directly.
    return ast::BuildCall(std::move(nodes), ctx);
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[2]);
    return ast::BuildCall(std::move(nodes), ctx);
  }

  auto binop  = std::make_unique<ast::Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

  binop->lhs = move_as<ast::Expression>(nodes[0]);
  binop->rhs = move_as<ast::Expression>(nodes[2]);

  static const base::unordered_map<std::string, Language::Operator> symbols = {
      {"->", Language::Operator::Arrow}, {"|=", Language::Operator::OrEq},
      {"&=", Language::Operator::AndEq}, {"^=", Language::Operator::XorEq},
      {"+=", Language::Operator::AddEq}, {"-=", Language::Operator::SubEq},
      {"*=", Language::Operator::MulEq}, {"/=", Language::Operator::DivEq},
      {"%=", Language::Operator::ModEq}, {"+", Language::Operator::Add},
      {"-", Language::Operator::Sub},    {"*", Language::Operator::Mul},
      {"/", Language::Operator::Div},    {"%", Language::Operator::Mod},
      {"[", Language::Operator::Index},  {"when", Language::Operator::When},
      {"as", Language::Operator::As}};
  {
    auto iter = symbols.find(tk);
    if (iter != symbols.end()) { binop->op = iter->second; }

    return binop;
  }
}

static std::unique_ptr<ast::Node> BuildEnumOrFlagLiteral(
    base::vector<std::unique_ptr<ast::Node>> nodes, bool is_enum,
    Context *ctx) {
  std::vector<std::unique_ptr<ast::Expression>> elems;
  if (auto *stmts = nodes[1]->if_as<ast::Statements>()) {
    // TODO if you want these values to depend on compile-time parameters,
    // you'll need to actually build the AST nodes.
    for (auto &stmt : stmts->content_) {
      ASSERT(stmt, Is<ast::Expression>());
      elems.push_back(move_as<ast::Expression>(stmt));
    }
  }

  return std::make_unique<ast::EnumLiteral>(
      std::move(elems), TextSpan(nodes[0]->span, nodes[1]->span), is_enum);
}

static std::unique_ptr<ast::Node> BuildInterfaceLiteral(
    std::unique_ptr<ast::Statements> stmts, Context *ctx) {
  auto interface_lit = std::make_unique<ast::Interface>();
  interface_lit->span =
      stmts->span;  // TODO it's really bigger than this because
                    // it involves the keyword too.
  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Declaration>()) {
      interface_lit->decls_.push_back(std::move(stmt->as<ast::Declaration>()));
    } else {
      NOT_YET(stmt);
    }
  }
  return interface_lit;
}

static std::unique_ptr<ast::Node> BuildScopeLiteral(
    std::unique_ptr<ast::Statements> stmts, TextSpan span, bool stateful, Context *ctx) {
  auto scope_lit  = std::make_unique<ast::ScopeLiteral>(stateful);
  scope_lit->span = std::move(span);  // TODO it's really bigger than this
                                      // because it involves the keyword too.
  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Declaration>()) {
      scope_lit->decls_.push_back(std::move(stmt->as<ast::Declaration>()));
    } else {
      NOT_YET(stmt);
    }
  }
  return scope_lit;
}

static std::unique_ptr<ast::Node> BuildBlock(
    std::unique_ptr<ast::Statements> stmts, bool required, Context *ctx) {
  auto block_expr  = std::make_unique<ast::BlockLiteral>(required);
  block_expr->span = stmts->span;  // TODO it's really bigger than this because
                                   // it involves the keyword too.

  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Declaration>()) {
      auto decl = move_as<ast::Declaration>(stmt);
      if (decl->id_ == "before") {
        block_expr->before_.push_back(std::move(decl));
      } else if (decl->id_ == "after") {
        block_expr->after_.push_back(std::move(decl));
      }
    } else {
      NOT_YET();
    }
  }

  return block_expr;
}

static std::unique_ptr<ast::Node> BuildSwitch(
    std::unique_ptr<ast::Statements> stmts, Context *ctx) {
  auto switch_expr  = std::make_unique<ast::Switch>();
  switch_expr->span = stmts->span;  // TODO it's really bigger than this because
                                    // it involves the keyword too.

  switch_expr->cases_.reserve(stmts->content_.size());
  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Binop>()) {
      auto binop = move_as<ast::Binop>(stmt);
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

static std::unique_ptr<ast::StructLiteral> BuildStructLiteral(
    ast::Statements &&stmts, TextSpan span, Context *ctx) {
  auto struct_lit  = std::make_unique<ast::StructLiteral>();
  struct_lit->mod_ = ctx->mod_;
  struct_lit->span = std::move(span);
  for (auto &&stmt : std::move(stmts).content_) {
    if (stmt->is<ast::Declaration>()) {
      struct_lit->fields_.push_back(move_as<ast::Declaration>(stmt));
    } else {
      ctx->error_log_.NonDeclarationInStructDeclaration(stmt->span);
      // TODO show the entire struct declaration and point to the problematic
      // lines.
    }
  }
  return struct_lit;
}

static std::unique_ptr<ast::Node> BuildGenericStruct(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto result = BuildStructLiteral(
      std::move(nodes[4]->as<ast::Statements>()),
      TextSpan(nodes.front()->span, nodes.back()->span), ctx);
  if (nodes[2]->is<ast::CommaList>()) {
    for (auto &expr : nodes[2]->as<ast::CommaList>().exprs_) {
      ASSERT(expr, Is<ast::Declaration>());  // TODO handle failure
      auto decl          = move_as<ast::Declaration>(expr);
      decl->is_fn_param_ = true;
      result->args_.push_back(std::move(decl));
    }
  } else {
    auto decl          = move_as<ast::Declaration>(nodes[2]);
    decl->is_fn_param_ = true;
    result->args_.push_back(std::move(decl));
  }
  return result;
}

static std::unique_ptr<ast::Node> BuildConcreteStruct(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  return BuildStructLiteral(std::move(nodes[1]->as<ast::Statements>()),
                            TextSpan(nodes.front()->span, nodes.back()->span),
                            ctx);
}

static std::unique_ptr<ast::Node> BuildKWBlock(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  if (nodes[0]->is<frontend::Token>()) {
    std::string const &tk = nodes[0]->as<frontend::Token>().token;

    if (bool is_enum = (tk == "enum"); is_enum || tk == "flags") {
      return BuildEnumOrFlagLiteral(std::move(nodes), is_enum, ctx);

    } else if (tk == "struct") {
      return BuildConcreteStruct(std::move(nodes), ctx);

    } else if (tk == "scope") {
      TextSpan span(nodes.front()->span, nodes.back()->span);
      return BuildScopeLiteral(move_as<ast::Statements>(nodes[1]), span, false,
                               ctx);

    } else if (tk == "scope!") {
      TextSpan span(nodes.front()->span, nodes.back()->span);
      return BuildScopeLiteral(move_as<ast::Statements>(nodes[1]), span, true,
                               ctx);

    } else if (tk == "interface") {
      return BuildInterfaceLiteral(move_as<ast::Statements>(nodes[1]), ctx);

    } else if (tk == "switch") {
      return BuildSwitch(move_as<ast::Statements>(nodes[1]), ctx);
    } else {
      UNREACHABLE(tk);
    }
  } else if (nodes[0]->is<ast::Terminal>()) {
    auto *t =
        std::get<const type::Type *>(nodes[0]->as<ast::Terminal>().value.value);

    if (t == type::Block) {
      return BuildBlock(move_as<ast::Statements>(nodes[1]), true, ctx);

    } else if (t == type::OptBlock) {
      return BuildBlock(move_as<ast::Statements>(nodes[1]), false, ctx);

    } else {
      UNREACHABLE(t);
    }
  } else {
    UNREACHABLE(nodes[0].get());
  }
}

static std::unique_ptr<ast::Node> Parenthesize(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto result            = move_as<ast::Expression>(nodes[1]);
  result->parenthesized_ = true;
  return result;
}

static std::unique_ptr<ast::Node> BuildEmptyParen(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto call  = std::make_unique<ast::Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = move_as<ast::Expression>(nodes[0]);

  if (call->fn_->is<ast::Declaration>()) {
    ctx->error_log_.CallingDeclaration(call->fn_->span);
  }
  return call;
}

template <size_t N>
static std::unique_ptr<ast::Node> drop_all_but(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  return std::move(nodes[N]);
}

static std::unique_ptr<ast::Node> CombineColonEq(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto *tk_node = &nodes[0]->as<frontend::Token>();
  tk_node->token += "=";  // Change : to := and :: to ::=
  tk_node->op = Language::Operator::ColonEq;
  return drop_all_but<0>(std::move(nodes), ctx);
}

std::unique_ptr<ast::Node> EmptyFile(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = nodes[0]->span;
  return std::move(stmts);
}

namespace ErrMsg {
template <size_t RTN, size_t RES>
static std::unique_ptr<ast::Node> Reserved(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  ctx->error_log_.Reserved(nodes[RES]->span,
                           nodes[RES]->as<frontend::Token>().token);

  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

template <size_t RTN, size_t RES1, size_t RES2>
static std::unique_ptr<ast::Node> BothReserved(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  ctx->error_log_.Reserved(nodes[RES1]->span,
                           nodes[RES1]->as<frontend::Token>().token);
  ctx->error_log_.Reserved(nodes[RES2]->span,
                           nodes[RES2]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

static std::unique_ptr<ast::Node> NonBinop(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  ctx->error_log_.NotBinary(nodes[1]->span,
                            nodes[1]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[1]->span, "invalid_node");
}

template <size_t RTN, size_t RES>
static std::unique_ptr<ast::Node> NonBinopReserved(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  ctx->error_log_.NotBinary(nodes[1]->span,
                            nodes[1]->as<frontend::Token>().token);
  ctx->error_log_.Reserved(nodes[RES]->span,
                           nodes[RES]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

static std::unique_ptr<ast::Node> NonBinopBothReserved(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  ctx->error_log_.Reserved(nodes[0]->span,
                           nodes[0]->as<frontend::Token>().token);
  ctx->error_log_.NotBinary(nodes[1]->span,
                            nodes[1]->as<frontend::Token>().token);
  ctx->error_log_.Reserved(nodes[2]->span,
                           nodes[2]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[1]->span, "invalid_node");
}
}  // namespace ErrMsg

static std::unique_ptr<ast::Node> BuildOperatorIdentifier(
    base::vector<std::unique_ptr<ast::Node>> nodes, Context *ctx) {
  auto span = nodes[1]->span;
  return std::make_unique<ast::Identifier>(
      span, move_as<frontend::Token>(nodes[1])->token);
}

namespace frontend {
static constexpr u64 OP_B = op_b | comma | colon | eq;
static constexpr u64 EXPR = expr | fn_expr | scope_expr | fn_call_expr;
// Used in error productions only!
static constexpr u64 RESERVED = kw_struct | kw_block_head | op_lt;
static constexpr u64 KW_BLOCK = kw_struct | kw_block_head | kw_block;
// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
auto Rules = std::array{
    Rule(fn_expr, {EXPR, fn_arrow, EXPR | kw_block}, BuildBinaryOperator),
    Rule(expr, {EXPR, (op_bl | OP_B), EXPR}, BuildBinaryOperator),
    Rule(op_b, {colon, eq}, CombineColonEq),
    Rule(fn_expr, {EXPR, fn_arrow, RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(fn_expr, {RESERVED, fn_arrow, EXPR | kw_block},
         ErrMsg::Reserved<1, 0>),
    Rule(fn_expr, {RESERVED, fn_arrow, RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),
    Rule(expr, {EXPR, (OP_B | op_bl), RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(expr, {RESERVED, (OP_B | op_bl), RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),
    Rule(expr, {EXPR, op_l, RESERVED}, ErrMsg::NonBinopReserved<1, 2>),
    Rule(expr, {RESERVED, op_l, RESERVED}, ErrMsg::NonBinopBothReserved),
    Rule(fn_call_expr, {EXPR, l_paren, EXPR, r_paren}, ast::BuildCall),
    Rule(fn_call_expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(expr, {l_paren, op_l | op_b | eq | op_bl, r_paren},
         BuildOperatorIdentifier),
    Rule(expr, {l_paren, r_paren}, ast::BuildEmptyCommaList),
    Rule(expr, {EXPR, l_bracket, EXPR, r_bracket}, ast::BuildIndexOperator),
    Rule(expr, {l_bracket, r_bracket}, ast::BuildEmptyArray),
    Rule(expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket},
         ast::BuildArrayType),
    Rule(expr, {l_bracket, EXPR, semicolon, kw_struct, r_bracket},
         ast::BuildGenericStructType),
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
    Rule(l_brace, {newline, l_brace}, drop_all_but<1>),
    Rule(stmts, {newline, stmts}, drop_all_but<1>),
    Rule(r_paren, {r_paren, newline}, drop_all_but<0>),
    Rule(r_bracket, {r_bracket, newline}, drop_all_but<0>),
    Rule(r_brace, {r_brace, newline}, drop_all_but<0>),
    Rule(braced_stmts, {l_brace, stmts, stmts | EXPR, r_brace},
         BracedStatementsSameLineEnd),
    Rule(braced_stmts, {l_brace, stmts, op_lt, r_brace},
         BracedStatementsJumpSameLineEnd),
    Rule(braced_stmts, {l_brace, stmts, r_brace}, drop_all_but<1>),
    Rule(braced_stmts, {l_brace, r_brace}, EmptyBraces),
    Rule(braced_stmts, {l_brace, EXPR, r_brace}, OneBracedStatement),
    Rule(braced_stmts, {l_brace, op_lt, r_brace}, ast::OneBracedJump),
    Rule(expr, {fn_expr, braced_stmts}, ast::BuildNormalFunctionLiteral),
    Rule(expr, {expr, fn_arrow, braced_stmts},
         ast::BuildInferredFunctionLiteral),
    Rule(hashtag, {hashtag, newline}, drop_all_but<0>),
    Rule(expr, {hashtag, expr}, AddHashtag),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case.
    Rule(expr, {EXPR, l_paren, RESERVED, r_paren}, ErrMsg::Reserved<0, 2>),
    Rule(expr, {EXPR, l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<0, 2>),

    Rule(expr, {EXPR, op_r}, ast::BuildRightUnop),
    Rule(expr, {(op_l | op_bl | op_lt), EXPR}, ast::BuildLeftUnop),
    Rule(expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    Rule(expr, {l_bracket, EXPR, r_bracket}, ast::BuildArrayLiteral),
    Rule(expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),
    Rule(stmts, {stmts, (EXPR | stmts), newline}, ast::BuildMoreStatements),
    Rule(expr, {kw_struct, l_paren, expr, r_paren, braced_stmts},
         BuildGenericStruct),
    Rule(expr, {KW_BLOCK, braced_stmts}, BuildKWBlock),
    Rule(expr, {KW_BLOCK, newline}, drop_all_but<0>),

    Rule(expr, {(op_l | op_bl | op_lt), RESERVED}, ErrMsg::Reserved<0, 1>),
    Rule(expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    // TODO does this rule prevent chained scope blocks on new lines or is it
    // preceeded by a shift rule that eats newlines after a right-brace?
    Rule(stmts, {EXPR, (newline | eof)}, ast::BuildOneStatement),
    Rule(expr, {l_paren, EXPR, comma, r_paren}, ast::BuildOneElementCommaList),
    Rule(comma, {comma, newline}, drop_all_but<0>),
    Rule(l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(stmts, {stmts, newline}, drop_all_but<0>),

    Rule(expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(stmts, {op_lt}, ast::BuildJump),
    Rule(block_expr, {expr, braced_stmts}, ast::BuildBlockNode),
    Rule(scope_expr, {fn_call_expr, block_expr}, ast::BuildScopeNode),
    Rule(scope_expr, {scope_expr, block_expr}, ast::ExtendScopeNode),
    Rule(scope_expr, {scope_expr, expr, scope_expr},
         ast::SugaredExtendScopeNode),
};

TaggedNode NextToken(SourceLocation &loc, error::Log *error_log);

namespace {
enum class ShiftState : char { NeedMore, EndOfExpr, MustReduce };
struct ParseState {
  ParseState(SourceLocation *loc, Context *ctx) : ctx_(ctx), loc_(loc) {
    lookahead_.push(frontend::TaggedNode(
        std::make_unique<frontend::Token>(loc_->ToSpan()), bof));
  }

  template <size_t N>
  inline frontend::Tag get_type() const {
    return tag_stack_[tag_stack_.size() - N];
  }

  template <size_t N>
  inline ast::Node *get() const {
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

    if (ahead.tag_ == l_brace && (get_type<1>() & kw_block) &&
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace && get_type<1>() == fn_expr &&
        get_type<2>() == fn_arrow) {
      return ShiftState::MustReduce;
    }

    if (ahead.tag_ == l_brace &&
        (get_type<1>() & (fn_expr | kw_block_head | kw_struct))) {
      return ShiftState::NeedMore;
    }

    if (get_type<1>() == newline && get_type<2>() == comma) {
      return ShiftState::MustReduce;
    }

    if (get_type<1>() == op_lt && ahead.tag_ != newline) {
      return ShiftState::NeedMore;
    }

    if ((get_type<1>() & (kw_block_head | kw_struct)) && ahead.tag_ == newline) {
      return ShiftState::NeedMore;
    }

    if ((get_type<2>() & (kw_block_head | kw_struct)) && get_type<1>() == newline) {
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

    constexpr u64 OP = hashtag | op_r | op_l | op_b | colon | eq | comma |
                       op_bl | op_lt | fn_arrow;
    if (get_type<2>() & OP) {
      if (get_type<1>() == r_paren) {
        // TODO this feels like a hack, but maybe this whole function is.
        return ShiftState::MustReduce;
      }
      auto left_prec = precedence(get<2>()->as<frontend::Token>().op);
      size_t right_prec;
      if (ahead.tag_ & OP) {
        right_prec = precedence(ahead.node_->as<frontend::Token>().op);
      } else if (ahead.tag_ == l_bracket) {
        right_prec = precedence(Operator::Index);

      } else if (ahead.tag_ == l_paren) {
        // TODO this might be a hack. To get the following example to parse
        // correctly:
        //
        //    #tag
        //    (+) ::= ...
        //
        // As it stands we're assuming there's some expression being called
        // between the tag and the paren where the newline actually is. We can
        // get around this here by just explicitly checking that case, but
        // perhaps we should actually just lex "(+)" as it's own symbol with
        // it's own tag type. That might be more robust.
        if (get_type<1>() == newline) { return ShiftState::MustReduce; }
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

  void LookAhead() { lookahead_.push(NextToken(*loc_, &ctx_->error_log_)); }

  const TaggedNode &Next() {
    if (lookahead_.empty()) { LookAhead(); }
    return lookahead_.back();
  }

  base::vector<frontend::Tag> tag_stack_;
  base::vector<std::unique_ptr<ast::Node>> node_stack_;
  std::queue<frontend::TaggedNode> lookahead_;
  Context *ctx_        = nullptr;
  SourceLocation *loc_ = nullptr;

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
    fprintf(stderr, "%s\n", ps->loc_->line().c_str());
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
  if (tag_ahead &
      (frontend::l_paren | frontend::l_bracket | frontend::l_brace)) {
    ++ps->brace_count;
  } else if (tag_ahead &
             (frontend::r_paren | frontend::r_bracket | frontend::r_brace)) {
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

  ASSERT_NOT_NULL(ps->node_stack_.back()->span.source);
  matched_rule_ptr->apply(&ps->node_stack_, &ps->tag_stack_, ps->ctx_);
  ASSERT_NOT_NULL(ps->node_stack_.back()->span.source);

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

std::unique_ptr<ast::Statements> frontend::Repl::Parse(Context *ctx) {
  first_entry = true;  // Show '>> ' the first time.

  SourceLocation loc;
  loc.source = this;

  auto state = frontend::ParseState(&loc, ctx);
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
        return move_as<ast::Statements>(state.node_stack_.back());
      case frontend::ShiftState::MustReduce:
        Reduce(&state) || (Shift(&state), true);
        if (debug::parser) { Debug(&state); }
    }
  }
}

std::unique_ptr<ast::Statements> frontend::File::Parse(Context *ctx) {
  SourceLocation loc;
  loc.source = this;

  auto state = frontend::ParseState(&loc, ctx);
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
    base::vector<TextSpan> lines;

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
    ctx->error_log_.UnknownParseError(lines);
  }

  return move_as<ast::Statements>(state.node_stack_.back());
}
