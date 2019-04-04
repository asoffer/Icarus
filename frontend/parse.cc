#include <array>
#include <cstdio>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/access.h"
#include "ast/array_literal.h"
#include "ast/array_type.h"
#include "ast/binop.h"
#include "ast/block_literal.h"
#include "ast/call.h"
#include "ast/cast.h"
#include "ast/chainop.h"
#include "ast/comma_list.h"
#include "ast/declaration.h"
#include "ast/enum_literal.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/import.h"
#include "ast/index.h"
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
#include "error/log.h"
#include "frontend/lex.h"
#include "frontend/operators.h"
#include "frontend/source.h"
#include "frontend/tagged_node.h"
#include "frontend/token.h"
#include "type/enum.h"
#include "type/flags.h"

using ::matcher::InheritsFrom;

namespace debug {
bool parser     = false;
}  // namespace debug

namespace frontend {
namespace {

template <typename To, typename From>
std::unique_ptr<To> move_as(std::unique_ptr<From> &val) {
  ASSERT(val, InheritsFrom<To>());
  return std::unique_ptr<To>(static_cast<To *>(val.release()));
}

void ValidateStatementSyntax(ast::Node *node, Module *mod,
                             error::Log *error_log) {
  if (node->is<ast::CommaList>()) {
    error_log->CommaListStatement(node->as<ast::CommaList>().span);
  }
}

constexpr size_t left_assoc  = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc   = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask  = 3;

constexpr size_t precedence(frontend::Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  case frontend::Operator::name:                                               \
    return (((prec) << 2) + (assoc));
#include "frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
  }
  __builtin_unreachable();
}

std::map<std::string, ast::Hashtag::Builtin> const BuiltinHashtagMap = {
    {"{export}", ast::Hashtag::Builtin::Export},
    {"{uncopyable}", ast::Hashtag::Builtin::Uncopyable},
    {"{immovable}", ast::Hashtag::Builtin::Immovable},
    {"{no_default}", ast::Hashtag::Builtin::NoDefault}};
std::unique_ptr<ast::Node> AddHashtag(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
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

std::unique_ptr<ast::Switch> BuildSwitch(std::unique_ptr<ast::Statements> stmts,
                                         Module *mod, error::Log *error_log) {
  auto switch_expr  = std::make_unique<ast::Switch>();
  switch_expr->span = stmts->span;  // TODO it's really bigger than this because
                                    // it involves the keyword too.

  switch_expr->cases_.reserve(stmts->content_.size());
  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Binop>()) {
      auto binop = move_as<ast::Binop>(stmt);
      if (binop->op == frontend::Operator::When) {
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

std::unique_ptr<ast::Node> OneBracedStatement(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
  return stmts;
}

std::unique_ptr<ast::Node> EmptyBraces(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return stmts;
}

std::unique_ptr<ast::Node> BuildJump(std::unique_ptr<ast::Node> node) {
  using ::frontend::Operator;
  auto &tk   = node->as<frontend::Token>().token;
  auto jmp   = std::make_unique<ast::RepeatedUnop>(node->span);
  jmp->op_   = tk == "return" ? Operator::Return : Operator::Yield;
  auto stmts = std::make_unique<ast::Statements>();
  stmts->append(std::move(jmp));
  stmts->span = node->span;
  return stmts;
}

std::unique_ptr<ast::Node> BracedStatementsSameLineEnd(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto stmts  = move_as<ast::Statements>(nodes[1]);
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[2]->is<ast::Statements>()) {
    for (auto &stmt : nodes[2]->as<ast::Statements>().content_) {
      stmts->append(std::move(stmt));
      ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
    }
  } else {
    stmts->append(std::move(nodes[2]));
    ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
  }
  return stmts;
}

std::unique_ptr<ast::Node> BracedStatementsJumpSameLineEnd(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  nodes[2] = BuildJump(std::move(nodes[2]));
  return BracedStatementsSameLineEnd(std::move(nodes), mod, error_log);
}

std::unique_ptr<ast::Node> BuildRightUnop(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  const std::string &tk = nodes[1]->as<frontend::Token>().token;
  if (tk == ":?") {
    auto unop     = std::make_unique<ast::Unop>();
    unop->operand = move_as<ast::Expression>(nodes[0]);
    unop->op      = frontend::Operator::TypeOf;
    unop->span    = TextSpan(unop->operand->span, nodes[1]->span);

    if (unop->operand->is<ast::Declaration>()) {
      error_log->DeclarationUsedInUnop(tk, unop->operand->span);
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
std::unique_ptr<ast::Node> BuildCall(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto call  = std::make_unique<ast::Call>();
  call->span = TextSpan(nodes.front()->span, nodes.back()->span);
  call->fn_  = move_as<ast::Expression>(nodes[0]);

  if (nodes[2]->is<ast::CommaList>()) {
    std::optional<TextSpan> last_named_span_before_error = std::nullopt;
    std::vector<TextSpan> positional_error_spans;

    for (auto &expr : nodes[2]->as<ast::CommaList>().exprs_) {
      if (expr->is<ast::Binop>() &&
          expr->as<ast::Binop>().op == frontend::Operator::Assign) {
        if (positional_error_spans.empty()) {
          last_named_span_before_error = expr->as<ast::Binop>().lhs->span;
        }
        call->args_.named_emplace(
            std::move(expr->as<ast::Binop>().lhs->as<ast::Identifier>().token),
            std::move(expr->as<ast::Binop>().rhs));
      } else {
        if (last_named_span_before_error.has_value()) {
          positional_error_spans.push_back(expr->span);
        }
        call->args_.pos_emplace(std::move(expr));
      }
    }

    if (!positional_error_spans.empty()) {
      error_log->PositionalArgumentFollowingNamed(
          positional_error_spans, *last_named_span_before_error);
    }
  } else {
    if (nodes[2]->is<ast::Binop>() &&
        nodes[2]->as<ast::Binop>().op == frontend::Operator::Assign) {
      call->args_.named_emplace(
          std::move(
              nodes[2]->as<ast::Binop>().lhs->as<ast::Identifier>().token),
          std::move(nodes[2]->as<ast::Binop>().rhs));
    } else {
      call->args_.pos_emplace(move_as<ast::Expression>(nodes[2]));
    }
  }

  if (call->fn_->is<ast::Declaration>()) {
    error_log->CallingDeclaration(call->fn_->span);
  }
  return call;
}

// Input guarantees:
// [unop] [expression]
//
// Internal checks:
// Operand cannot be a declaration.
// Operand cannot be an assignment of any kind.
std::unique_ptr<ast::Node> BuildLeftUnop(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  const std::string &tk = nodes[0]->as<frontend::Token>().token;

  using frontend::Operator;
  if (tk == "import") {
    auto import_node =
        std::make_unique<ast::Import>(move_as<ast::Expression>(nodes[1]));
    import_node->span = TextSpan(nodes[0]->span, import_node->operand_->span);
    return import_node;
  } else if (tk == "return" || tk == "yield") {
    auto unop = std::make_unique<ast::RepeatedUnop>(
        TextSpan(nodes.front()->span, nodes.back()->span));
    unop->op_ = tk == "return" ? Operator::Return : Operator::Yield;
    if (nodes[1]->is<ast::CommaList>() &&
        !nodes[1]->as<ast::CommaList>().parenthesized_) {
      unop->args_ = std::move(nodes[1]->as<ast::CommaList>());
    } else {
      unop->args_.exprs_.push_back(move_as<ast::Expression>(nodes[1]));
      unop->args_.span = TextSpan(unop->args_.exprs_.front()->span,
                                  unop->args_.exprs_.back()->span);
    }
    return unop;
  } else if (tk == "print") {
    // TODO Copy of above.
    std::unique_ptr<ast::RepeatedUnop> unop;
    if (nodes[1]->is<ast::CommaList>() &&
        !nodes[1]->as<ast::CommaList>().parenthesized_) {
      unop = std::make_unique<ast::RepeatedUnop>(
          TextSpan(nodes.front()->span, nodes.back()->span));
      unop->args_ = std::move(nodes[1]->as<ast::CommaList>());
    } else {
      unop = std::make_unique<ast::RepeatedUnop>(nodes[1]->span);
      unop->args_.exprs_.push_back(move_as<ast::Expression>(nodes[1]));
    }
    unop->op_ = Operator::Print;
    return unop;
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[1]);
    nodes.push_back(std::make_unique<ast::CommaList>());
    nodes.back()->span = nodes[0]->span;
    return BuildCall(std::move(nodes), mod, error_log);
  }

  auto unop     = std::make_unique<ast::Unop>();
  unop->operand = move_as<ast::Expression>(nodes[1]);
  unop->span    = TextSpan(nodes[0]->span, unop->operand->span);

  static absl::flat_hash_map<std::string_view, Operator> const UnopMap{
      {"*", Operator::Mul},     {"[*]", Operator::BufPtr},
      {"@", Operator::At},      {"import", Operator::Import},
      {"&", Operator::And},     {"which", Operator::Which},
      {"-", Operator::Sub},     {"needs", Operator::Needs},
      {"!", Operator::Not},     {"ensure", Operator::Ensure},
      {"<<", Operator::Expand}, {"copy", frontend::Operator::Copy},
      {"$", Operator::Eval},    {"move", frontend::Operator::Move}};
  unop->op = UnopMap.at(tk);

  if (unop->operand->is<ast::Declaration>()) {
    error_log->DeclarationUsedInUnop(tk, unop->operand->span);
  }
  return unop;
}

// Input guarantees
// [expr] [chainop] [expr]
//
// Internal checks: None
std::unique_ptr<ast::Node> BuildChainOp(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto op = nodes[1]->as<frontend::Token>().op;
  std::unique_ptr<ast::ChainOp> chain;

  // Add to a chain so long as the precedence levels match. The only thing at
  // that precedence level should be the operators which can be chained.
  if (nodes[0]->is<ast::ChainOp>() &&
      precedence(nodes[0]->as<ast::ChainOp>().ops.front()) == precedence(op)) {
    chain = move_as<ast::ChainOp>(nodes[0]);

  } else {
    chain       = std::make_unique<ast::ChainOp>();
    chain->span = TextSpan(nodes[0]->span, nodes[2]->span);

    chain->exprs.push_back(move_as<ast::Expression>(nodes[0]));
  }

  chain->ops.push_back(op);
  chain->exprs.push_back(move_as<ast::Expression>(nodes[2]));
  return chain;
}

std::unique_ptr<ast::Node> BuildCommaList(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  std::unique_ptr<ast::CommaList> comma_list = nullptr;
  if (nodes[0]->is<ast::CommaList>() &&
      !nodes[0]->as<ast::CommaList>().parenthesized_) {
    comma_list = move_as<ast::CommaList>(nodes[0]);
  } else {
    comma_list       = std::make_unique<ast::CommaList>();
    comma_list->span = TextSpan(nodes[0]->span, nodes[2]->span);
    comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[0]));
  }
  comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[2]));
  comma_list->span.finish = comma_list->exprs_.back()->span.finish;
  return comma_list;
}

// Input guarantees
// [expr] [dot] [expr]
//
// Internal checks:
// LHS is not a declaration
// RHS is an identifier
std::unique_ptr<ast::Node> BuildAccess(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto access     = std::make_unique<ast::Access>();
  access->span    = TextSpan(nodes[0]->span, nodes[2]->span);
  access->operand = move_as<ast::Expression>(nodes[0]);

  if (access->operand->is<ast::Declaration>()) {
    error_log->DeclarationInAccess(access->operand->span);
  }

  if (!nodes[2]->is<ast::Identifier>()) {
    error_log->RHSNonIdInAccess(nodes[2]->span);
  } else {
    access->member_name = std::move(nodes[2]->as<ast::Identifier>().token);
  }
  return access;
}

// Input guarantees
// [expr] [l_bracket] [expr] [r_bracket]
//
// Internal checks:
// LHS is not a declaration
// RHS is not a declaration
std::unique_ptr<ast::Node> BuildIndexOperator(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto index  = std::make_unique<ast::Index>();
  index->span = TextSpan(nodes[0]->span, nodes[2]->span);
  index->lhs_ = move_as<ast::Expression>(nodes[0]);
  index->rhs_ = move_as<ast::Expression>(nodes[2]);

  if (index->lhs_->is<ast::Declaration>()) {
    error_log->IndexingDeclaration(index->lhs_->span);
  }

  if (index->rhs_->is<ast::Declaration>()) {
    error_log->DeclarationInIndex(index->rhs_->span);
  }

  return index;
}

std::unique_ptr<ast::Node> BuildEmptyArray(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  return std::make_unique<ast::ArrayLiteral>(
      TextSpan(nodes.front()->span, nodes.back()->span));
}

std::unique_ptr<ast::Node> BuildEmptyCommaList(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto comma_list  = std::make_unique<ast::CommaList>();
  comma_list->span = TextSpan(nodes[0]->span, nodes[1]->span);
  return comma_list;
}

std::unique_ptr<ast::Node> BuildArrayLiteral(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto array_lit = std::make_unique<ast::ArrayLiteral>(nodes[0]->span);

  if (nodes[1]->is<ast::CommaList>() &&
      !nodes[1]->as<ast::CommaList>().parenthesized_) {
    array_lit->cl_.exprs_ = std::move(nodes[1]->as<ast::CommaList>().exprs_);
  } else {
    array_lit->cl_.exprs_.push_back(move_as<ast::Expression>(nodes[1]));
  }

  return array_lit;
}

std::unique_ptr<ast::Node> BuildGenericStructType(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto result = std::make_unique<ast::StructType>(
      TextSpan(nodes.front()->span, nodes.back()->span));
  if (nodes[1]->is<ast::CommaList>() &&
      !nodes[1]->as<ast::CommaList>().parenthesized_) {
    result->args_ = std::move(nodes[1]->as<ast::CommaList>().exprs_);
  } else {
    result->args_.push_back(move_as<ast::Expression>(nodes[1]));
  }

  return result;
}

std::unique_ptr<ast::Node> BuildArrayType(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  if (nodes[1]->is<ast::CommaList>() &&
      !nodes[1]->as<ast::CommaList>().parenthesized_) {
    auto *length_chain = &nodes[1]->as<ast::CommaList>();
    int i              = static_cast<int>(length_chain->exprs_.size() - 1);
    auto prev          = move_as<ast::Expression>(nodes[3]);

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
    array_type->length_    = move_as<ast::Expression>(nodes[1]);
    array_type->data_type_ = move_as<ast::Expression>(nodes[3]);

    return array_type;
  }
}

template <bool IsConst>
std::unique_ptr<ast::Node> BuildDeclaration(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto op    = nodes[1]->as<frontend::Token>().op;
  auto decl  = std::make_unique<ast::Declaration>(IsConst);
  ASSERT(nodes[0]->span.source != nullptr);
  decl->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[0]->is<ast::Identifier>()) {
    decl->id_ = std::move(nodes[0]->as<ast::Identifier>().token);
  }
  decl->mod_ = mod;

  if (op == frontend::Operator::Colon ||
      op == frontend::Operator::DoubleColon) {
    decl->type_expr = move_as<ast::Expression>(nodes[2]);
  } else {
    decl->init_val = move_as<ast::Expression>(nodes[2]);
  }

  return decl;
}

std::unique_ptr<ast::Node> BuildMatchDeclaration(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto decl  = std::make_unique<ast::MatchDeclaration>();
  decl->span = TextSpan(nodes[0]->span, nodes[2]->span);
  if (nodes[2]->is<ast::Identifier>()) {
    decl->id_ = std::move(nodes[2]->as<ast::Identifier>().token);
  }
  decl->mod_      = mod;
  decl->const_    = true;
  decl->type_expr = move_as<ast::Expression>(nodes[0]);
  return decl;
}

std::vector<std::unique_ptr<ast::Declaration>> ExtractInputs(
    std::unique_ptr<ast::Expression> args) {
  std::vector<std::unique_ptr<ast::Declaration>> inputs;
  if (args->is<ast::Declaration>()) {
    inputs.push_back(move_as<ast::Declaration>(args));

  } else if (args->is<ast::CommaList>()) {
    auto *decls = &args->as<ast::CommaList>();
    inputs.reserve(decls->exprs_.size());

    for (auto &expr : decls->exprs_) {
      if (expr->is<ast::Declaration>()) {
        inputs.push_back(move_as<ast::Declaration>(expr));
      } else {
        NOT_YET("log an error: ", args);
      }
    }
  } else {
    NOT_YET("log an error: ", args);
  }
  return inputs;
}

std::unique_ptr<ast::Node> BuildFunctionLiteral(
    TextSpan span, std::vector<std::unique_ptr<ast::Declaration>> inputs,
    std::unique_ptr<ast::Expression> output, ast::Statements &&stmts,
    Module *mod, error::Log *error_log) {
  auto fn     = std::make_unique<ast::FunctionLiteral>();
  fn->module_ = ASSERT_NOT_NULL(mod);
  for (auto &input : inputs) {
    input->is_fn_param_ = true;
    // NOTE: This is safe because the declaration is behind a unique_ptr so the
    // string is never moved. You need to be careful if you ever decide to use
    // make this declaration inline because SSO might mean moving the
    // declaration (which can happen if core::FnParams internal vector gets
    // reallocated) could invalidate the string_view unintentionally.
    std::string_view name = input->id_;

    // Note the weird naming here: A declaration which is default initialized
    // means there is no `=` as part of the declaration. This means that the
    // declaration, when thougth of as a parameter to a function, has no default
    // value.
    core::FnParamFlags flags{};
    if (!input->IsDefaultInitialized()) { flags = core::HAS_DEFAULT; }

    fn->inputs_.append(name, std::move(input), flags);
  }

  fn->span        = std::move(span);
  fn->statements_ = std::move(stmts);

  if (output == nullptr) {
    fn->return_type_inferred_ = true;
  } else if (output->is<ast::CommaList>()) {
    for (auto &expr : output->as<ast::CommaList>().exprs_) {
      if (auto *decl = expr->if_as<ast::Declaration>()) {
        decl->is_fn_param_ = true;
        decl->is_output_   = true;
      }
      fn->outputs_.push_back(std::move(expr));
    }
  } else {
    if (auto decl = output->if_as<ast::Declaration>()) {
      decl->is_fn_param_ = true;
      decl->is_output_   = true;
    }
    fn->outputs_.push_back(std::move(output));
  }

  return fn;
}

std::unique_ptr<ast::Node> BuildNormalFunctionLiteral(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto span   = TextSpan(nodes[0]->span, nodes.back()->span);
  auto *binop = &nodes[0]->as<ast::Binop>();
  return BuildFunctionLiteral(
      std::move(span), ExtractInputs(std::move(binop->lhs)),
      std::move(binop->rhs), std::move(nodes[1]->as<ast::Statements>()), mod,
      error_log);
}

std::unique_ptr<ast::Node> BuildInferredFunctionLiteral(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto span = TextSpan(nodes[0]->span, nodes.back()->span);
  return BuildFunctionLiteral(
      std::move(span), ExtractInputs(move_as<ast::Expression>(nodes[0])),
      nullptr, std::move(nodes[2]->as<ast::Statements>()), mod, error_log);
}

std::unique_ptr<ast::Node> BuildShortFunctionLiteral(
    std::unique_ptr<ast::Expression> args,
    std::unique_ptr<ast::Expression> body, Module *mod, error::Log *error_log) {
  auto span   = TextSpan(args->span, body->span);
  auto inputs = ExtractInputs(std::move(args));

  std::unique_ptr<ast::RepeatedUnop> ret;
  if (body->is<ast::CommaList>()) {
    ret = std::make_unique<ast::RepeatedUnop>(
        TextSpan(body->as<ast::CommaList>().exprs_.front()->span,
                 body->as<ast::CommaList>().exprs_.back()->span));
    ret->op_   = frontend::Operator::Return;
    ret->args_ = std::move(body->as<ast::CommaList>());
  } else {
    ret      = std::make_unique<ast::RepeatedUnop>(body->span);
    ret->op_ = frontend::Operator::Return;
    ret->args_.exprs_.push_back(std::move(body));
  }

  ast::Statements stmts;
  stmts.append(std::move(ret));
  return BuildFunctionLiteral(std::move(span), std::move(inputs), nullptr,
                              std::move(stmts), mod, error_log);
}

std::unique_ptr<ast::Node> BuildOneElementCommaList(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto comma_list  = std::make_unique<ast::CommaList>();
  comma_list->span = TextSpan(nodes[0]->span, nodes[3]->span);
  comma_list->exprs_.push_back(move_as<ast::Expression>(nodes[1]));
  comma_list->parenthesized_ = true;
  return comma_list;
}

std::unique_ptr<ast::Node> BuildOneStatement(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = nodes[0]->span;
  stmts->append(std::move(nodes[0]));
  ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
  return stmts;
}

std::unique_ptr<ast::Node> BuildMoreStatements(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  std::unique_ptr<ast::Statements> stmts = move_as<ast::Statements>(nodes[0]);
  stmts->append(std::move(nodes[1]));
  ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
  return stmts;
}

std::unique_ptr<ast::Node> OneBracedJump(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto stmts  = std::make_unique<ast::Statements>();
  stmts->span = TextSpan(nodes[0]->span, nodes[2]->span);
  stmts->append(BuildJump(std::move(nodes[1])));
  ValidateStatementSyntax(stmts->content_.back().get(), mod, error_log);
  return stmts;
}

std::unique_ptr<ast::Node> BuildJump(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *, error::Log *) {
  return BuildJump(std::move(nodes[0]));
}

std::unique_ptr<ast::Node> BuildScopeNode(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto scope_node   = std::make_unique<ast::ScopeNode>();
  auto call         = move_as<ast::Call>(nodes[0]);
  scope_node->name_ = std::move(call->fn_);
  scope_node->args_ = std::move(call->args_);

  scope_node->span = TextSpan(scope_node->name_->span, nodes[1]->span);
  scope_node->blocks_.push_back(std::move(nodes[1]->as<ast::BlockNode>()));
  return scope_node;
}

std::unique_ptr<ast::Node> BuildBlockNode(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto bn    = std::make_unique<ast::BlockNode>();
  bn->span   = TextSpan(nodes[0]->span, nodes[1]->span);
  bn->name_  = move_as<ast::Expression>(nodes[0]);
  bn->stmts_ = std::move(nodes[1]->as<ast::Statements>());
  return bn;
}

std::unique_ptr<ast::Node> ExtendScopeNode(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto &scope_node = nodes[0]->as<ast::ScopeNode>();
  (scope_node.sugared_ ? scope_node.sugared_ : &scope_node)
      ->blocks_.push_back(std::move(nodes[1]->as<ast::BlockNode>()));
  return std::move(nodes[0]);
}

std::unique_ptr<ast::Node> SugaredExtendScopeNode(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto *scope_node      = &nodes[0]->as<ast::ScopeNode>();
  auto *extension_point = (scope_node->sugared_ != nullptr)
                              ? scope_node->sugared_
                              : &nodes[0]->as<ast::ScopeNode>();

  auto &bn = extension_point->blocks_.emplace_back();
  // TODO span
  bn.name_ = move_as<ast::Expression>(nodes[1]);
  // TODO hook this up to a yield when it exists
  scope_node->sugared_ = &nodes[2]->as<ast::ScopeNode>();
  bn.stmts_.append(std::move(nodes[2]));
  return std::move(nodes[0]);
}

struct Rule {
 public:
  using OptVec = std::vector<uint64_t>;
  // TODO use spans
  using fnptr =
      std::unique_ptr<ast::Node> (*)(std::vector<std::unique_ptr<ast::Node>>,
                                     Module *mod, error::Log *error_log);

  Rule(frontend::Tag output, OptVec const &input, fnptr fn)
      : output_(output), input_(input), fn_(fn) {}

  size_t size() const { return input_.size(); }

  bool match(std::vector<frontend::Tag> const &tag_stack) const {
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

  void apply(std::vector<std::unique_ptr<ast::Node>> *node_stack,
             std::vector<frontend::Tag> *tag_stack, Module *mod,
             error::Log *error_log) const {
    std::vector<std::unique_ptr<ast::Node>> nodes_to_reduce;
    nodes_to_reduce.reserve(this->size());
    for (auto i = node_stack->size() - this->size(); i < node_stack->size();
         ++i) {
      nodes_to_reduce.push_back(std::move((*node_stack)[i]));
    }
    tag_stack->resize(node_stack->size() - this->size());
    node_stack->resize(node_stack->size() - this->size());

    node_stack->push_back(fn_(std::move(nodes_to_reduce), mod, error_log));
    tag_stack->push_back(output_);
  }

 private:
  frontend::Tag output_;
  OptVec input_;
  fnptr fn_;
};

std::unique_ptr<ast::Node> BuildBinaryOperator(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  static absl::flat_hash_map<std::string_view, frontend::Operator> const
      chain_ops{
          {",", frontend::Operator::Comma}, {"==", frontend::Operator::Eq},
          {"!=", frontend::Operator::Ne},   {"<", frontend::Operator::Lt},
          {">", frontend::Operator::Gt},    {"<=", frontend::Operator::Le},
          {">=", frontend::Operator::Ge},   {"&", frontend::Operator::And},
          {"|", frontend::Operator::Or},    {"^", frontend::Operator::Xor},
      };

  std::string const &tk = nodes[1]->as<frontend::Token>().token;
  {
    auto iter = chain_ops.find(tk);
    if (iter != chain_ops.end()) {
      nodes[1]->as<frontend::Token>().op = iter->second;
      return (iter->second == frontend::Operator::Comma)
                 ? BuildCommaList(std::move(nodes), mod, error_log)
                 : BuildChainOp(std::move(nodes), mod, error_log);
    }
  }

  if (tk == ".") {
    return BuildAccess(std::move(nodes), mod, error_log);

  } else if (tk == "`") {
    return BuildMatchDeclaration(std::move(nodes), mod, error_log);

  } else if (tk == ":" || tk == ":=") {
    return BuildDeclaration<false>(std::move(nodes), mod, error_log);

  } else if (tk == "::" || tk == "::=") {
    return BuildDeclaration<true>(std::move(nodes), mod, error_log);

  } else if (tk == "=>") {
    return BuildShortFunctionLiteral(move_as<ast::Expression>(nodes[0]),
                                     move_as<ast::Expression>(nodes[2]), mod,
                                     error_log);
  } else if (tk == "=") {
    if (nodes[0]->is<ast::Declaration>()) {
      if (nodes[0]->as<ast::Declaration>().IsInferred()) {
        // NOTE: It might be that this was supposed to be a bool ==? How can we
        // give a good error message if that's what is intended?
        error_log->DoubleDeclAssignment(nodes[0]->span, nodes[1]->span);
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
      binop->op  = frontend::Operator::Assign;
      return binop;
    }
  } else if (tk == "as") {
    auto cast   = std::make_unique<ast::Cast>();
    cast->span  = TextSpan(nodes[0]->span, nodes[2]->span);
    cast->expr_ = move_as<ast::Expression>(nodes[0]);
    cast->type_ = move_as<ast::Expression>(nodes[2]);
    return cast;
  }

  if (tk == "(") {  // TODO these should just generate BuildCall directly.
    return BuildCall(std::move(nodes), mod, error_log);
  } else if (tk == "'") {
    std::swap(nodes[0], nodes[2]);
    return BuildCall(std::move(nodes), mod, error_log);
  }

  auto binop  = std::make_unique<ast::Binop>();
  binop->span = TextSpan(nodes[0]->span, nodes[2]->span);

  binop->lhs = move_as<ast::Expression>(nodes[0]);
  binop->rhs = move_as<ast::Expression>(nodes[2]);

  static absl::flat_hash_map<std::string_view, frontend::Operator> const
      symbols = {
          {"->", frontend::Operator::Arrow}, {"|=", frontend::Operator::OrEq},
          {"&=", frontend::Operator::AndEq}, {"^=", frontend::Operator::XorEq},
          {"+=", frontend::Operator::AddEq}, {"-=", frontend::Operator::SubEq},
          {"*=", frontend::Operator::MulEq}, {"/=", frontend::Operator::DivEq},
          {"%=", frontend::Operator::ModEq}, {"+", frontend::Operator::Add},
          {"-", frontend::Operator::Sub},    {"*", frontend::Operator::Mul},
          {"/", frontend::Operator::Div},    {"%", frontend::Operator::Mod},
          {"when", frontend::Operator::When}};
  {
    auto iter = symbols.find(tk);
    if (iter != symbols.end()) { binop->op = iter->second; }

    return binop;
  }
}

std::unique_ptr<ast::Node> BuildEnumOrFlagLiteral(
    std::vector<std::unique_ptr<ast::Node>> nodes, bool is_enum, Module *mod,
    error::Log *error_log) {
  std::vector<std::unique_ptr<ast::Expression>> elems;
  if (auto *stmts = nodes[1]->if_as<ast::Statements>()) {
    // TODO if you want these values to depend on compile-time parameters,
    // you'll need to actually build the AST nodes.
    for (auto &stmt : stmts->content_) {
      ASSERT(stmt, InheritsFrom<ast::Expression>());
      elems.push_back(move_as<ast::Expression>(stmt));
    }
  }

  return std::make_unique<ast::EnumLiteral>(
      std::move(elems), TextSpan(nodes[0]->span, nodes[1]->span), is_enum);
}

std::unique_ptr<ast::Node> BuildInterfaceLiteral(
    std::unique_ptr<ast::Statements> stmts, Module *mod,
    error::Log *error_log) {
  auto interface_lit  = std::make_unique<ast::Interface>();
  interface_lit->span = stmts->span;  // TODO it's really bigger than this
                                      // because it involves the keyword too.
  for (auto &stmt : stmts->content_) {
    if (stmt->is<ast::Declaration>()) {
      interface_lit->decls_.push_back(std::move(stmt->as<ast::Declaration>()));
    } else {
      NOT_YET(stmt);
    }
  }
  return interface_lit;
}

std::unique_ptr<ast::Node> BuildScopeLiteral(
    std::unique_ptr<ast::Statements> stmts, TextSpan span, bool stateful,
    Module *mod, error::Log *error_log) {
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

std::unique_ptr<ast::Node> BuildBlock(std::unique_ptr<ast::Statements> stmts,
                                      bool required, Module *mod,
                                      error::Log *error_log) {
  auto block_expr  = std::make_unique<ast::BlockLiteral>(required);
  block_expr->span = stmts->span;  // TODO it's really bigger than this because
                                   // it involves the keyword too.

  for (auto &stmt : stmts->content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      if (decl->id_ == "before") {
        block_expr->before_.push_back(std::move(*decl));
      } else if (decl->id_ == "after") {
        block_expr->after_.push_back(std::move(*decl));
      } else {
        NOT_YET(stmt->to_string(0));
      }
    } else {
      NOT_YET();
    }
  }

  return block_expr;
}

std::unique_ptr<ast::StructLiteral> BuildStructLiteral(ast::Statements &&stmts,
                                                       TextSpan span,
                                                       Module *mod,
                                                       error::Log *error_log) {
  auto struct_lit  = std::make_unique<ast::StructLiteral>();
  struct_lit->mod_ = mod;
  struct_lit->span = std::move(span);
  for (auto &&stmt : std::move(stmts).content_) {
    if (stmt->is<ast::Declaration>()) {
      struct_lit->fields_.push_back(std::move(stmt->as<ast::Declaration>()));
    } else {
      error_log->NonDeclarationInStructDeclaration(stmt->span);
      // TODO show the entire struct declaration and point to the problematic
      // lines.
    }
  }
  return struct_lit;
}

// TODO rename this now that it supports switch statements too.
std::unique_ptr<ast::Node> BuildParameterizedKeywordScope(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  // TODO should probably not do this with a token but some sort of enumerator
  // so we can ensure coverage/safety.
  ASSERT(nodes[0], InheritsFrom<frontend::Token>());
  auto const &tk = nodes[0]->as<frontend::Token>().token;
  if (tk == "switch") {
    auto sw   = BuildSwitch(move_as<ast::Statements>(nodes[4]), mod, error_log);
    sw->expr_ = move_as<ast::Expression>(nodes[2]);
    return sw;
  } else if (tk == "struct") {
    auto result = BuildStructLiteral(
        std::move(nodes[4]->as<ast::Statements>()),
        TextSpan(nodes.front()->span, nodes.back()->span), mod, error_log);
    if (nodes[2]->is<ast::CommaList>()) {
      for (auto &expr : nodes[2]->as<ast::CommaList>().exprs_) {
        ASSERT(expr, InheritsFrom<ast::Declaration>());  // TODO handle failure
        auto decl          = move_as<ast::Declaration>(expr);
        decl->is_fn_param_ = true;
        result->args_.push_back(std::move(*decl));
      }
    } else {
      auto decl          = move_as<ast::Declaration>(nodes[2]);
      decl->is_fn_param_ = true;
      result->args_.push_back(std::move(*decl));
    }
    return result;
  } else {
    UNREACHABLE();
  }
}

std::unique_ptr<ast::Node> BuildConcreteStruct(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  return BuildStructLiteral(std::move(nodes[1]->as<ast::Statements>()),
                            TextSpan(nodes.front()->span, nodes.back()->span),
                            mod, error_log);
}

std::unique_ptr<ast::Node> BuildKWBlock(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  if (nodes[0]->is<frontend::Token>()) {
    std::string const &tk = nodes[0]->as<frontend::Token>().token;

    if (bool is_enum = (tk == "enum"); is_enum || tk == "flags") {
      return BuildEnumOrFlagLiteral(std::move(nodes), is_enum, mod, error_log);

    } else if (tk == "struct") {
      return BuildConcreteStruct(std::move(nodes), mod, error_log);

    } else if (tk == "switch") {
      return BuildSwitch(move_as<ast::Statements>(nodes[1]), mod, error_log);

    } else if (tk == "scope") {
      TextSpan span(nodes.front()->span, nodes.back()->span);
      return BuildScopeLiteral(move_as<ast::Statements>(nodes[1]), span, false,
                               mod, error_log);

    } else if (tk == "scope!") {
      TextSpan span(nodes.front()->span, nodes.back()->span);
      return BuildScopeLiteral(move_as<ast::Statements>(nodes[1]), span, true,
                               mod, error_log);

    } else if (tk == "interface") {
      return BuildInterfaceLiteral(move_as<ast::Statements>(nodes[1]), mod,
                                   error_log);
    } else if (tk == "block") {
      return BuildBlock(move_as<ast::Statements>(nodes[1]), true, mod,
                        error_log);
    } else if (tk == "block?") {
      return BuildBlock(move_as<ast::Statements>(nodes[1]), false, mod,
                        error_log);
    } else {
      UNREACHABLE(tk);
    }
  } else {
    UNREACHABLE(nodes[0].get());
  }
}

std::unique_ptr<ast::Node> Parenthesize(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto result            = move_as<ast::Expression>(nodes[1]);
  result->parenthesized_ = true;
  return result;
}

std::unique_ptr<ast::Node> BuildEmptyParen(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto call  = std::make_unique<ast::Call>();
  call->span = TextSpan(nodes[0]->span, nodes[2]->span);
  call->fn_  = move_as<ast::Expression>(nodes[0]);

  if (call->fn_->is<ast::Declaration>()) {
    error_log->CallingDeclaration(call->fn_->span);
  }
  return call;
}

template <size_t N>
std::unique_ptr<ast::Node> drop_all_but(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  return std::move(nodes[N]);
}

std::unique_ptr<ast::Node> CombineColonEq(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto *tk_node = &nodes[0]->as<frontend::Token>();
  tk_node->token += "=";  // Change : to := and :: to ::=
  tk_node->op = frontend::Operator::ColonEq;
  return drop_all_but<0>(std::move(nodes), mod, error_log);
}

namespace ErrMsg {
template <size_t RTN, size_t RES>
std::unique_ptr<ast::Node> Reserved(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  error_log->Reserved(nodes[RES]->span,
                      nodes[RES]->as<frontend::Token>().token);

  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

template <size_t RTN, size_t RES1, size_t RES2>
std::unique_ptr<ast::Node> BothReserved(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  error_log->Reserved(nodes[RES1]->span,
                      nodes[RES1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[RES2]->span,
                      nodes[RES2]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

std::unique_ptr<ast::Node> NonBinop(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[1]->span, "invalid_node");
}

template <size_t RTN, size_t RES>
std::unique_ptr<ast::Node> NonBinopReserved(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[RES]->span,
                      nodes[RES]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[RTN]->span, "invalid_node");
}

std::unique_ptr<ast::Node> NonBinopBothReserved(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  error_log->Reserved(nodes[0]->span, nodes[0]->as<frontend::Token>().token);
  error_log->NotBinary(nodes[1]->span, nodes[1]->as<frontend::Token>().token);
  error_log->Reserved(nodes[2]->span, nodes[2]->as<frontend::Token>().token);
  return std::make_unique<ast::Identifier>(nodes[1]->span, "invalid_node");
}
}  // namespace ErrMsg

std::unique_ptr<ast::Node> BuildOperatorIdentifier(
    std::vector<std::unique_ptr<ast::Node>> nodes, Module *mod,
    error::Log *error_log) {
  auto span = nodes[1]->span;
  return std::make_unique<ast::Identifier>(
      span, move_as<frontend::Token>(nodes[1])->token);
}

constexpr uint64_t OP_B = op_b | comma | colon | eq;
constexpr uint64_t EXPR = expr | fn_expr | scope_expr | fn_call_expr;
// Used in error productions only!
constexpr uint64_t RESERVED = kw_struct | kw_block_head | op_lt;
constexpr uint64_t KW_BLOCK = kw_struct | kw_block_head | kw_block;
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
    Rule(fn_call_expr, {EXPR, l_paren, EXPR, r_paren}, BuildCall),
    Rule(fn_call_expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(expr, {l_paren, op_l | op_b | eq | op_bl, r_paren},
         BuildOperatorIdentifier),
    Rule(expr, {l_paren, r_paren}, BuildEmptyCommaList),
    Rule(expr, {EXPR, l_bracket, EXPR, r_bracket}, BuildIndexOperator),
    Rule(expr, {l_bracket, r_bracket}, BuildEmptyArray),
    Rule(expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket}, BuildArrayType),
    Rule(expr, {l_bracket, EXPR, semicolon, kw_struct, r_bracket},
         BuildGenericStructType),
    Rule(expr, {l_bracket, EXPR, semicolon, RESERVED, r_bracket},
         ErrMsg::Reserved<0, 3>),
    Rule(expr, {l_bracket, RESERVED, semicolon, EXPR, r_bracket},
         ErrMsg::Reserved<0, 1>),
    Rule(expr, {l_bracket, RESERVED, semicolon, RESERVED, r_bracket},
         ErrMsg::BothReserved<0, 1, 3>),
    Rule(eof, {newline, eof}, drop_all_but<1>),
    Rule(stmts, {stmts, eof}, drop_all_but<0>),
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
    Rule(braced_stmts, {l_brace, op_lt, r_brace}, OneBracedJump),
    Rule(expr, {fn_expr, braced_stmts}, BuildNormalFunctionLiteral),
    Rule(expr, {expr, fn_arrow, braced_stmts}, BuildInferredFunctionLiteral),
    Rule(hashtag, {hashtag, newline}, drop_all_but<0>),
    Rule(expr, {hashtag, expr}, AddHashtag),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case.
    Rule(expr, {EXPR, l_paren, RESERVED, r_paren}, ErrMsg::Reserved<0, 2>),
    Rule(expr, {EXPR, l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<0, 2>),

    Rule(expr, {EXPR, op_r}, BuildRightUnop),
    Rule(expr, {(op_l | op_bl | op_lt), EXPR}, BuildLeftUnop),
    Rule(expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(expr, {l_paren | l_ref, EXPR, r_paren}, Parenthesize),
    Rule(expr, {l_bracket, EXPR, r_bracket}, BuildArrayLiteral),
    Rule(expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),
    Rule(stmts, {stmts, (EXPR | stmts), newline | eof}, BuildMoreStatements),
    Rule(expr, {kw_struct, l_paren, expr, r_paren, braced_stmts},
         BuildParameterizedKeywordScope),
    Rule(expr, {KW_BLOCK, braced_stmts}, BuildKWBlock),
    Rule(expr, {KW_BLOCK, newline}, drop_all_but<0>),

    Rule(expr, {(op_l | op_bl | op_lt), RESERVED}, ErrMsg::Reserved<0, 1>),
    Rule(expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    // TODO does this rule prevent chained scope blocks on new lines or is it
    // preceeded by a shift rule that eats newlines after a right-brace?
    Rule(stmts, {EXPR, (newline | eof)}, BuildOneStatement),
    Rule(expr, {l_paren, EXPR, comma, r_paren}, BuildOneElementCommaList),
    Rule(comma, {comma, newline}, drop_all_but<0>),
    Rule(l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(stmts, {stmts, newline}, drop_all_but<0>),

    Rule(expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(stmts, {op_lt}, BuildJump),
    Rule(block_expr, {expr, braced_stmts}, BuildBlockNode),
    Rule(scope_expr, {fn_call_expr, block_expr}, BuildScopeNode),
    Rule(scope_expr, {scope_expr, block_expr}, ExtendScopeNode),
    Rule(scope_expr, {scope_expr, expr, scope_expr}, SugaredExtendScopeNode),
};

enum class ShiftState : char { NeedMore, EndOfExpr, MustReduce };
struct ParseState {
  ParseState(Src *src, Module *mod)
      : mod_(mod), lex_state_{src, &mod->error_log_} {
  }

  template <size_t N>
  inline Tag get_type() const {
    return tag_stack_[tag_stack_.size() - N];
  }

  template <size_t N>
  inline ast::Node *get() const {
    return node_stack_[node_stack_.size() - N].get();
  }

  ShiftState shift_state() {
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

    if ((get_type<1>() & (kw_block_head | kw_struct)) &&
        ahead.tag_ == newline) {
      return ShiftState::NeedMore;
    }

    if ((get_type<2>() & (kw_block_head | kw_struct)) &&
        get_type<1>() == newline) {
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

    constexpr uint64_t OP = hashtag | op_r | op_l | op_b | colon | eq | comma |
                            op_bl | op_lt | fn_arrow;
    if (get_type<2>() & OP) {
      if (get_type<1>() == r_paren) {
        // TODO this feels like a hack, but maybe this whole function is.
        return ShiftState::MustReduce;
      }
      auto left_prec = precedence(get<2>()->as<Token>().op);
      size_t right_prec;
      if (ahead.tag_ & OP) {
        right_prec = precedence(ahead.node_->as<Token>().op);
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

  void LookAhead() { lookahead_ = NextToken(&lex_state_); }

  const TaggedNode &Next() {
    if (!lookahead_) { LookAhead(); }
    return *lookahead_;
  }

  std::vector<Tag> tag_stack_;
  std::vector<std::unique_ptr<ast::Node>> node_stack_;
  std::optional<TaggedNode> lookahead_;
  Module *mod_ = nullptr;
  LexState lex_state_;

  // We actually don't care about mathing braces because we are only using this
  // to determine for the REPL if we should prompt for further input. If it's
  // wrong, we won't be able to to parse anyway, so it only needs to be the
  // correct value when the braces match.
  int brace_count = 0;
};

// Print out the debug information for the parse stack, and pause.
void Debug(ParseState *ps) {
  // Clear the screen
  fprintf(stderr, "\033[2J\033[1;1H\n");
  for (auto x : ps->tag_stack_) { fprintf(stderr, "%lu, ", x); }
  fprintf(stderr, " -> %lu", ps->Next().tag_);
  fputs("", stderr);

  for (const auto &node_ptr : ps->node_stack_) {
    fputs(node_ptr->to_string(0).c_str(), stderr);
  }
  fgetc(stdin);
}

void Shift(ParseState *ps) {
  if (!ps->lookahead_) { ps->LookAhead(); }
  auto ahead     = *std::move(ps->lookahead_);
  ps->lookahead_ = std::nullopt;
  ps->tag_stack_.push_back(ahead.tag_);
  ps->node_stack_.push_back(std::move(ahead.node_));

  auto tag_ahead = ps->Next().tag_;
  if (tag_ahead & (l_paren | l_bracket | l_brace)) {
    ++ps->brace_count;
  } else if (tag_ahead & (r_paren | r_bracket | r_brace)) {
    --ps->brace_count;
  }
}

bool Reduce(ParseState *ps) {
  const Rule *matched_rule_ptr = nullptr;
  for (const Rule &rule : Rules) {
    if (rule.match(ps->tag_stack_)) {
      matched_rule_ptr = &rule;
      break;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(&ps->node_stack_, &ps->tag_stack_, ps->mod_,
                          ps->lex_state_.error_log_);

  return true;
}

void CleanUpReduction(ParseState *state) {
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
}  // namespace

std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod) {
  auto state = ParseState(src, mod);
  Shift(&state);

  while (state.Next().tag_ != eof) {
    ASSERT(state.tag_stack_.size() == state.node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (state.shift_state() == ShiftState::NeedMore || !Reduce(&state)) {
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
          (braced_stmts | l_paren | r_paren | l_bracket | r_bracket | l_brace |
           r_brace | semicolon | fn_arrow | expr)) {
        lines.push_back(state.node_stack_[i]->span);
      }
    }

    // This is an exceedingly crappy error message.
    mod->error_log_.UnknownParseError(lines);
  }

  return move_as<ast::Statements>(state.node_stack_.back());
}

}  // namespace frontend
