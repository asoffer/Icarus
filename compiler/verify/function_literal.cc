#include <utility>

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/context.h"
#include "compiler/instantiate.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/transient_state.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/cast.h"
#include "type/function.h"

namespace compiler {
namespace {

struct ReturningNonType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-non-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expected a type for the function's return-type but "
                         "found an expression of type `%s`",
                         type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

struct NoReturnTypes {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-return-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to return a value when function returns nothing."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct ReturnTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "return-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Returning an expression of type `%s` from a function which "
        "returns `%s`.",
        actual, expected));
  }

  std::string actual;
  std::string expected;
};

struct ReturningWrongNumber {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-wrong-number";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to return %u value%s from a function which has %u "
            "return value%s.",
            actual, actual == 1 ? "" : "s", expected, expected == 1 ? "" : "s"),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  size_t actual;
  size_t expected;
  std::string_view view;
};

// `InferReturnTypes` looks at the possible return type of a function only
// through `return` statements. The explicitly specified return type (if it is
// even provided) is entirely ignored. This way, this function can be used both
// to verify that returns match any specified return type, or to infer the
// return type of the function.
absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>>
InferReturnTypes(Context const &c, ast::FunctionLiteral const *node) {
  // TODO: we can have yields and returns, or yields and jumps, but not jumps
  // and returns. Check this.
  //
  // TODO: In the event of compile-time scope execution, just looking at the
  // syntax tree will be insufficient here.
  absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>> result;

  for (ast::ReturnStmt const *ret_node : node->returns()) {
    std::vector<type::Type> ret_types;
    for (auto const *expr : ret_node->exprs()) {
      ret_types.push_back(c.qual_types(expr)[0].type());
    }

    result.emplace(ret_node, std::move(ret_types));
  }
  return result;
}

std::optional<std::vector<type::Type>> JoinReturnTypes(
    diagnostic::DiagnosticConsumer &diag,
    absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>> const
        &ret_types) {
  LOG("function-literal-join-return-types",
      "Joining types from %u return statements.", ret_types.size());
  if (ret_types.empty()) { return std::vector<type::Type>{}; }

  size_t num_returns = ret_types.begin()->second.size();
  bool error         = false;
  for (auto const &[stmt, types] : ret_types) {
    // TODO: This error message is non-deterministic and also not even always
    // useful. Sometimes it might catch the one place the user left off a return
    // and that that was the expected number of returns. We should do some more
    // processing here to generate good error messages.
    //
    // In the event that this isn't inferred we would want to use the specified
    // value as the source of truth.
    //
    // Also, in the presence of default return values or named return values,
    // this won't work.
    if (num_returns != types.size()) {
      error = true;
      diag.Consume(ReturningWrongNumber{.actual   = types.size(),
                                        .expected = num_returns,
                                        .view     = stmt->range()});
    }
  }
  if (error) { return std::nullopt; }
  // TODO: Also check that the types match, not just the number of types.
  return ret_types.begin()->second;
}

// Verify the body of a function literal, assuming it's parameters have already
// been verified. This is called it two places:
// * From VerifyBody which also checks that the return statements
//   match the return types (if specified).
// * From VerifyType if the return types are inferred.
std::optional<
    absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>>>
VerifyBodyOnly(CompilationDataReference data,
               ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "VerifyBodyOnly for %s on %s", node->DebugString(),
      data.context().DebugString());
  bool found_error = false;
  for (auto const *stmt : node->stmts()) {
    absl::Span<type::QualType const> qts = VerifyType(data, stmt);
    bool current_was_error = (qts.size() == 1 and not qts[0].ok());
    if (current_was_error) {
      found_error = true;
      LOG("FunctionLiteral", "Found an error in %s", node->DebugString());
    }
  }
  if (found_error) { return std::nullopt; }

  return InferReturnTypes(data.context(), node);
}

std::optional<std::vector<type::Type>> VerifyReturnTypes(
    CompilationDataReference data,
    base::PtrSpan<ast::Expression const> outputs) {
  std::vector<type::Type> return_types;
  return_types.reserve(outputs.size());
  bool error = false;

  // TODO: Output types could depend on each other.
  for (auto *output : outputs) {
    auto result = VerifyType(data, output)[0];
    if (not result) {
      error = true;
    } else if (result.type() != type::Type_) {
      error = true;
      // TODO: Declarations are given the type of the variable being declared.
      data.diag().Consume(ReturningNonType{
          .view = output->range(),
          .type = TypeForDiagnostic(output, data.context()),
      });
    }
  }

  if (error) { return std::nullopt; }

  for (auto const *output : outputs) {
    if (auto *decl = output->if_as<ast::Declaration>()) {
      return_types.push_back(data.context().qual_types(decl)[0].type());
    } else if (auto maybe_type =
                   data.EvaluateOrDiagnoseAs<type::Type>(output)) {
      return_types.push_back(*maybe_type);
    }
  }
  return return_types;
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "Verifying %p: %s", node, node->DebugString());

  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   auto params, VerifyParameters(*this, node->parameters()));

  if (auto outputs = node->outputs()) {
    ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                     auto return_types, VerifyReturnTypes(*this, *outputs));

    LOG("FunctionLiteral", "Request work fn-lit: %p, %p", node, &context());
    Enqueue({.kind    = WorkItem::Kind::VerifyFunctionBody,
             .node    = node,
             .context = &context()});
    return context().set_qual_type(
        node, type::QualType::Constant(
                  type::Func(std::move(params), std::move(return_types))));
  } else {
    if (auto maybe_return_types = VerifyBodyOnly(*this, node)) {
      auto joined_return_types = JoinReturnTypes(diag(), *maybe_return_types);
      return context().set_qual_type(
          node, type::QualType::Constant(type::Func(
                    std::move(params), *std::move(joined_return_types))));
    } else {
      return context().set_qual_type(node, type::QualType::Error());
    }
  }
}

// TODO: Nothing about this has been comprehensively tested. Especially the
// generic bits.
bool BodyVerifier::VerifyBody(ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "function-literal body verification: %s %p",
      node->DebugString(), &context());

  auto const &fn_type =
      context().qual_types(node)[0].type().as<type::Function>();

  auto &module_table = resources().shared_context->module_table();

  for (auto const &param : fn_type.parameters()) {
    type::Type t = param.value.type();
    if (const auto *s = t.if_as<type::Struct>()) {
      EnsureComplete(
          {.kind    = WorkItem::Kind::CompleteStruct,
           .node    = ASSERT_NOT_NULL(module_table.module(s->defining_module())
                                       ->as<CompiledModule>()
                                       .context()
                                       .AstLiteral(s)),
           .context = &context()});
    }
    if (t.get()->completeness() == type::Completeness::Incomplete) {
      NOT_YET();
    }
  }
  for (type::Type ret : fn_type.return_types()) {
    if (auto const *s = ret.if_as<type::Struct>()) {
      auto const &ctx = module_table.module(s->defining_module())
                            ->as<CompiledModule>()
                            .context();
      EnsureComplete({.kind    = WorkItem::Kind::CompleteStruct,
                      .node    = ASSERT_NOT_NULL(ctx.AstLiteral(s)),
                      .context = const_cast<Context *>(&ctx)});
    } else if (auto const *e = ret.if_as<type::Enum>()) {
      auto const &ctx = resources()
                            .shared_context->module_table()
                            .module(e->defining_module())
                            ->as<CompiledModule>()
                            .context();
      EnsureComplete({.kind    = WorkItem::Kind::CompleteEnum,
                      .node    = ASSERT_NOT_NULL(ctx.AstLiteral(e)),
                      .context = const_cast<Context *>(&ctx)});
    } else if (auto const *f = ret.if_as<type::Flags>()) {
      // TODO: Ensure the flags are complete.
    }
    if (ret.get()->completeness() == type::Completeness::Incomplete) {
      NOT_YET(ret);
    }
  }

  auto maybe_return_types = VerifyBodyOnly(*this, node);
  if (not maybe_return_types) {
    LOG("FunctionLiteral", "Body verification was a failure.");
    return false;
  }
  auto joined_return_types = JoinReturnTypes(diag(), *maybe_return_types);
  if (not joined_return_types) {
    LOG("FunctionLiteral", "Body verification was a failure.");
    return false;
  }

  if (joined_return_types->size() != fn_type.return_types().size()) {
    diag().Consume(ReturningWrongNumber{
        .actual   = joined_return_types->size(),
        .expected = fn_type.return_types().size(),
        // TODO: The location specified here is really wide.
        .view = node->range()});
    return false;
  }

  if (maybe_return_types->empty()) { return true; }

  bool error = false;
  for (auto const &[return_stmt, return_types] : *maybe_return_types) {
    for (size_t i = 0; i < return_types.size(); ++i) {
      if (not type::CanCastImplicitly(return_types[i],
                                      fn_type.return_types()[i])) {
        error = true;

        // It is impossible that there are no outputs: If that were the case
        // either the types would match because that's what they were deduced
        // from, or not all the returns of the function had matching types and
        // this would have been diagnosed earlier.
        ASSERT(node->outputs().has_value() == true);
        auto outputs = *node->outputs();

        // TODO: Expressions can be expanded and may not match 1-1 with types.
        diag().Consume(ReturnTypeMismatch{
            .actual   = TypeForDiagnostic(return_stmt->exprs()[i], context()),
            .expected = ExpressionForDiagnostic(outputs[i], context()),
        });
      }
    }
  }

  return not error;
}

}  // namespace compiler
