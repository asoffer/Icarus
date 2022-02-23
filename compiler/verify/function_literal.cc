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
    if (not ret_node) { continue; }
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
std::optional<std::vector<type::Type>> VerifyBodyOnly(
    CompilationDataReference data, ast::FunctionLiteral const *node) {
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

  return JoinReturnTypes(data.diag(), InferReturnTypes(data.context(), node));
}

type::QualType VerifyConcrete(CompilationDataReference data,
                              ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "VerifyConcrete %s", node->DebugString());
  TypeVerifier tv(data);
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, VerifyParameters(tv, node->params()));
  if (auto outputs = node->outputs()) {
    std::vector<type::Type> output_type_vec(outputs->size());
    bool error = false;

    // TODO: Output types could depend on each other.
    for (auto *output : *outputs) {
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

    if (error) { return type::QualType::Error(); }

    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = (*outputs)[i]->if_as<ast::Declaration>()) {
        output_type_vec[i] = data.context().qual_types(decl)[0].type();
      } else if (auto maybe_type =
                     data.EvaluateOrDiagnoseAs<type::Type>((*outputs)[i])) {
        output_type_vec[i] = *maybe_type;
      }
    }

    LOG("FunctionLiteral", "Request work fn-lit: %p, %p", node,
        &data.context());
    data.Enqueue({.kind    = WorkItem::Kind::VerifyFunctionBody,
                  .node    = node,
                  .context = &data.context()});
    return type::QualType::Constant(
        type::Func(std::move(params), std::move(output_type_vec)));
  } else {
    if (auto maybe_return_types = VerifyBodyOnly(data, node)) {
      return type::QualType::Constant(
          type::Func(std::move(params), *std::move(maybe_return_types)));
    } else {
      return type::QualType::Error();
    }
  }
}

}  // namespace

type::QualType VerifyGeneric(CompilationDataReference data,
                             ast::FunctionLiteral const *node) {
  auto gen = [node, comp_data = data.data()](
                 WorkResources const &wr,
                 core::Arguments<type::Typed<ir::CompleteResultRef>> const
                     &args) mutable -> type::Function const * {
    comp_data.work_resources = wr;
    ASSIGN_OR(
        return nullptr,  //
               auto result,
               Instantiate(CompilationDataReference(&comp_data), node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("FunctionLiteral", "inserted! %s into %s", node->DebugString(),
          context.DebugString());
      CompilationData data{.context        = &context,
                           .work_resources = wr,
                           .resources      = comp_data.resources};
      auto qt   = VerifyConcrete(CompilationDataReference(&data), node);
      auto outs = qt.type().as<type::Function>().return_types();
      rets_ref.assign(outs.begin(), outs.end());

      // TODO: Provide a mechanism by which this can fail.
      ASSERT(qt.ok() == true);
      context.set_qual_type(node, qt);
      // TODO: We shouldn't have a queue per compiler. We may not be able to
      // verify these yet.
      return &qt.type().as<type::Function>();
    } else {
      LOG("FunctionLiteral", "cached! %s", node->DebugString());
      type::Function const *ft = type::Func(params, rets_ref);
      context.set_qual_type(node, type::QualType::Constant(ft));
      return ft;
    }
  };

  return type::QualType::Constant(
      type::Allocate<type::Generic<type::Function>>(std::move(gen)));
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "Verifying %p: %s", node, node->DebugString());
  auto qt = node->is_generic() ? VerifyGeneric(*this, node)
                               : VerifyConcrete(*this, node);
  return context().set_qual_type(node, qt);
}

// TODO: Nothing about this has been comprehensively tested. Especially the
// generic bits.
bool BodyVerifier::VerifyBody(ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "function-literal body verification: %s %p",
      node->DebugString(), &context());

  auto const &fn_type =
      context().qual_types(node)[0].type().as<type::Function>();

  for (auto const &param : fn_type.params()) {
    type::Type t = param.value.type();
    if (const auto *s = t.if_as<type::Struct>()) {
      EnsureComplete(
          {.kind = WorkItem::Kind::CompleteStruct,
           .node = ASSERT_NOT_NULL(
               s->defining_module()->as<CompiledModule>().context().AstLiteral(
                   s)),
           .context = &context()});
    }
    if (t.get()->completeness() == type::Completeness::Incomplete) {
      NOT_YET();
    }
  }
  for (type::Type ret : fn_type.return_types()) {
    if (auto const *s = ret.if_as<type::Struct>()) {
      EnsureComplete(
          {.kind = WorkItem::Kind::CompleteStruct,
           .node = ASSERT_NOT_NULL(
               s->defining_module()->as<CompiledModule>().context().AstLiteral(
                   s)),
           .context = const_cast<Context *>(
               &s->defining_module()->as<CompiledModule>().context())});
    } else if (auto const *e = ret.if_as<type::Enum>()) {
      // TODO: This is fraught. It could have been defined in a subcontext and
      // returned/exported.
      EnsureComplete(
          {.kind = WorkItem::Kind::CompleteEnum,
           .node = ASSERT_NOT_NULL(
               e->defining_module()->as<CompiledModule>().context().AstLiteral(
                   e)),
           .context = const_cast<Context *>(
               &e->defining_module()->as<CompiledModule>().context())});

    } else if (auto const *f = ret.if_as<type::Flags>()) {
      EnsureComplete(
          {.kind = WorkItem::Kind::CompleteEnum,
           .node = ASSERT_NOT_NULL(
               f->defining_module()->as<CompiledModule>().context().AstLiteral(
                   f)),
           .context = const_cast<Context *>(
               &f->defining_module()->as<CompiledModule>().context())});
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
  if (maybe_return_types->size() != fn_type.return_types().size()) {
    diag().Consume(ReturningWrongNumber{
        .actual   = maybe_return_types->size(),
        .expected = fn_type.return_types().size(),
        // TODO: The location specified here is really wide.
        .view = node->range()});
    return false;
  }

  bool error = false;
  for (size_t i = 0; i < maybe_return_types->size(); ++i) {
    if (not type::CanCastImplicitly((*maybe_return_types)[i],
                                    fn_type.return_types()[i])) {
      error = true;
      diag().Consume(ReturnTypeMismatch{
          .actual =
              (*maybe_return_types)[i].to_string(),  // TODO: Improve this.
          .expected =
              fn_type.return_types()[i].to_string(),  // TODO: Improve this.
      });
    }
  }

  return not error;
}

}  // namespace compiler
