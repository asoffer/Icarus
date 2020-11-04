#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/transient_state.h"
#include "compiler/verify/common.h"
#include "type/cast.h"
#include "type/function.h"

#include <utility>

namespace compiler {
namespace {

struct ReturningNonType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-non-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expected a type for the function's return-type but "
                         "found an expression of type `%s`",
                         type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

struct NoReturnTypes {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-return-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to return a value when function returns nothing."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct ReturnTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "return-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Returning an expression of type `%s` from a function which "
            "returns `%s`.",
            actual->to_string(), expected->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type actual;
  type::Type expected;
  frontend::SourceRange range;
};

struct ReturningWrongNumber {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-wrong-number";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to return %u value%s from a function which has %u "
            "return value%s.",
            actual, actual == 1 ? "" : "s", expected, expected == 1 ? "" : "s"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  size_t actual;
  size_t expected;
  frontend::SourceRange range;
};

// `InferReturnTypes` looks at the possible return type of a function only
// through `return` statements. The explicitly specified return type (if it is
// even provided) is entirely ignored. This way, this function can be used both
// to verify that returns match any specified return type, or to infer the
// return type of the function.
absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>>
InferReturnTypes(Compiler &c, ast::FunctionLiteral const *node) {
  // TODO: we can have yields and returns, or yields and jumps, but not jumps
  // and returns. Check this.
  //
  // TODO: In the event of compile-time scope execution, just looking at the
  // syntax tree will be insufficient here.
  absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type>> result;

  // TODO: You shouldn't need to call `ExtractJumps`. They should already have
  // been extracted. Likely the problem is you created some dependent context.
  ExtractJumps(&c.context().extraction_map_, node);

  for (auto const *n : c.context().extraction_map_[node]) {
    auto const *ret_node = n->if_as<ast::ReturnStmt>();
    if (not ret_node) { continue; }
    std::vector<type::Type> ret_types;
    for (auto const *expr : ret_node->exprs()) {
      ret_types.push_back(ASSERT_NOT_NULL(c.context().qual_type(expr))->type());
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
                                        .range    = stmt->range()});
    }
  }
  if (error) { return std::nullopt; }
  // TODO: Also check that the types match, not just the number of types.
  return ret_types.begin()->second;
}

// Verify the body of a function literal, assuming it's parameters have already
// been verified. This is called it two places:
// * From Compiler::VerifyBody which also checks that the return statements
//   match the return types (if specified).
// * From Compiler::VerifyType if the return types are inferred.
std::optional<std::vector<type::Type>> VerifyBodyOnly(
    Compiler &c, ast::FunctionLiteral const *node) {
  bool found_error = false;
  for (auto *stmt : node->stmts()) {
    found_error = (c.VerifyType(stmt) == type::QualType::Error());
  }
  if (found_error) { return std::nullopt; }

  return JoinReturnTypes(c.diag(), InferReturnTypes(c, node));
}

}  // namespace

type::QualType VerifyConcrete(Compiler &c, ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "VerifyConcrete %s", node->DebugString());
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, c.VerifyParams(node->params()));

  if (auto outputs = node->outputs()) {
    std::vector<type::Type> output_type_vec(outputs->size());
    bool error = false;

    // TODO: Output types could depend on each other.
    for (auto *output : *outputs) {
      auto result = c.VerifyType(output);
      if (not result) {
        error = true;
      } else if (result.type() != type::Type_) {
        error = true;
        // TODO: Declarations are given the type of the variable being declared.
        c.diag().Consume(ReturningNonType{
            .range = output->range(),
            .type  = result.type(),
        });
      }
    }

    if (error) { return type::QualType::Error(); }

    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = (*outputs)[i]->if_as<ast::Declaration>()) {
        output_type_vec[i] = ASSERT_NOT_NULL(c.context().qual_type(decl))->type();
      } else if (auto maybe_type =
                     c.EvaluateOrDiagnoseAs<type::Type>((*outputs)[i])) {
        output_type_vec[i] = *maybe_type;
      }
    }

    LOG("compile-work-queue", "Request work fn-lit: %p", node);
    c.Enqueue({.kind     = WorkItem::Kind::VerifyFunctionBody,
               .node     = node,
               .context  = c.context(),
               .consumer = c.diag()});
    return type::QualType::Constant(
        type::Func(std::move(params), std::move(output_type_vec)));
  } else {
    if (auto maybe_return_types = VerifyBodyOnly(c, node)) {
      return type::QualType::Constant(
          type::Func(std::move(params), *std::move(maybe_return_types)));
    } else {
      return type::QualType::Error();
    }
  }
}

type::QualType VerifyGeneric(Compiler &c, ast::FunctionLiteral const *node) {
  auto gen = [node, resources = c.resources(), cg = c.builder().CurrentGroup()](
                 core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    Compiler instantiation_compiler(resources);
    auto [params, rets_ref, context, inserted] =
        instantiation_compiler.Instantiate(node, args);

    if (inserted) {
      LOG("FunctionLiteral", "inserted! %s", node->DebugString());
      auto compiler =
          instantiation_compiler.MakeChild(Compiler::PersistentResources{
              .data                = context,
              .diagnostic_consumer = instantiation_compiler.diag(),
              .importer            = instantiation_compiler.importer(),
          });
      compiler.builder().CurrentGroup() = cg;
      auto qt   = VerifyConcrete(compiler, node);
      auto outs = qt.type()->as<type::Function>().output();
      rets_ref.assign(outs.begin(), outs.end());

      // TODO: Provide a mechanism by which this can fail.
      ASSERT(qt.ok() == true);
      context.set_qual_type(node, qt);
      // TODO: We shouldn't have a queue per compiler. We may not be able to
      // verify these yet.
      compiler.CompleteWorkQueue();
      return &qt.type()->as<type::Function>();
    }

    type::Function const *ft = type::Func(
        params.Transform([](auto const &p) { return p.second; }), rets_ref);
    context.set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return type::QualType::Constant(type::Allocate<type::GenericFunction>(
      node->params().Transform(
          [](auto const &p) { return type::GenericFunction::EmptyStruct{}; }),
      std::move(gen)));
}

type::QualType Compiler::VerifyType(ast::FunctionLiteral const *node) {
  LOG("FunctionLiteral", "Verifying %p: %s", node, node->DebugString());
  ast::OverloadSet os;
  os.insert(node);
  context().SetAllOverloads(node, std::move(os));
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto qt,
                   node->is_generic() ? VerifyGeneric(*this, node)
                                      : VerifyConcrete(*this, node));
  return context().set_qual_type(node, qt);
}

// TODO: Nothing about this has been comprehensively tested. Especially the
// generic bits.
WorkItem::Result Compiler::VerifyBody(ast::FunctionLiteral const *node) {
  // TODO: Move this check out to the ProcessOneItem code?
  if (not context().ShouldVerifyBody(node)) { return WorkItem::Result::Success; }

  LOG("function", "function-literal body verification: %s %p",
      node->DebugString(), &context());
  auto const &fn_type =
      ASSERT_NOT_NULL(context().qual_type(node))->type()->as<type::Function>();
  // TODO: Get the params and check them for completeness, deferring if they're
  // not yet complete.

  auto maybe_return_types = VerifyBodyOnly(*this, node);
  if (not maybe_return_types) { return WorkItem::Result::Failure; }
  if (maybe_return_types->size() != fn_type.output().size()) {
    diag().Consume(ReturningWrongNumber{
        .actual   = maybe_return_types->size(),
        .expected = fn_type.output().size(),
        // TODO: The location specified here is really wide.
        .range = node->range()});
    return WorkItem::Result::Failure;
  }

  bool error = false;
  for (size_t i = 0; i < maybe_return_types->size(); ++i) {
    if (not type::CanCastImplicitly((*maybe_return_types)[i],
                                    fn_type.output()[i])) {
      error = true;
      diag().Consume(ReturnTypeMismatch{.actual   = (*maybe_return_types)[i],
                                        .expected = fn_type.output()[i]});
    }
  }
  return error ? WorkItem::Result::Failure : WorkItem::Result::Success;
}

}  // namespace compiler
