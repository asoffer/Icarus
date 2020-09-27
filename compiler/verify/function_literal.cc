#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/transient_state.h"
#include "compiler/verify/common.h"
#include "diagnostic/errors.h"
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
  type::Type const *type;
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

  type::Type const *actual;
  type::Type const *expected;
  frontend::SourceRange range;
};

struct ReturningWrongNumber {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-wrong-number";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to return %u values from a function which has %u "
            "return values.",
            actual, expected),
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
absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type const *>>
InferReturnTypes(Compiler &c, ast::FunctionLiteral const *node) {
  // TODO: we can have yields and returns, or yields and jumps, but not jumps
  // and returns. Check this.
  //
  // TODO: In the event of compile-time scope execution, just looking at the
  // syntax tree will be insufficient here.
  absl::flat_hash_map<ast::ReturnStmt const *, std::vector<type::Type const *>>
      result;

  // TODO: You shouldn't need to call `ExtractJumps`. They should already have
  // been extracted. Likely the problem is you created some dependent context.
  ExtractJumps(&c.data().extraction_map_, node);

  for (auto const *n : c.data().extraction_map_[node]) {
    auto const *ret_node = n->if_as<ast::ReturnStmt>();
    if (not ret_node) { continue; }
    std::vector<type::Type const *> ret_types;
    for (auto const *expr : ret_node->exprs()) {
      ret_types.push_back(ASSERT_NOT_NULL(c.data().qual_type(expr))->type());
    }

    result.emplace(ret_node, std::move(ret_types));
  }
  return result;
}

std::optional<std::vector<type::Type const *>> JoinReturnTypes(
    diagnostic::DiagnosticConsumer &diag,
    absl::flat_hash_map<ast::ReturnStmt const *,
                        std::vector<type::Type const *>> const &ret_types) {
  LOG("function-literal-join-return-types",
      "Joining types from %u return statements.", ret_types.size());
  if (ret_types.empty()) { return std::vector<type::Type const *>{}; }

  size_t num_returns = ret_types.begin()->second.size();
  bool error = false;
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
std::optional<std::vector<type::Type const *>> VerifyBodyOnly(
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
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto params, c.VerifyParams(node->params()));

  if (auto outputs = node->outputs()) {
    std::vector<type::Type const *> output_type_vec(outputs->size());
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
        output_type_vec[i] = ASSERT_NOT_NULL(c.data().qual_type(decl))->type();
      } else {
        if (auto maybe_type = c.EvaluateAs<type::Type const *>((*outputs)[i])) {
          output_type_vec[i] = *maybe_type;
        } else {
          c.diag().Consume(diagnostic::EvaluationFailure{
              .failure = maybe_type.error(),
              .range   = (*outputs)[i]->range(),
          });
        }
      }
    }

    LOG("compile-work-queue", "Request work fn-lit: %p", node);
    c.Enqueue({.kind     = WorkItem::Kind::VerifyFunctionBody,
               .node     = node,
               .context  = c.data(),
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
  auto ordered_nodes = OrderedDependencyNodes(node);

  auto gen = [node, importer = &c.importer(), compiler_data = &c.data(),
              diag_consumer = &c.diag(),
              ordered_nodes = std::move(ordered_nodes)](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> type::Function const * {
    auto [params, rets, data, inserted] =
        MakeConcrete(node, &compiler_data->module(), ordered_nodes, args,
                     *compiler_data, *diag_consumer);
    if (inserted) {
      Compiler c({
          .builder             = ir::GetBuilder(),
          .data                = data,
          .diagnostic_consumer = *diag_consumer,
          .importer            = *importer,
      });

      if (auto outputs = node->outputs(); outputs and not outputs->empty()) {
        for (auto const *o : *outputs) {
          auto qt = c.VerifyType(o);
          ASSERT(qt == type::QualType::Constant(type::Type_));
          auto maybe_type = c.EvaluateAs<type::Type const *>(o);
          if (not maybe_type) { NOT_YET(); }
          rets.push_back(ASSERT_NOT_NULL(*maybe_type));
        }
      }
    }

    type::Function const *ft = type::Func(
        params.Transform([](auto const &p) { return p.second; }), rets);
    data.set_qual_type(node, type::QualType::Constant(ft));
    return ft;
  };

  return type::QualType::Constant(type::Allocate<type::GenericFunction>(
      node->params().Transform(
          [](auto const &p) { return type::GenericFunction::EmptyStruct{}; }),
      std::move(gen)));
}

type::QualType Compiler::VerifyType(ast::FunctionLiteral const *node) {
  ast::OverloadSet os;
  os.insert(node);
  data().SetAllOverloads(node, std::move(os));
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto qt,
                   node->is_generic() ? VerifyGeneric(*this, node)
                                      : VerifyConcrete(*this, node));
  return data().set_qual_type(node, qt);
}

// TODO: Nothing about this has been comprehensively tested. Especially the
// generic bits.
WorkItem::Result Compiler::VerifyBody(ast::FunctionLiteral const *node) {
  // TODO: Move this check out to the ProcessOneItem code?
  if (not data().ShouldVerifyBody(node)) { return WorkItem::Result::Success; }

  LOG("function", "function-literal body verification: %s %p",
      node->DebugString(), &data());
  auto const &fn_type =
      ASSERT_NOT_NULL(data().qual_type(node))->type()->as<type::Function>();
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
