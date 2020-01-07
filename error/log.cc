#include "error/log.h"

#include "absl/strings/str_cat.h"
#include "ast/ast.h"
#include "diagnostic/message.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"

namespace error {
void Log::UndeclaredIdentifier(ast::Identifier const *id) {
  undeclared_ids_[std::string{id->token()}].push_back(id);
}

void Log::PostconditionNeedsBool(frontend::SourceRange const &range,
                                 std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Function postcondition must be of type bool, but you "
                       "provided an expression of type `%s`."),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::PreconditionNeedsBool(frontend::SourceRange const &range,
                                std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Function precondition must be of type bool, but you provided an "
          "expression of type %s.",
          type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

#define MAKE_LOG_ERROR(fn_name, msg)                                           \
  void Log::fn_name(frontend::SourceRange const &range) {                      \
    renderer_.AddError(diagnostic::DiagnosticMessage(                          \
        diagnostic::Text(msg), diagnostic::SourceQuote(src_).Highlighted(      \
                                   range, diagnostic::Style{})));              \
  }
#include "error/errors.xmacro.h"
#undef MAKE_LOG_ERROR

void Log::StatementsFollowingJump(frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Statements cannot follow a `return` or `yield` statement."),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::RunawayMultilineComment() {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Finished reading file during multi-line comment.")));
}

void Log::DoubleDeclAssignment(frontend::SourceRange const &decl_range,
                               frontend::SourceRange const &val_range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Attempting to initialize an identifier that already has an initial "
          "value. Did you mean `==` instead of `=`?"),
      diagnostic::SourceQuote(src_)
          .Highlighted(decl_range, diagnostic::Style{})
          .Highlighted(val_range, diagnostic::Style{})));
}

void Log::DeclarationUsedInUnop(std::string const &unop,
                                frontend::SourceRange const &decl_range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Declarations cannot be used as argument to unary operator `%s`.",
          unop),
      diagnostic::SourceQuote(src_).Highlighted(decl_range,
                                                diagnostic::Style{})));
}

void Log::MissingMember(frontend::SourceRange const &range,
                        std::string_view member_name, std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Expressions of type `%s` have no member named `%s`.",
                       type, member_name),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NonExportedMember(frontend::SourceRange const &range,
                            std::string_view member_name,
                            std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Expressions of type `%s` do not export the member `%s`.", type,
          member_name),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::ReturnTypeMismatch(std::string_view expected_type,
                             std::string_view actual_type,
                             frontend::SourceRange const &range) {
  // TODO also show where the return type is specified?
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Returning an expression of type `%s` from a function "
                       "which returns `%s`.",
                       actual_type, expected_type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NoReturnTypes(ast::ReturnStmt const *ret_expr) {
  // TODO allow "return foo(...)" when foo: ??? -> ().
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Attempting to return a value when function returns nothing."),
      diagnostic::SourceQuote(src_).Highlighted(ret_expr->span,
                                                diagnostic::Style{})));
}

void Log::ReturningWrongNumber(frontend::SourceRange const &range,
                               size_t actual, size_t expected) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Attempting to return %u values from a function which "
                       "has %u return values."),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::IndexedReturnTypeMismatch(std::string_view expected_type,
                                    std::string_view actual_type,
                                    frontend::SourceRange const &range,
                                    size_t index) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Returning an expression in slot #%u (zero-indexed) of "
                       "type `%s` but function expects a value of type `%s` in "
                       "that slot.",
                       index, actual_type, expected_type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::DereferencingNonPointer(std::string_view type,
                                  frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Attempting to dereference an object of type `%s` which "
                       "is not a pointer",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::WhichNonVariant(std::string_view type,
                          frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Attempting to call `which` an object of type `%s` "
                       "which is not a variant.",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::Reserved(frontend::SourceRange const &range,
                   std::string const &token) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Identifier `%s` is a reserved keyword.", token),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NotBinary(frontend::SourceRange const &range,
                    std::string const &token) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Operator `%s` is not a binary operator.", token),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NotAType(frontend::SourceRange const &range, std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Expression was expected to be a type, but instead was of type `%s`.",
          type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::PositionalArgumentFollowingNamed(
    std::vector<frontend::SourceRange> const &pos_ranges,
    frontend::SourceRange const &named_range) {
  diagnostic::SourceQuote quote(src_);
  quote.Highlighted(named_range, diagnostic::Style{});
  for (auto const &pos_range : pos_ranges) {
    quote.Highlighted(pos_range, diagnostic::Style{});
  }

  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Positional function arguments cannot follow a named argument."),
      quote));
}

void Log::UnknownParseError(std::vector<frontend::SourceRange> const &lines) {
  // TODO there's something seriously wrong with this
  // TODO source name?
  diagnostic::SourceQuote quote(src_);
  for (const auto &range : lines) {
    quote.Highlighted(range, diagnostic::Style{});
  }

  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Parse errors found in \"<SOME FILE>\" on the following lines:"),
      quote));
}

void Log::CyclicDependency(std::vector<ast::Identifier const *> cyc_deps) {
  cyc_dep_vecs_.push_back(std::move(cyc_deps));
}

void Log::ShadowingDeclaration(frontend::SourceRange const &span1,
                               frontend::SourceRange const &span2) {
  renderer_.AddError(
      diagnostic::DiagnosticMessage(diagnostic::Text("Ambiguous declarations:"),
                                    diagnostic::SourceQuote(src_)
                                        .Line(span1.begin().line_num)
                                        .Line(span2.begin().line_num)));
}

void Log::Dump() {
  for (auto &cycle : cyc_dep_vecs_) {
    // TODO make cyc_dep_vec just identifiers
    std::cerr << "Found a cyclic dependency:\n\n";

    absl::flat_hash_map<ast::Declaration const *, size_t> decls;
    for (auto const *id : cycle) { decls.emplace(id->decl(), decls.size()); }

    diagnostic::SourceQuote quote(src_);
    for (const auto *id : cycle) {
      quote.Highlighted(id->decl()->span, diagnostic::Style{})
          .Highlighted(id->span, diagnostic::Style{});
    }

    renderer_.AddError(diagnostic::DiagnosticMessage(
        diagnostic::Text("Found a cyclic dependency:"), quote));
  }

  for (auto const &[decl, ids] : out_of_order_decls_) {
    diagnostic::SourceQuote quote(src_);
    for (auto const *id : ids) {
      quote.Highlighted(id->span, diagnostic::Style{});
    }

    renderer_.AddError(diagnostic::DiagnosticMessage(
        diagnostic::Text("Variable `%s` is used before it is defined (which is "
                         "only allowed for constants).",
                         decl->id()),
        quote));
  }

  for (const auto &[token, ids] : undeclared_ids_) {
    diagnostic::SourceQuote quote(src_);
    for (auto const *id : ids) {
      quote.Highlighted(id->span, diagnostic::Style{});
    }

    renderer_.AddError(diagnostic::DiagnosticMessage(
        diagnostic::Text("Use of undeclared identifier `%s`:", token), quote));
  }
}

void Log::DeclOutOfOrder(ast::Declaration const *decl,
                         ast::Identifier const *id) {
  out_of_order_decls_[decl].push_back(id);
}

void Log::InvalidIndexing(frontend::SourceRange const &range,
                          std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Cannot index into a non-array, non-buffer type. "
                       "Indexed type is a `%s`.",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::InvalidIndexType(frontend::SourceRange const &range,
                           std::string_view type, std::string_view index_type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "Attempting to index a value of type `%s` with a non-integral index. "
          "Indices must be integers, but you provided an index of type `%s`.",
          type, index_type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::TypeMustBeInitialized(frontend::SourceRange const &range,
                                std::string_view type) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("There is no default value for the type `%s`.", type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::ComparingIncomparables(std::string_view lhs, std::string_view rhs,
                                 frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Values of type `%s` and `%s` are being compared but no "
                       "such comparison is allowed:",
                       lhs, rhs),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::InvalidNumber(frontend::SourceRange const &range,
                        std::string_view err) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("%s.", err),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NoCallMatch(frontend::SourceRange const &range,
                      std::vector<std::string> generic_failure_reasons,
                      absl::flat_hash_map<ast::Expression const *,
                                          std::string> const &failure_reasons) {
  diagnostic::SourceQuote quote(src_);
  std::vector<std::string> reasons = std::move(generic_failure_reasons);
  for (auto const &[expr, reason] : failure_reasons) {
    quote.Highlighted(expr->span, diagnostic::Style{});
    reasons.push_back(reason);
  }

  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Failed to find a matching function signature to call."),
      diagnostic::List(reasons),
      quote.Highlighted(range, diagnostic::Style{})));
}

void Log::MissingDispatchContingency(
    frontend::SourceRange const &range,
    std::vector<core::FnArgs<std::string>> const &missing_dispatch) {
  std::vector<std::string> reasons;
  reasons.reserve(missing_dispatch.size());
  for (core::FnArgs<std::string> const &fnargs : missing_dispatch) {
    reasons.push_back(absl::StrCat("No function taking arguments (",
                                   fnargs.to_string(), ")"));
  }

  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Failed to find a valid function to call for all "
                       "required dispatches."),
      diagnostic::List(std::move(reasons)),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::NotCopyable(frontend::SourceRange const &range,
                      std::string_view from) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Attempting to copy an uncopyable type `%s`.", from),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::IndexingTupleOutOfBounds(frontend::SourceRange const &range,
                                   std::string_view tup, size_t tup_size,
                                   size_t index) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Tuple is indexed out of bounds. Tuple of type `%s` has "
                       "size %u but you are attempting to access position %u.",
                       tup, tup_size, index),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::MissingModule(std::string_view src, std::string_view requestor) {
  renderer_.AddError(diagnostic::DiagnosticMessage(diagnostic::Text(
      "Could not find module named \"%s\" requested from %s", src,
      requestor.empty() ? "command line"
                        : absl::StrCat("\"", requestor, "\"."))));
}

void Log::UninferrableType(InferenceFailureReason reason,
                           frontend::SourceRange const &range) {
  char const *text = nullptr;
  switch (reason) {
    case InferenceFailureReason::Inferrable: UNREACHABLE();
    case InferenceFailureReason::EmptyArray:
      text =
          "Unable to infer the type of the following expression because the "
          "type of an empty array cannot be inferred. Either specify the "
          "type explicitly, or cast it to a specific array type:";
      break;
    case InferenceFailureReason::NullPtr:
      text =
          "Unable to infer the type of the following expression because the "
          "type of `null` cannot be inferred. Either specify the type "
          "explicitly, or cast it to a specific pointer type:";
      break;
    case InferenceFailureReason::Hole:
      text = "Unable to infer the type of a value that is uninitalized:";
      break;
  }

  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(text),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::PrintMustReturnVoid(std::string_view type,
                              frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(
          "`print` must return void, but evaluates to an object of type `%s`.",
          type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::SwitchConditionNeedsBool(std::string_view type,
                                   frontend::SourceRange const &range) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text("Expressionless switch conditions must evaluate to a "
                       "`bool`, but you provided a `%s`.",
                       type),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

void Log::BuiltinError(frontend::SourceRange const &range, std::string text) {
  renderer_.AddError(diagnostic::DiagnosticMessage(
      diagnostic::Text(std::move(text)),
      diagnostic::SourceQuote(src_).Highlighted(range, diagnostic::Style{})));
}

}  // namespace error
