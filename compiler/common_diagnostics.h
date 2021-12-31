#ifndef ICARUS_COMPILER_COMMON_DIAGNOSTICS_H
#define ICARUS_COMPILER_COMMON_DIAGNOSTICS_H

#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/context.h"
#include "compiler/type_for_diagnostic.h"
#include "core/call.h"
#include "diagnostic/message.h"
#include "frontend/source/buffer.h"
#include "frontend/source/view.h"
#include "type/callable.h"
#include "type/type.h"

namespace compiler {

struct NotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "not-a-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression was expected to be a type, but instead "
                         "was a value of type `%s`.",
                         type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  type::Type type;
};

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No viable cast from `%s` to `%s`.", from, to),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string from;
  std::string to;
  frontend::SourceView view;
};

struct AssigningToConstant {
  // TODO I'm not sure this shouldn't be in the type-error category.
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-constant";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot assign to a constant (of type `%s`).", to),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type to;
  frontend::SourceView view;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to move an immovable type `%s`.", from),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type from;
  frontend::SourceView view;
};

struct PatternTypeMismatch {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "pattern-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            R"(Mismatched type between pattern and expression being matched.
  Type from pattern:          %s
  Type being matched against: %s)",
            pattern_type, matched_type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type pattern_type;
  std::string matched_type;
  frontend::SourceView view;
};

struct UncallableWithArguments {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncallable-with-arguments";
  diagnostic::DiagnosticMessage ToMessage() const {
    std::vector<std::string> items;
    items.reserve(errors.size());
    for (auto const &type_and_reason : errors) {
      type_and_reason.second.Visit([&](auto const &err) {
        using call_error                    = core::CallabilityResult;
        auto const &[callable_type, reason] = type_and_reason;
        static constexpr auto type = base::meta<std::decay_t<decltype(err)>>;
        if constexpr (type == base::meta<call_error::TooManyArguments>) {
          items.push_back(absl::StrFormat(
              "%s -- Has %d parameters, but %d arguments provided.",
              callable_type->to_string(), err.max_num_accepted,
              err.num_provided));
        } else if constexpr (type ==
                             base::meta<
                                 call_error::MissingNonDefaultableArguments>) {
          std::vector<std::string> names(err.names.begin(), err.names.end());
          std::sort(names.begin(), names.end());
          items.push_back(
              absl::StrCat(callable_type->to_string(),
                           " -- The following parameters do not have default "
                           "arguments and are not provided at the call-site:",
                           absl::StrJoin(names, ", ")));
        } else if constexpr (type == base::meta<call_error::TypeMismatch>) {
          std::string param_str;
          if (auto const *param_as_str =
                  std::get_if<std::string>(&err.parameter)) {
            param_str = absl::StrCat("named `", *param_as_str, "`");
          } else {
            param_str =
                absl::StrCat("at index ", std::get<size_t>(err.parameter));
          }

          std::string_view argument = std::visit(
              [&](auto const &key) -> std::string_view {
                return arguments[key];
              },
              err.parameter);

          items.push_back(absl::StrFormat(
              "%s -- Parameter %s cannot accept an argument of type `%s`",
              callable_type->to_string(), param_str, argument));
        } else if constexpr (type == base::meta<call_error::NoParameterNamed>) {
          items.push_back(absl::StrFormat("%s -- No parameter named `%s`.",
                                          callable_type->to_string(),
                                          err.name));
        } else if constexpr (type ==
                             base::meta<call_error::PositionalArgumentNamed>) {
          items.push_back(absl::StrFormat(
              "%s -- Named argument `%s` bound to the same parameter as "
              "the argument at index %d.",
              callable_type->to_string(), err.name, err.index));
        } else {
          // TODO: Determine how deeply to dig into this error message.
          items.push_back(
              absl::StrCat(callable_type->to_string(), " -- ", "TODO"));
        }
      });
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Expression cannot be called with the given arguments."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()),
        diagnostic::List(std::move(items)));
  }

  core::Arguments<std::string> arguments;
  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  frontend::SourceView view;
};

UncallableWithArguments UncallableError(
    Context const &context, ast::Expression const *name,
    absl::Span<ast::Call::Argument const> arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        errors);

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMMON_DIAGNOSTICS_H
