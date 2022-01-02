#include "compiler/common_diagnostics.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "compiler/common.h"
#include "compiler/context.h"

namespace compiler {

diagnostic::DiagnosticMessage UncallableWithArguments::ToMessage() const {
  if (errors.empty()) {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression cannot be called"),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

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
            [&](auto const &key) -> std::string_view { return arguments[key]; },
            err.parameter);

        items.push_back(absl::StrFormat(
            "%s -- Parameter %s cannot accept an argument of type `%s`",
            callable_type->to_string(), param_str, argument));
      } else if constexpr (type == base::meta<call_error::NoParameterNamed>) {
        items.push_back(absl::StrFormat("%s -- No parameter named `%s`.",
                                        callable_type->to_string(), err.name));
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
      diagnostic::Text("Expression cannot be called with the given arguments."),
      diagnostic::SourceQuote(&view.buffer())
          .Highlighted(view.range(), diagnostic::Style::ErrorText()),
      diagnostic::List(std::move(items)));
}

UncallableWithArguments UncallableError(
    Context const &context, ast::Expression const *name,
    absl::Span<ast::Call::Argument const> arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        errors) {
  UncallableWithArguments result;
  result.view = SourceViewFor(name);

  size_t i = 0;
  for (; i < arguments.size(); ++i) {
    if (arguments[i].named()) { break; }
    result.arguments.pos_emplace(
        TypeForDiagnostic(&arguments[i].expr(), context));
  }
  for (; i < arguments.size(); ++i) {
    result.arguments.named_emplace(
        arguments[i].name(),
        TypeForDiagnostic(&arguments[i].expr(), context));
  }

  result.errors = std::move(errors);
  return result;
}

}  // namespace compiler
