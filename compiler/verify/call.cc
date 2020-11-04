#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/callable.h"
#include "type/generic_struct.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct BuiltinError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "builtin-error";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }
  frontend::SourceRange range;
  std::string message;
};

struct UncallableExpression {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncallable-expression";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Uncallable expression"),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

struct UncallableWithArguments {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncallable-with-arguments";
  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    std::vector<std::string> items;
    items.reserve(error.reasons.size());
    for (auto const &type_and_reason : error.reasons) {
      std::visit(
          [&](auto const &err) {
            using call_error                    = Compiler::CallError;
            auto const &[callable_type, reason] = type_and_reason;
            static constexpr auto type =
                base::meta<std::decay_t<decltype(err)>>;
            if constexpr (type == base::meta<call_error::TooManyArguments>) {
              items.push_back(absl::StrFormat(
                  "%s -- Has %d parameters, but %d arguments provided.",
                  callable_type->to_string(), err.max_num_accepted,
                  err.num_provided));
            } else if constexpr (
                type ==
                base::meta<call_error::MissingNonDefaultableArguments>) {
              std::vector<std::string> names(err.names.begin(),
                                             err.names.end());
              std::sort(names.begin(), names.end());
              items.push_back(absl::StrCat(
                  callable_type->to_string(),
                  " -- The following parameters do not have default arguments "
                  "and are not provided at the call-site: ",
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

              items.push_back(absl::StrFormat(
                  "%s -- Parameter %s cannot accept an argument of type `%s`",
                  callable_type->to_string(), param_str,
                  err.argument_type->to_string()));
            } else if constexpr (type ==
                                 base::meta<call_error::NoParameterNamed>) {
              items.push_back(absl::StrFormat("%s -- No parameter named `%s`.",
                                              callable_type->to_string(),
                                              err.name));
            } else if constexpr (type ==
                                 base::meta<
                                     call_error::PositionalArgumentNamed>) {
              items.push_back(absl::StrFormat(
                  "%s -- Named argument `%s` bound to the same parameter as "
                  "the argument at index %d.",
                  callable_type->to_string(), err.name, err.index));
            } else {
              // TODO: Determine how deeply to dig into this error message.
              items.push_back(
                  absl::StrCat(callable_type->to_string(), " -- ", "TODO"));
            }
          },
          type_and_reason.second);
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Expression cannot be called with the given arguments."),
        diagnostic::List(std::move(items)),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  Compiler::CallError error;
  frontend::SourceRange range;
};

type::QualType VerifyForeignCall(
    Compiler *c, frontend::SourceRange const &range,
    core::Arguments<type::Typed<ir::Value>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = "Built-in function `foreign` cannot be called with named "
                   "arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = absl::StrCat("Built-in function `foreign` takes exactly two "
                                "arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (arg_vals[0].type() != type::ByteView) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = absl::StrCat("First argument to `foreign` must be a "
                                "byte-view (You provided a(n) ",
                                arg_vals[0].type()->to_string(), ")."),
    });
    error = true;
  }

  if (arg_vals[0]->empty()) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = "First argument to `foreign` must be a constant."});
    error = true;
  }

  if (arg_vals[1].type() != type::Type_) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = absl::StrCat(
            "Second argument to `foreign` must be a type (You provided a(n) ",
            arg_vals[0].type()->to_string(), ").")});
    error = true;
  }

  if (arg_vals[1]->empty()) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = "Second argument to `foreign` must be a constant."});
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  auto const *foreign_type = arg_vals[1]->get_if<type::Type>();
  if (not foreign_type or not((*foreign_type)->is<type::Function>() or
                              (*foreign_type)->is<type::Pointer>())) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = "Builtin `foreign` may only be called when the second "
                   "argument is a pointer or a function type.",
    });
    return type::QualType::Error();
  }
  return type::QualType::NonConstant(*foreign_type);
}

type::QualType VerifyOpaqueCall(
    Compiler *c, frontend::SourceRange const &range,
    core::Arguments<type::Typed<ir::Value>> const &arg_vals) {
  type::QualType qt =
      type::QualType::Constant(ir::BuiltinFn::Opaque().type()->output()[0]);
  if (not arg_vals.empty()) {
    c->diag().Consume(BuiltinError{
        .range   = range,
        .message = "Built-in function `opaque` takes no arguments."});
    qt.MarkError();
  }
  return qt;
}

type::QualType VerifyBytesCall(
    Compiler *c, frontend::SourceRange const &range,
    core::Arguments<type::Typed<ir::Value>> const &arg_vals) {
  auto qt =
      type::QualType::Constant(ir::BuiltinFn::Bytes().type()->output()[0]);

  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{.range = range,
                                   .message =
                                       "Built-in function `bytes` cannot be "
                                       "called with named arguments."});
    qt.MarkError();
  } else {
    if (size_t size = arg_vals.size(); size != 1u) {
      c->diag().Consume(BuiltinError{
          .range   = range,
          .message = absl::StrCat("Built-in function `bytes` takes exactly "
                                  "one argument (You provided ",
                                  size, ")."),
      });

      qt.MarkError();
    } else if (arg_vals[0].type() != type::Type_) {
      c->diag().Consume(BuiltinError{
          .range = range,
          .message =
              absl::StrCat("Built-in function `bytes` must take a single "
                           "argument of type `type` (You provided a(n) ",
                           arg_vals[0].type()->to_string(), ").")});
      qt.MarkError();
    }
  }

  return qt;
}

type::QualType VerifyAlignmentCall(
    Compiler *c, frontend::SourceRange const &range,
    core::Arguments<type::Typed<ir::Value>> const &arg_vals) {
  auto qt =
      type::QualType::Constant(ir::BuiltinFn::Alignment().type()->output()[0]);

  if (not arg_vals.named().empty()) {
    c->diag().Consume(
        BuiltinError{.range   = range,
                     .message = "Built-in function `alignment` cannot be "
                                "called with named arguments."});
    qt.MarkError();
  } else {
    if (size_t size = arg_vals.size(); size != 1u) {
      c->diag().Consume(BuiltinError{
          .range   = range,
          .message = absl::StrCat("Built-in function `alignment` takes exactly "
                                  "one argument (You provided ",
                                  size, ")."),
      });

      qt.MarkError();
    } else if (arg_vals[0].type() != type::Type_) {
      c->diag().Consume(BuiltinError{
          .range = range,
          .message =
              absl::StrCat("Built-in function `alignment` must take a single "
                           "argument of type `type` (You provided a(n) ",
                           arg_vals[0].type()->to_string(), ").")});
      qt.MarkError();
    }
  }

  return qt;
}

}  // namespace

type::QualType Compiler::VerifyType(ast::Call const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto arg_vals, VerifyArguments(node->args()));
  // TODO: consider having `foreign` be a generic type. This would allow for the
  // possibility of overlading builtins. That's a dangerous yet principled idea.
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    type::QualType qt;
    switch (b->value().which()) {
      case ir::BuiltinFn::Which::Foreign: {
        qt = VerifyForeignCall(this, b->range(), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Opaque: {
        qt = VerifyOpaqueCall(this, b->range(), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Bytes: {
        qt = VerifyBytesCall(this, b->range(), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Alignment: {
        qt = VerifyAlignmentCall(this, b->range(), arg_vals);
      } break;
      case ir::BuiltinFn::Which::DebugIr: {
        // This is for debugging the compiler only, so there's no need to write
        // decent errors here.
        ASSERT(arg_vals.size() == 0u);
        qt = type::QualType::Constant(type::Void());
      }
    }
    if (qt) { context().set_qual_type(node, qt); }
    return qt;
  }

  auto [callee_qt, overload_map] = VerifyCallee(node->callee(), arg_vals);
  if (not callee_qt.ok()) { return type::QualType::Error(); }

  if (auto const *c = callee_qt.type()->if_as<type::Callable>()) {
    LOG("Call.VerifyType", "Callee's (%s) qual-type: %s",
        node->callee()->DebugString(), callee_qt);
    auto result = VerifyCall(node, overload_map, arg_vals);
    if (not result) {
      diag().Consume(UncallableWithArguments{
          .error = std::move(result).error(),
          .range = node->callee()->range(),
      });
      return type::QualType::Error();
    }
    // TODO: under what circumstances can we prove that the implementation
    // doesn't need to be run at runtime?
    return context().set_qual_type(node, *result);
  } else if (auto const *gen_struct =
                 callee_qt.type()->if_as<type::GenericStruct>()) {
    // TODO: Not always a constant
    // TODO: Isn't this also callable?
    return context().set_qual_type(node, type::QualType::Constant(type::Type_));
  } else {
    diag().Consume(UncallableExpression{.range = node->callee()->range()});
    return type::QualType::Error();
  }
}

}  // namespace compiler
