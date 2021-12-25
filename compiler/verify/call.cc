#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/type_for_diagnostic.h"
#include "type/callable.h"
#include "type/cast.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct BuiltinError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "builtin-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }
  frontend::SourceView view;
  std::string message;
};

struct UserDefinedError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "user-defined-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }
  frontend::SourceView view;
  std::string message;
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

type::QualType VerifySliceCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `slice` cannot be called with named "
                   "arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `slice` takes exactly two "
                                "arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (not arg_vals[0].type().is<type::BufferPointer>()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `slice` must be a buffer "
                                "pointer (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (!type::CanCastImplicitly(arg_vals[1].type(), type::U64)) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Second argument to `slice` must be "
                                "implicitly convertible to `u64` (You "
                                "provided `",
                                arg_vals[1].type().to_string(), "`)."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  return type::QualType::NonConstant(
      type::Slc(arg_vals[0].type().as<type::BufferPointer>().pointee()));
}

type::QualType VerifyCompilationErrorCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `compilation_error` cannot be called "
                   "with named arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `compilation_error` takes "
                                "exactly two arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (arg_vals[0].type() != type::Type_) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `compilation_error` must be "
                                "a type (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (arg_vals[0]->empty()) {
    c->diag().Consume(BuiltinError{
        .view = view,
        .message =
            "First argument to `compilation_error` must be a constant."});
    error = true;
  }

  if (arg_vals[1].type() != type::Slc(type::Char)) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Second argument to `compilation_error` must "
                                "be a []char (You provided a(n) ",
                                arg_vals[0].type().to_string(), ").")});
    error = true;
  }

  if (arg_vals[1]->empty()) {
    c->diag().Consume(BuiltinError{
        .view = view,
        .message =
            "Second argument to `compilation_error` must be a constant."});
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  std::string_view error_text = arg_vals[1]->get<ir::Slice>();
  c->diag().Consume(
      UserDefinedError{.view = view, .message = std::string(error_text)});
  return type::QualType::NonConstant(arg_vals[0]->get<type::Type>());
}

type::QualType VerifyForeignCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  bool error = false;
  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `foreign` cannot be called with named "
                   "arguments.",
    });
    error = true;
  }

  size_t size = arg_vals.size();
  if (size != 2u) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `foreign` takes exactly two "
                                "arguments (You provided ",
                                size, ")."),
    });
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  if (arg_vals[0].type() != type::Slc(type::Char)) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("First argument to `foreign` must be a "
                                "[]char (You provided a(n) ",
                                arg_vals[0].type().to_string(), ")."),
    });
    error = true;
  }

  if (arg_vals[0]->empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "First argument to `foreign` must be a constant."});
    error = true;
  }

  if (arg_vals[1].type() != type::Type_) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat(
            "Second argument to `foreign` must be a type (You provided a(n) ",
            arg_vals[0].type().to_string(), ").")});
    error = true;
  }

  if (arg_vals[1]->empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Second argument to `foreign` must be a constant."});
    error = true;
  }

  if (error) { return type::QualType::Error(); }

  type::Type foreign_type = arg_vals[1]->get<type::Type>();
  if (not foreign_type.is<type::Function>() and
      not foreign_type.is<type::Pointer>()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Builtin `foreign` may only be called when the second "
                   "argument is a pointer or a function type.",
    });
    return type::QualType::Error();
  }

  return type::QualType::Constant(foreign_type);
}

type::QualType VerifyReserveMemoryCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  type::QualType qt = type::QualType::NonConstant(type::BufPtr(type::Byte));
  size_t size       = arg_vals.size();
  if (size != 2u) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = absl::StrCat("Built-in function `reserve_memory` takes "
                                "exactly two arguments (You provided ",
                                size, ")."),
    });
    qt.MarkError();
  } else {
    for (size_t i : {0, 1}) {
      if (!type::CanCastImplicitly(arg_vals[i].type(), type::U64)) {
        c->diag().Consume(BuiltinError{
            .view = view,
            .message =
                absl::StrCat("Arguments to `reserve_memory` must be "
                             "implicitly convertible to `u64` (You provided `",
                             arg_vals[i].type().to_string(), "`)."),
        });
        qt.MarkError();
        break;
      } else if (arg_vals[i]->empty()) {
        c->diag().Consume(
            BuiltinError{.view    = view,
                         .message = "Arguments to `reserve_memory` must be "
                                    "compile-time constants."});
        qt.MarkError();
        break;
      }
    }
  }

  return qt;
}

type::QualType VerifyOpaqueCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  type::QualType qt = type::QualType::Constant(
      ir::Fn(ir::BuiltinFn::Opaque()).type()->return_types()[0]);
  if (not arg_vals.empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `opaque` takes no arguments."});
    qt.MarkError();
  }
  return qt;
}

type::QualType VerifyBytesCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  auto qt = type::QualType::Constant(
      ir::Fn(ir::BuiltinFn::Bytes()).type()->return_types()[0]);

  if (not arg_vals.named().empty()) {
    c->diag().Consume(BuiltinError{.view = view,
                                   .message =
                                       "Built-in function `bytes` cannot be "
                                       "called with named arguments."});
    qt.MarkError();
  } else {
    if (size_t size = arg_vals.size(); size != 1u) {
      c->diag().Consume(BuiltinError{
          .view    = view,
          .message = absl::StrCat("Built-in function `bytes` takes exactly "
                                  "one argument (You provided ",
                                  size, ")."),
      });

      qt.MarkError();
    } else if (arg_vals[0].type() != type::Type_) {
      c->diag().Consume(BuiltinError{
          .view = view,
          .message =
              absl::StrCat("Built-in function `bytes` must take a single "
                           "argument of type `type` (You provided a(n) ",
                           arg_vals[0].type().to_string(), ").")});
      qt.MarkError();
    }
  }

  return qt;
}

type::QualType VerifyAlignmentCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  auto qt = type::QualType::Constant(
      ir::Fn(ir::BuiltinFn::Alignment()).type()->return_types()[0]);

  if (not arg_vals.named().empty()) {
    c->diag().Consume(
        BuiltinError{.view    = view,
                     .message = "Built-in function `alignment` cannot be "
                                "called with named arguments."});
    qt.MarkError();
  } else {
    if (size_t size = arg_vals.size(); size != 1u) {
      c->diag().Consume(BuiltinError{
          .view    = view,
          .message = absl::StrCat("Built-in function `alignment` takes exactly "
                                  "one argument (You provided ",
                                  size, ")."),
      });

      qt.MarkError();
    } else if (arg_vals[0].type() != type::Type_) {
      c->diag().Consume(BuiltinError{
          .view = view,
          .message =
              absl::StrCat("Built-in function `alignment` must take a single "
                           "argument of type `type` (You provided a(n) ",
                           arg_vals[0].type().to_string(), ").")});
      qt.MarkError();
    }
  }

  return qt;
}

type::QualType VerifyAbortCall(
    Compiler *c, frontend::SourceView view,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arg_vals) {
  auto qt = type::QualType::NonConstant(type::Void);

  if (not arg_vals.empty()) {
    c->diag().Consume(BuiltinError{
        .view    = view,
        .message = "Built-in function `abort` takes no arguments."});
    qt.MarkError();
  }

  return qt;
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::Call const *node) {
  LOG("Call", "Verifying %s", node->DebugString());

  ir::CompleteResultBuffer buffer;
  ASSIGN_OR(return type::QualType::ErrorSpan(),  //
                   auto arg_vals, VerifyArguments(node->arguments(), buffer));
  // TODO: consider having `foreign` be a generic type. This would allow for the
  // possibility of overlading builtins. That's a dangerous yet principled idea.
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    type::QualType qt;
    switch (b->value().which()) {
      case ir::BuiltinFn::Which::Slice: {
        ast::OverloadSet os;
        os.insert(node);
        context().SetAllOverloads(node, std::move(os));
        qt = VerifySliceCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::ReserveMemory: {
        qt = VerifyReserveMemoryCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Foreign: {
        ast::OverloadSet os;
        os.insert(node);
        context().SetAllOverloads(node, std::move(os));
        qt = VerifyForeignCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::CompilationError: {
        qt = VerifyCompilationErrorCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Opaque: {
        qt = VerifyOpaqueCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Bytes: {
        qt = VerifyBytesCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Alignment: {
        qt = VerifyAlignmentCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::Abort: {
        qt = VerifyAbortCall(this, SourceViewFor(b), arg_vals);
      } break;
      case ir::BuiltinFn::Which::DebugIr: {
        // This is for debugging the compiler only, so there's no need to
        // write decent errors here.
        ASSERT(arg_vals.size() == 0u);
        qt = type::QualType::Constant(type::Void);
      }
    }
    return context().set_qual_type(node, qt);
  }

  absl::flat_hash_set<type::Type> argument_dependent_lookup_types;
  for (auto const &arg : node->prefix_arguments()) {
    argument_dependent_lookup_types.insert(
        context().qual_types(&arg.expr())[0].type());
  }
  auto callee_qt =
      VerifyCallee(node->callee(), argument_dependent_lookup_types);
  LOG("Call", "Callee's qual-type is %s", callee_qt);
  if (not callee_qt.ok()) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  auto qts_or_errors = VerifyCall(node, arg_vals);
  if (auto *errors = std::get_if<
          absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>(
          &qts_or_errors)) {
    core::Arguments<std::string> argument_type_strings;
    for (auto const &arg : node->positional_arguments()) {
      argument_type_strings.pos_emplace(
          TypeForDiagnostic(&arg.expr(), context()));
    }
    for (auto const &arg : node->named_arguments()) {
      argument_type_strings.named_emplace(
          arg.name(), TypeForDiagnostic(&arg.expr(), context()));
    }

    diag().Consume(UncallableWithArguments{
        .arguments = std::move(argument_type_strings),
        .errors    = std::move(*errors),
        .view      = SourceViewFor(node->callee()),
    });
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto &qual_type = std::get<std::vector<type::QualType>>(qts_or_errors);
  LOG("Call", "Call qual-type is %s on %p", qual_type, &context());
  // TODO: under what circumstances can we prove that the implementation
  // doesn't need to be run at runtime?
  return context().set_qual_types(node, std::move(qual_type));
}

bool Compiler::VerifyPatternType(ast::Call const *node, type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));

  // Note that the type here cannot use ADL. Maybe we could inspect the
  // expression arguments and use ADL for anything we see has a module set? But
  // that seems like a bad idea.
  ASSIGN_OR(return false,  //
                   auto qt, VerifyType(node->callee())[0]);
  if (auto const *gs = qt.type().if_as<type::Generic<type::Struct>>()) {
    for (auto const &arg : node->arguments()) {
      // TODO: Having these always be types is problematic, but for now we don't
      // have a way to deduce another possibility.
      EnqueueVerifyPatternMatchType(&arg.expr(), type::Type_);
    }

    return true;
  } else {
    return false;
  }
}

}  // namespace compiler
