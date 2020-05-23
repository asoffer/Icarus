#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/errors.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

template <typename EPtr, typename StrType>
type::QualType VerifyCall(
    Compiler *c, ast::BuiltinFn const *b,
    core::FnArgs<EPtr, StrType> const &args,
    core::FnArgs<type::Typed<ir::Value>> const &arg_vals) {
  // TODO for builtin's consider moving all the messages into an enum.
  switch (b->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      bool err = false;
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `foreign` cannot be called with "
                       "named arguments.",
        });
        err = true;
      }

      size_t size = arg_vals.size();
      if (size != 2u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat("Built-in function `foreign` takes exactly "
                                    "two arguments (You provided ",
                                    size, ")."),
        });
        err = true;
      }

      if (not err) {
        if (arg_vals[0].type() != type::ByteView) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = absl::StrCat("First argument to `foreign` must be a "
                                      "byte-view (You provided a(n) ",
                                      arg_vals[0].type()->to_string(), ")."),
          });
        }
        if (arg_vals[0]->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = "First argument to `foreign` must be a constant."});
        }
        if (arg_vals[1].type() != type::Type_) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range = b->range(),
              .message =
                  absl::StrCat("Second argument to `foreign` must be a type "
                               "(You provided a(n) ",
                               arg_vals[0].type()->to_string(), ").")});
        }
        if (arg_vals[1]->empty()) {
          c->diag().Consume(diagnostic::BuiltinError{
              .range   = b->range(),
              .message = "Second argument to `foreign` must be a constant."});
        }
      }

      auto maybe_type = c->EvaluateAs<type::Type const *>(args[1]);
      if (not maybe_type) { NOT_YET(); }
      auto const *foreign_type = *maybe_type;
      if (not foreign_type->template is<type::Function>() and
          not foreign_type->template is<type::Pointer>()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Builtin `foreign` may only be called when the second "
                       "argument is a pointer or a function type.",
        });
      }
      return type::QualType::Constant(foreign_type);
    } break;
    case ir::BuiltinFn::Which::Opaque:
      if (not arg_vals.empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `opaque` takes no arguments."});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Opaque().type()->output()[0]);

    case ir::BuiltinFn::Which::Bytes: {
      size_t size = arg_vals.size();
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `bytes` cannot be called with named "
                       "arguments."});
      } else if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat(
                "Built-in function `bytes` takes exactly one argument "
                "(You provided ",
                size, ")."),
        });
      } else if (arg_vals[0].type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->range(),
            .message =
                absl::StrCat("Built-in function `bytes` must take a single "
                             "argument of type `type` (You provided a(n) ",
                             arg_vals[0].type()->to_string(), ").")});
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Bytes().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::Alignment: {
      size_t size = arg_vals.size();
      if (not arg_vals.named().empty()) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = "Built-in function `alignment` cannot be called with "
                       "named arguments."});
      }
      if (size != 1u) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range   = b->range(),
            .message = absl::StrCat("Built-in function `alignment` takes "
                                    "exactly one argument (You provided ",
                                    size, ")."),
        });

      } else if (arg_vals[0].type() != type::Type_) {
        c->diag().Consume(diagnostic::BuiltinError{
            .range = b->range(),
            .message =
                absl::StrCat("Built-in function `alignment` must take a single "
                             "argument of type `type` (you provided a(n) ",
                             arg_vals[0].type()->to_string(), ")"),
        });
      }
      return type::QualType::Constant(
          ir::BuiltinFn::Alignment().type()->output()[0]);
    }
    case ir::BuiltinFn::Which::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_vals.size() == 0u);
      return type::QualType::Constant(type::Void());
  }
  UNREACHABLE();
}

}  // namespace

type::QualType Compiler::VerifyType(ast::Call const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto arg_vals, VerifyFnArgs(node->args()));


  // Note: Currently `foreign` being generic means that we can't easily make
  // builtins overloadable, not that it ever makes sense to do so (because
  // they're globally available).
  //
  // TODO Once type::OverloadSet becomes more robust, we can make generics more
  // robust, then have `foreign` use generics and make this part of the overload
  // set code too.
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return type::QualType::Error(), auto result,
                     VerifyCall(this, b, node->args(), arg_vals));
    return data().set_qual_type(node, result);
  }

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto callee_qt, VerifyType(node->callee()));
  if (auto *c = callee_qt.type()->if_as<type::Callable>()) {
    DEBUG_LOG("Call.VerifyType")
    ("Callee's (", node->callee()->DebugString(), ") qual-type: ", callee_qt);
    auto ret_types = c->return_types(arg_vals);
    // TODO under what circumstances can we prove that the implementation
    // doesn't need to be run at runtime?
    return data().set_qual_type(
        node, type::QualType(ret_types, type::Quals::Unqualified()));
  } else {
    diag().Consume(diagnostic::UncallableExpression{
        .range = node->callee()->range(),
    });
    return data().set_qual_type(node, type::QualType::Error());
  }
}

}  // namespace compiler
