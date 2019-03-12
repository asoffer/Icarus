#include "ast/builtin_fn.h"

#include "backend/eval.h"
#include "ir/arguments.h"
#include "ir/cmd.h"
#include "ir/out_params.h"
#include "type/type.h"

namespace ast {
VerifyResult BuiltinFn::VerifyCall(
    core::FnArgs<std::unique_ptr<Expression>> const &args,
    core::FnArgs<VerifyResult> const &arg_results, Context *ctx) const {
  switch (b_) {
    case ir::Builtin::Foreign: {
      bool err = false;
      if (arg_results.num_named() != 0u) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `foreign` cannot be "
                                       "called with named arguments.");
        err = true;
      }

      size_t size = arg_results.num_named() + arg_results.num_pos();
      if (size != 2u) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `foreign` takes "
                                       "exactly two arguments (You provided " +
                                           std::to_string(size) + ").");
        err = true;
      }

      if (!err) {
        if (arg_results.at(0).type_ != type::ByteView) {
          ctx->error_log()->BuiltinError(
              span,
              "First argument to `foreign` must be a byte-view (You provided "
              "a(n) " +
                  arg_results.at(0).type_->to_string() + ").");
        }
        if (!arg_results.at(0).const_) {
          ctx->error_log()->BuiltinError(
              span, "First argument to `foreign` must be a constant.");
        }
        if (arg_results.at(1).type_ != type::Type_) {
          ctx->error_log()->BuiltinError(
              span,
              "Second argument to `foreign` must be a type (You provided "
              "a(n) " +
                  arg_results.at(0).type_->to_string() + ").");
        }
        if (!arg_results.at(1).const_) {
          ctx->error_log()->BuiltinError(
              span, "Second argument to `foreign` must be a constant.");
        }
      }
      return VerifyResult::Constant(
          backend::EvaluateAs<type::Type const *>(args.at(1).get(), ctx));
    } break;
    case ir::Builtin::Opaque:
      if (!arg_results.empty()) {
        ctx->error_log()->BuiltinError(
            span, "Built-in function `opaque` takes no arguments.");
      }
      return VerifyResult::Constant(
          ir::BuiltinType(ir::Builtin::Opaque)->as<type::Function>().output[0]);

    case ir::Builtin::Bytes: {
      size_t size = arg_results.num_named() + arg_results.num_pos();
      if (arg_results.num_named() != 0u) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `bytes` cannot be "
                                       "called with named arguments.");
      } else if (size != 1u) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `bytes` takes "
                                       "exactly one argument (You provided " +
                                           std::to_string(size) + ").");
      } else if (arg_results.at(0).type_ != type::Type_) {
        ctx->error_log()->BuiltinError(
            span,
            "Built-in function `bytes` must take a single argument of type "
            "`type` (You provided a(n) " +
                arg_results.at(0).type_->to_string() + ").");
      }
      return VerifyResult::Constant(
          ir::BuiltinType(ir::Builtin::Bytes)->as<type::Function>().output[0]);
    }
    case ir::Builtin::Alignment: {
      size_t size = arg_results.num_named() + arg_results.num_pos();
      if (arg_results.num_named()) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `alignment` cannot "
                                       "be called with named arguments.");
      }
      if (size != 1u) {
        ctx->error_log()->BuiltinError(span,
                                       "Built-in function `alignment` takes "
                                       "exactly one argument (You provided " +
                                           std::to_string(size) + ").");

      } else if (arg_results.at(0).type_ != type::Type_) {
        ctx->error_log()->BuiltinError(
            span,
            "Built-in function `alignment` must take a single argument of "
            "type `type` (you provided a(n) " +
                arg_results.at(0).type_->to_string() + ")");
      }
      return VerifyResult::Constant(ir::BuiltinType(ir::Builtin::Alignment)
                                        ->as<type::Function>()
                                        .output[0]);
    }
#ifdef DBG
    case ir::Builtin::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_results, matcher::IsEmpty());
      return VerifyResult::Constant(type::Void());
#endif  // DBG
  }
  UNREACHABLE();
}

}  // namespace ast
