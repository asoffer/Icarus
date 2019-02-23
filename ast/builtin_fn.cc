#include "ast/builtin_fn.h"

#include "backend/eval.h"
#include "ir/arguments.h"
#include "ir/cmd.h"
#include "ir/out_params.h"
#include "type/type.h"

namespace ast {
VerifyResult BuiltinFn::VerifyCall(
    FnArgs<std::unique_ptr<Expression>> const &args,
    FnArgs<VerifyResult> const &arg_results, Context *ctx) const {
  switch (b_) {
    case ir::Builtin::Foreign:
      if (!arg_results.named_.empty()) { NOT_YET(); }
      if (arg_results.pos_.size() != 2u) {
        NOT_YET();
      } else {
        if (arg_results.pos_[0].type_ != type::ByteView) { NOT_YET(); }
        if (!arg_results.pos_[0].const_) { NOT_YET(); }
        if (arg_results.pos_[0].type_ != type::Type_) { NOT_YET(); }
        if (!arg_results.pos_[0].const_) { NOT_YET(); }
        return VerifyResult::Constant(
            backend::EvaluateAs<type::Type const *>(args.pos_[1].get(), ctx));
      }

    case ir::Builtin::Opaque:
      if (!arg_results.empty()) { NOT_YET(); }
      return VerifyResult::Constant(
          ir::BuiltinType(ir::Builtin::Opaque)->as<type::Function>().output[0]);

    case ir::Builtin::Bytes:
      if (!arg_results.named_.empty()) { NOT_YET(); }
      if (arg_results.pos_.size() != 1u) {
        NOT_YET();
      } else if (arg_results.pos_[0].type_ != type::Type_) {
        NOT_YET();
      }
      return VerifyResult::Constant(
          ir::BuiltinType(ir::Builtin::Bytes)->as<type::Function>().output[0]);

    case ir::Builtin::Alignment:
      if (!arg_results.named_.empty()) { NOT_YET(); }
      if (arg_results.pos_.size() != 1u) {
        NOT_YET();
      } else if (arg_results.pos_[0].type_ != type::Type_) {
        NOT_YET();
      }
      return VerifyResult::Constant(ir::BuiltinType(ir::Builtin::Alignment)
                                        ->as<type::Function>()
                                        .output[0]);
#ifdef DBG
    case ir::Builtin::DebugIr:
      if (!arg_results.empty()) { NOT_YET(); }
      return VerifyResult::Constant(type::Void());
#endif  // DBG
  }
  UNREACHABLE();
}

}  // namespace ast
