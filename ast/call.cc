#include "ast/call.h"

#include <sstream>

#include "absl/container/flat_hash_map.h"
#include "ast/block_literal.h"
#include "ast/builtin_fn.h"
#include "ast/dispatch_table.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "ast/unop.h"
#include "backend/eval.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/array.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ast {
std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  args_.ApplyWithIndex(
      [&](auto &&index, std::unique_ptr<Expression> const &expr) {
        ss << (seen_one ? ", " : "");
        if constexpr (!std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          ss << index << " = ";
        }
        ss << expr->to_string(n);
        seen_one = true;
      });
  ss << ")";
  return ss.str();
}

ir::Results Call::EmitIr(Context *ctx) {
  if (auto *b = fn_->if_as<BuiltinFn>()) {
    switch (b->b_) {
      case ir::Builtin::Foreign: {
        auto name =
            backend::EvaluateAs<std::string_view>(args_.at(0).get(), ctx);
        auto *foreign_type =
            backend::EvaluateAs<type::Type const *>(args_.at(1).get(), ctx);
        return ir::Results{ir::LoadSymbol(name, foreign_type).get()};
      } break;

      case ir::Builtin::Opaque:
        return ir::Results{static_cast<ir::Reg>(ir::NewOpaqueType(ctx->mod_))};

      case ir::Builtin::Bytes: {
        auto const &fn_type =
            ir::BuiltinType(ir::Builtin::Bytes)->as<type::Function>();
        ir::Arguments call_args{&fn_type, args_.at(0)->EmitIr(ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::BytesFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

      case ir::Builtin::Alignment: {
        auto const &fn_type =
            ir::BuiltinType(ir::Builtin::Alignment)->as<type::Function>();
        ir::Arguments call_args{&fn_type, args_.at(0)->EmitIr(ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::AlignmentFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

#ifdef DBG
      case ir::Builtin::DebugIr: ir::DebugIr(); return ir::Results{};
#endif  // DBG
    }
    UNREACHABLE();
    //} else if (auto *t = fn->if_as<Terminal>()) {
    //  UNREACHABLE(this);
    //  if (auto *bs = std::get_if<ir::BlockSequence>(&fn_val.value)) {
    //    // TODO might be optional.
    //    return ir::Results{*bs};
    //  }
  }

  auto const &dispatch_table = *ASSERT_NOT_NULL(ctx->dispatch_table(this));
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching
  // insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args =
      args_.Transform([ctx](std::unique_ptr<Expression> const &expr)
                          -> std::pair<Expression const *, ir::Results> {
        return std::pair(expr.get(), expr->EmitIr(ctx));
      });

  return dispatch_table.EmitCall(
      args, ctx, contains_hashtag(Hashtag(Hashtag::Builtin::Inline)));
}

}  // namespace ast
