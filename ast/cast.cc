#include "ast/cast.h"

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "misc/context.h"
#include "type/cast.h"
#include "type/tuple.h"

namespace ast {

std::string Cast::to_string(size_t n) const {
  return "(" + expr_->to_string(n) + ") as (" + type_->to_string(n) + ")";
}

ir::Results Cast::EmitIr(Context *ctx) {
  if (auto *dispatch_table = ctx->dispatch_table(this)) {
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<Expression const *, ir::Results>>(
            {std::pair(expr_.get(), expr_->EmitIr(ctx)),
             std::pair(type_.get(), type_->EmitIr(ctx))},
            {}),
        ctx);
  }

  auto *this_type = ASSERT_NOT_NULL(ctx->type_of(this));
  auto results    = expr_->EmitIr(ctx);
  if (this_type == type::Type_) {
    std::vector<type::Type const *> entries;
    entries.reserve(results.size());
    for (size_t i = 0; i < results.size(); ++i) {
      // TODO what about incomplete structs?
      entries.push_back(results.get<type::Type const *>(i).val_);
    }
    return ir::Results{type::Tup(entries)};
  }
  return ir::Cast(ctx->type_of(expr_.get()), this_type, results);
}

std::vector<ir::RegisterOr<ir::Addr>> Cast::EmitLVal(Context *ctx) {
  NOT_YET();
}

}  // namespace ast
