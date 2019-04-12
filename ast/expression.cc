#include "ast/expression.h"

#include "misc/context.h"

namespace ast {
void Expression::EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx) {
  type::EmitCopyInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
}
void Expression::EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx) {
  type::EmitMoveInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
}
}  // namespace ast
