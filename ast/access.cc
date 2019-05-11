#include "ast/access.h"

#include "ast/declaration.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "misc/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"

namespace ast {
namespace {
using ::matcher::InheritsFrom;

}  // namespace

void Access::DependentDecls(DeclDepGraph *g,
                            Declaration *d) const {
  operand->DependentDecls(g, d);
}

bool Access::InferType(type::Type const *t, InferenceState *state) const {
  return false;
}

std::vector<ir::RegisterOr<ir::Addr>> Access::EmitLVal(Context *ctx) {
  auto reg = operand->EmitLVal(ctx)[0];
  auto *t  = ctx->type_of(operand.get());

  while (t->is<type::Pointer>()) {
    t   = t->as<type::Pointer>().pointee;
    reg = ir::Load<ir::Addr>(reg, t);
  }

  ASSERT(t, InheritsFrom<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return {ir::Field(reg, struct_type, struct_type->index(member_name)).get()};
}

ir::Results Access::EmitIr(Context *ctx) {
  if (ctx->type_of(operand.get()) == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    return backend::EvaluateAs<Module const *>(operand.get(), ctx)
        ->GetDecl(member_name)
        ->EmitIr(ctx);
  }

  auto *this_type = ctx->type_of(this);
  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(member_name);
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(member_name);
    return ir::Results{lit};
  } else {
    auto reg = operand->EmitLVal(ctx)[0];
    auto *t  = ctx->type_of(operand.get());

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (t->is<type::Pointer>()) {
      t   = t->as<type::Pointer>().pointee;
      reg = ir::Load<ir::Addr>(reg, t);
    }

    ASSERT(t, InheritsFrom<type::Struct>());
    auto *struct_type = &t->as<type::Struct>();
    auto field = ir::Field(reg, struct_type, struct_type->index(member_name));
    return ir::Results{ir::PtrFix(field.get(), this_type)};
  }

}

}  // namespace ast
