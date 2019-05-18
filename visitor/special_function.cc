#include "visitor/special_function.h"
#include "ast/declaration.h"
#include "ir/any_func.h"
#include "misc/context.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "visitor/emit_ir.h"

namespace visitor {
std::optional<ir::AnyFunc> SpecialFunction(EmitIr const *visitor,
                                           type::Struct const *s,
                                           char const *symbol, Context *ctx) {
  auto *ptr_to_s = type::Ptr(s);
  for (auto const *decl : s->scope_->AllDeclsWithId(symbol)) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = ctx->type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<type::Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != ptr_to_s) { continue; }
    return decl->EmitIr(visitor, ctx).get<ir::AnyFunc>(0).val_;
  }
  return std::nullopt;
}
}  // namespace visitor
