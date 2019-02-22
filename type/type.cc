#include "type/type.h"

#include <unordered_map>
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "misc/architecture.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/generic_struct.h"

namespace type {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  Type const *EnumName = new Primitive(PrimType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

Type const *Void() { return Tup({}); }

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
}

bool VerifyAssignment(TextSpan const &span, type::Type const *to,
                      type::Type const *from, Context *ctx) {
  if (to == from && to->is<GenericStruct>()) { return true; }

  // TODO this feels like the semantics are iffy. It works fine if we assign
  // to/from the same type, but we really care if you can assign to a type
  // rather than copy from another, I think.
  if (!from->IsMovable()) {
    ctx->error_log()->NotMovable(span, from);
    return false;
  }

  if (to == from) { return true; }
  auto *to_tup   = to->if_as<Tuple>();
  auto *from_tup = from->if_as<Tuple>();
  if (to_tup && from_tup) {
    if (to_tup->size() != from_tup->size()) {
      ctx->error_log()->MismatchedAssignmentSize(span, to_tup->size(),
                                               from_tup->size());
      return false;
    }

    bool result = true;
    for (size_t i = 0; i < to_tup->size(); ++i) {
      result &= VerifyAssignment(span, to_tup->entries_.at(i),
                                 from_tup->entries_.at(i), ctx);
    }
    return result;
  }

  if (auto *to_var = to->if_as<Variant>()) {
    if (auto *from_var = from->if_as<Variant>()) {
      for (auto fvar : from_var->variants_) {
        if (!to_var->contains(fvar)) {
          NOT_YET("log an error", from, to);
          return false;
        }
      }
      return true;
    } else {
      if (!to_var->contains(from)) {
        NOT_YET("log an error", from, to);
        return false;
      }

      return true;
    }
  }

  if (auto *to_ptr = to->if_as<Pointer>()) {
    if (from == NullPtr) { return true; }
    NOT_YET("log an error", from, to);
    return false;
  }

  NOT_YET("log an error: no cast from ", from, " to ", to);
}

void EmitCopyInit(Type const *from_type, ir::Results const &from_val,
                  Typed<ir::Register> to_var, Context *ctx) {
  auto *to_type = to_var.type()->as<Pointer>().pointee;
  // TODO Optimize once you understand the semantics better.
  if (!to_type->is<Primitive>() && !to_type->is<Function>() &&
      !to_type->is<Variant>()) {
    to_type->EmitInit(to_var.get(), ctx);
  }
  to_type->EmitCopyAssign(from_type, from_val, to_var.get(), ctx);
}

void EmitMoveInit(Type const *from_type, ir::Results const &from_val,
                  Typed<ir::Register> to_var, Context *ctx) {
  auto *to_type = to_var.type()->as<Pointer>().pointee;
  // TODO Optimize once you understand the semantics better.
  if (!to_type->is<Primitive>() && !to_type->is<Function>() &&
      !to_type->is<Variant>()) {
    to_type->EmitInit(to_var.get(), ctx);
  }
  to_type->EmitMoveAssign(from_type, from_val, to_var.get(), ctx);
}

}  // namespace type
