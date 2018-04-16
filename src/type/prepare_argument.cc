#include "../ir/cmd.h"
#include "all.h"

extern IR::Val PtrCallFix(const IR::Val &v);

namespace type {
IR::Val Array::PrepareArgument(const Type *from, const IR::Val &val,
                               Context *ctx) const {
  if (from->is<Variant>()) {
    NOT_YET(this, from);
  } else {
    ASSERT_EQ(from, this);
    if (fixed_length) {
      // TODO Copy may be overkill. Think about value category.
      auto arg = IR::Alloca(from);
      from->EmitAssign(from, val, arg, ctx);
      return arg;
    } else {
      NOT_YET();
    }
  }
}

IR::Val Primitive::PrepareArgument(const Type *from, const IR::Val &val,
                                   Context *ctx) const {
  if (from->is<Variant>()) {
    return IR::Load(IR::VariantValue(this, val));
  } else {
    ASSERT_EQ(from, this);
    return val;
  }
}

IR::Val Pointer::PrepareArgument(const Type *from, const IR::Val &val,
                                 Context *ctx) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Function::PrepareArgument(const Type *from, const IR::Val &,
                                  Context *ctx) const {
  NOT_YET(this, from);
}

IR::Val Enum::PrepareArgument(const Type *from, const IR::Val &val,
                              Context *ctx) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Variant::PrepareArgument(const Type *from, const IR::Val &val,
                                 Context *ctx) const {
  if (this == from) { return val; }
  auto arg = IR::Alloca(this);
  type::Type_->EmitAssign(Type_, IR::Val::Type(from), IR::VariantType(arg),
                          ctx);
  // TODO this isn't exactly right because 'from' might not be the appropriate
  // type here.
  from->EmitAssign(from, val, IR::VariantValue(from, arg), ctx);
  return arg;
}
IR::Val Range::PrepareArgument(const Type *from, const IR::Val &,
                               Context *ctx) const {
  NOT_YET(this, from);
}
IR::Val Slice::PrepareArgument(const Type *from, const IR::Val &,
                               Context *ctx) const {
  NOT_YET(this, from);
}
IR::Val Scope::PrepareArgument(const Type *from, const IR::Val &,
                               Context *ctx) const {
  NOT_YET(this, from);
}

IR::Val Struct::PrepareArgument(const Type *from, const IR::Val &val,
                                Context *ctx) const {
  auto arg = IR::Alloca(this);
  if (from->is<Variant>()) {
    EmitAssign(this, IR::VariantValue(this, val), arg, ctx);
  } else if (this == from) {
    EmitAssign(from, val, arg, ctx);
  } else {
    UNREACHABLE(from);
  }
  return arg;
}

} // namespace type
