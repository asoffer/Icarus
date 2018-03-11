#include "../ir/cmd.h"
#include "type.h"

extern IR::Val PtrCallFix(const IR::Val &v);

IR::Val Primitive::PrepareArgument(const Type *from, const IR::Val &val) const {
  if (from->is<Variant>()) {
    return IR::Load(IR::VariantValue(this, val));
  } else {
    ASSERT_EQ(from, this);
    return val;
  }
}

IR::Val Array::PrepareArgument(const Type *from, const IR::Val &val) const {
  if (from->is<Variant>()) {
    NOT_YET(this, from);
  } else {
    ASSERT_EQ(from, this);
    if (fixed_length) {
      // TODO Copy may be overkill. Think about value category.
      auto arg = IR::Alloca(from);
      from->EmitAssign(from, val, arg);
      return arg;
    } else {
      NOT_YET();
    }
  }
}

IR::Val Pointer::PrepareArgument(const Type *from, const IR::Val &val) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Tuple::PrepareArgument(const Type *from, const IR::Val &) const {
  NOT_YET(this, from);
}

IR::Val Function::PrepareArgument(const Type *from, const IR::Val &) const {
  NOT_YET(this, from);
}

IR::Val Enum::PrepareArgument(const Type *from, const IR::Val &val) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Struct::PrepareArgument(const Type *from, const IR::Val &val) const {
  auto arg = IR::Alloca(this);
  if (from->is<Variant>()) {
    EmitAssign(this, IR::VariantValue(this, val), arg);
  } else if (this == from) {
    EmitAssign(from, val, arg);
  } else {
    UNREACHABLE(from);
  }
  return arg;
}

IR::Val Variant::PrepareArgument(const Type *from, const IR::Val &val) const {
  if (this == from) { return val; }
  auto arg = IR::Alloca(this);
  Type_->EmitAssign(Type_, IR::Val::Type(from), IR::VariantType(arg));
  // TODO this isn't exactly right because 'from' might not be the appropriate
  // type here.
  from->EmitAssign(from, val, IR::VariantValue(from, arg));
  return arg;
}
IR::Val RangeType::PrepareArgument(const Type *from, const IR::Val &) const {
  NOT_YET(this, from);
}
IR::Val SliceType::PrepareArgument(const Type *from, const IR::Val &) const {
  NOT_YET(this, from);
}
IR::Val Scope_Type::PrepareArgument(const Type *from, const IR::Val &) const {
  NOT_YET(this, from);
}
