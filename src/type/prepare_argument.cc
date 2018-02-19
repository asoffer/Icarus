#include "../ir/cmd.h"
#include "type.h"

extern IR::Val PtrCallFix(const IR::Val& v);

IR::Val Primitive::PrepareArgument(Type *from, const IR::Val& val) const {
  if (from->is<Variant>()) {
    return IR::Load(IR::VariantValue(const_cast<Primitive *>(this), val));
  } else {
    ASSERT_EQ(from, this);
    return val;
  }
}

IR::Val Array::PrepareArgument(Type *from, const IR::Val &val) const {
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

IR::Val Pointer::PrepareArgument(Type *from, const IR::Val& val) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Tuple::PrepareArgument(Type *from, const IR::Val&) const {
  NOT_YET(this, from);
}

IR::Val Function::PrepareArgument(Type *from, const IR::Val&) const {
  NOT_YET(this, from);
}

IR::Val Enum::PrepareArgument(Type *from, const IR::Val& val) const {
  ASSERT_EQ(from, this);
  return val;
}

IR::Val Struct::PrepareArgument(Type *from, const IR::Val& val) const {
  auto arg = IR::Alloca(const_cast<Struct *>(this));
  if (from->is<Variant>()) {
    const_cast<Struct *>(this)->EmitAssign(
        const_cast<Struct *>(this),
        IR::VariantValue(const_cast<Struct *>(this), val), arg);
  } else if (this == from) {
    const_cast<Struct *>(this)->EmitAssign(from, val, arg);
  } else {
    UNREACHABLE(from);
  }
  return arg;
}

IR::Val Variant::PrepareArgument(Type *from, const IR::Val& val) const {
  if (this == from) { return val; }
  auto arg = IR::Alloca(const_cast<Variant *>(this));
  Type_->EmitAssign(Type_, IR::Val::Type(from), IR::VariantType(arg));
  // TODO this isn't exactly right because 'from' might not be the appropriate
  // type here.
  from->EmitAssign(from, val, IR::VariantValue(from, arg));
  return arg;
}
IR::Val RangeType::PrepareArgument(Type *from, const IR::Val&) const {
  NOT_YET(this, from);
}
IR::Val SliceType::PrepareArgument(Type *from, const IR::Val&) const {
  NOT_YET(this, from);
}
IR::Val Scope_Type::PrepareArgument(Type *from, const IR::Val&) const {
  NOT_YET(this, from);
}
