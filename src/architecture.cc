#include "architecture.h"

#include "context.h"
#include "ir/cmd.h"

IR::Val Architecture::ComputeArrayLength(const IR::Val &len,
                                         const Type *t) const {
  auto space_in_array = MoveForwardToAlignment(t, bytes(t));
  return IR::Mul(len, IR::Val::Int(static_cast<i32>(space_in_array)));
}

size_t Architecture::alignment(const Type *t) const {
  if (t->is<Primitive>()) {
    switch (t->as<const Primitive>().type_) {
    case PrimType::Generic:
    case PrimType::Err: NOT_YET();
    case PrimType::Unknown: UNREACHABLE();
    case PrimType::EmptyArray:
    case PrimType::Void: return 0;
    case PrimType::Bool:
    case PrimType::Char: return 1;
    case PrimType::Int:
    case PrimType::Real:
    case PrimType::Type:
    case PrimType::NullPtr:
    case PrimType::Code: return 8;
    case PrimType::String: return 8;
    }
  } else if (t->is<Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<Array>()) {
    auto *array_type = &t->as<const Array>();
    return array_type->fixed_length ? this->alignment(array_type->data_type)
                                    : ptr_align_;
  } else if (t->is<Struct>()) {
    auto *struct_type = const_cast<Struct *>(&t->as<const Struct>());
    size_t alignment_val = 1;
    for (const auto& field : struct_type->fields_) {
      alignment_val = std::max(alignment_val, this->alignment(field.type));
    }
    return alignment_val;
  } else if (t->is<Function>()) {
    return  ptr_align_;
  } else if (t->is<Enum>()) {
    return 8; // TODO
  } else if (t->is<Scope_Type>()) {
    return 1;
  } else if (t->is<Variant>()) {
    size_t alignment_val = this->alignment(Type_);
    for (Type* type : t->as<Variant>().variants_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else {
    NOT_YET();
  }
  UNREACHABLE();
}

size_t Architecture::bytes(const Type *t) const {
  if (t->is<Primitive>()) {
    switch (t->as<const Primitive>().type_) {
    case PrimType::Generic:
    case PrimType::Err: NOT_YET();
    case PrimType::Unknown: UNREACHABLE();
    case PrimType::EmptyArray:
    case PrimType::Void: return 0;
    case PrimType::Bool:
    case PrimType::Char: return 1;
    case PrimType::Int:
    case PrimType::Real:
    case PrimType::Type:
    case PrimType::NullPtr:
    case PrimType::Code: return 8;
    case PrimType::String: return 8;
    }
  } else if (t->is<Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<Array>()) {
    auto *array_type = &t->as<const Array>();
    if (array_type->fixed_length) {
      return array_type->len *
             MoveForwardToAlignment(array_type->data_type,
                                    bytes(array_type->data_type));
    } else {
      return 2 * ptr_bytes_;
    }
  } else if (t->is<Struct>()) {
    auto *struct_type = const_cast<Struct *>(&t->as<const Struct>());
    size_t num_bytes  = 0;
    for (const auto &field : struct_type->fields_) {
      num_bytes += this->bytes(field.type);
      num_bytes = this->MoveForwardToAlignment(field.type, num_bytes);
    }

    return MoveForwardToAlignment(struct_type, num_bytes);
  } else if (t->is<Function>()) {
    return 2 * ptr_bytes_;
  } else if (t->is<Enum>()) {
    return 8; // TODO
  } else if (t->is<Scope_Type>()) {
    return 0;
  } else if (t->is<Variant>()) {
    size_t num_bytes = 0;
    for (Type* type : t->as<Variant>().variants_) {
      num_bytes = std::max(num_bytes, this->bytes(type));
    }
    return num_bytes + ptr_bytes_;
  } else {
    NOT_YET();
  }
  UNREACHABLE();
}
