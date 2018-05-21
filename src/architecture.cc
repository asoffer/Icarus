#include "architecture.h"

#include "context.h"
#include "ir/cmd.h"
#include "type/all.h"

IR::Val Architecture::ComputeArrayLength(const IR::Val &len,
                                         const type::Type *t) const {
  auto space_in_array = MoveForwardToAlignment(t, bytes(t));
  return IR::Mul(len, IR::Val::Int(static_cast<i32>(space_in_array)));
}

size_t Architecture::alignment(const type::Type *t) const {
  if (t->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Generic: NOT_YET();
      case type::PrimType::Module: NOT_YET();
      case type::PrimType::Err: NOT_YET();
      case type::PrimType::Block: NOT_YET();
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Char: return 1;
      case type::PrimType::Int:
      case type::PrimType::Real:
      case type::PrimType::Type:
      case type::PrimType::NullPtr:
      case type::PrimType::Code: return 8;
      case type::PrimType::String: return 8;
    }
  } else if (t->is<type::Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<type::Array>()) {
    auto *array_type = &t->as<type::Array>();
    return array_type->fixed_length ? this->alignment(array_type->data_type)
                                    : ptr_align_;
  } else if (t->is<type::Struct>()) {
    size_t alignment_val = 1;
    for (const auto &field : t->as<type::Struct>().fields_) {
      alignment_val = std::max(alignment_val, this->alignment(field.type));
    }
    return alignment_val;
  } else if (t->is<type::Function>()) {
    return ptr_align_;
  } else if (t->is<type::Enum>()) {
    return 8;  // TODO
  } else if (t->is<type::Flags>()) {
    return 8;  // TODO
  } else if (t->is<type::Scope>()) {
    return 1;
  } else if (t->is<type::Variant>()) {
    size_t alignment_val = this->alignment(type::Type_);
    for (const type::Type *type : t->as<type::Variant>().variants_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else {
    NOT_YET();
  }
  UNREACHABLE();
}

size_t Architecture::bytes(const type::Type *t) const {
  if (t->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Generic: NOT_YET();
      case type::PrimType::Module: NOT_YET();
      case type::PrimType::Err: NOT_YET();
      case type::PrimType::Block: NOT_YET();
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Char: return 1;
      case type::PrimType::Int:
      case type::PrimType::Real:
      case type::PrimType::Type:
      case type::PrimType::NullPtr:
      case type::PrimType::Code: return 8;
      case type::PrimType::String: return 8;
    }
  } else if (t->is<type::Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<type::Array>()) {
    auto *array_type = &t->as<type::Array>();
    if (array_type->fixed_length) {
      return array_type->len *
             MoveForwardToAlignment(array_type->data_type,
                                    bytes(array_type->data_type));
    } else {
      return 2 * ptr_bytes_;
    }
  } else if (t->is<type::Struct>()) {
    auto *struct_type = &t->as<type::Struct>();
    size_t num_bytes  = 0;
    for (const auto &field : struct_type->fields_) {
      num_bytes += this->bytes(field.type);
      num_bytes = this->MoveForwardToAlignment(field.type, num_bytes);
    }

    return MoveForwardToAlignment(struct_type, num_bytes);
  } else if (t->is<type::Function>()) {
    return 2 * ptr_bytes_;
  } else if (t->is<type::Enum>()) {
    return 8; // TODO
  } else if (t->is<type::Flags>()) {
    return 8; // TODO
  } else if (t->is<type::Scope>()) {
    return 0;
  } else if (t->is<type::Variant>()) {
    size_t num_bytes = 0;
    for (const type::Type* type : t->as<type::Variant>().variants_) {
      num_bytes = std::max(num_bytes, this->bytes(type));
    }
    return num_bytes + ptr_bytes_;
  } else {
    NOT_YET();
  }
  UNREACHABLE();
}
