#include "architecture.h"

#include "context.h"
#include "ir/cmd.h"
#include "type/all.h"

// TODO a lot of stuff that can only be run at compile-time needs to have
// values for size and alignment. Figure out the best way to handle this.

IR::RegisterOr<i32> Architecture::ComputeArrayLength(
    IR::RegisterOr<i32> len, const type::Type *t) const {
  return IR::MulInt(len, static_cast<i32>(MoveForwardToAlignment(t, bytes(t))));
}

size_t Architecture::alignment(const type::Type *t) const {
  if (ASSERT_NOT_NULL(t)->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Generic: return 8; // TODO ???
      case type::PrimType::Module: return 8; // TODO ???
      case type::PrimType::Err: NOT_YET();
      case type::PrimType::Block: return 8; // TODO ???
      case type::PrimType::OptBlock: return 8; // TODO ??
      case type::PrimType::Interface: NOT_YET();
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Char: return 1;
      case type::PrimType::Int: return 4;
      case type::PrimType::Real:
      case type::PrimType::Type:
      case type::PrimType::NullPtr:
      case type::PrimType::Code: return 8;
    }
  } else if (t->is<type::CharBuffer>()) {
    // TODO what about utf-16 or utf-32 buffers?
    return alignof(std::string_view);
    // alignment(type::Char);
  } else if (t->is<type::Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<type::Array>()) {
    auto *array_type = &t->as<type::Array>();
    return array_type->fixed_length ? this->alignment(array_type->data_type)
                                    : ptr_align_;
  } else if (t->is<type::Struct>()) {
    size_t alignment_val = 1;
    for (auto const &field : t->as<type::Struct>().fields()) {
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
    return 8;
  } else if (t->is<type::Variant>()) {
    size_t alignment_val = this->alignment(type::Type_);
    for (const type::Type *type : t->as<type::Variant>().variants_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else if (t->is<type::Tuple>()) {
    size_t alignment_val = 1;
    for (const type::Type *type : t->as<type::Tuple>().entries_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else {
    NOT_YET(t->to_string());
  }
  UNREACHABLE();
}

size_t Architecture::bytes(const type::Type *t) const {
  if (ASSERT_NOT_NULL(t)->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Generic: return 8; // TODO ???
      case type::PrimType::Module: return 8; // TODO ???
      case type::PrimType::Err: NOT_YET();
      case type::PrimType::Block: return 8; // TODO ???
      case type::PrimType::OptBlock: return 8; // TODO ??
      case type::PrimType::Interface: NOT_YET();
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Char: return 1;
      case type::PrimType::Int: return 4;
      case type::PrimType::Real:
      case type::PrimType::Type:
      case type::PrimType::NullPtr:
      case type::PrimType::Code: return 8;
    }
  } else if (t->is<type::CharBuffer>()) {
    return sizeof(std::string_view); // TODO fix me t->as<type::CharBuffer>().length_;
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
    for (auto const &field : struct_type->fields()) {
      num_bytes += this->bytes(field.type);
      num_bytes = this->MoveForwardToAlignment(field.type, num_bytes);
    }

    return MoveForwardToAlignment(struct_type, num_bytes);
  } else if (t->is<type::Tuple>()) {
    auto *tuple_type = &t->as<type::Tuple>();
    size_t num_bytes = 0;
    for (const auto &entry_type : tuple_type->entries_) {
      num_bytes += this->bytes(entry_type);
      num_bytes = this->MoveForwardToAlignment(entry_type, num_bytes);
    }

    return MoveForwardToAlignment(t, num_bytes);
  } else if (t->is<type::Function>()) {
    return sizeof(IR::AnyFunc);
    // TODO it's weird that this is 8 and not ptr_bytes_ which may be larger. On
    // the interpretting machinge, it seems like we aren't just returning a
    // pointer type but sometimes an actual IR::Func* which is smaller.
  } else if (t->is<type::Enum>()) {
    return 8; // TODO
  } else if (t->is<type::Flags>()) {
    return 8; // TODO
  } else if (t->is<type::Scope>()) {
    return 8;
  } else if (t->is<type::Variant>()) {
    size_t num_bytes = 0;
    for (const type::Type* type : t->as<type::Variant>().variants_) {
      num_bytes = std::max(num_bytes, this->bytes(type));
    }
    return num_bytes + ptr_bytes_;
  } else {
    NOT_YET(t);
  }
  UNREACHABLE();
}
