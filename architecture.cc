#include "architecture.h"

#include "context.h"
#include "ir/cmd.h"
#include "type/all.h"

ir::RegisterOr<i32> Architecture::ComputeArrayLength(
    ir::RegisterOr<i32> len, type::Type const *t) const {
  return ir::Mul(len, static_cast<i32>(MoveForwardToAlignment(t, bytes(t))));
}

size_t Architecture::alignment(type::Type const *t) const {
  if (ASSERT_NOT_NULL(t)->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Module: return local_ptr_align_;
      case type::PrimType::Block: return local_ptr_align_;
      case type::PrimType::OptBlock: return local_ptr_align_;
      case type::PrimType::RepBlock: return local_ptr_align_;
      case type::PrimType::Intf: return local_ptr_align_;
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Int8: return 1;
      case type::PrimType::Int16: return 2;
      case type::PrimType::Int32: return 4;
      case type::PrimType::Int64: return 8;
      case type::PrimType::Nat8: return 1;
      case type::PrimType::Nat16: return 2;
      case type::PrimType::Nat32: return 4;
      case type::PrimType::Nat64: return 8;
      case type::PrimType::Float32: return 4; // TODO can the alignment ever be different?
      case type::PrimType::Float64: return 8;
      case type::PrimType::Type_:
      case type::PrimType::NullPtr:
      case type::PrimType::Scope:
      case type::PrimType::StatefulScope: return 8;
      case type::PrimType::ByteView: return alignof(std::string_view);
    }
    UNREACHABLE(t);
  } else if (t->is<type::GenericStruct>()) {
    return local_ptr_align_;
  } else if (t->is<type::Pointer>()) {
    return ptr_bytes_;
  } else if (t->is<type::Array>()) {
    auto *array_type = &t->as<type::Array>();
    return this->alignment(array_type->data_type);
  } else if (auto *s = t->if_as<type::Struct>()) {
    size_t alignment_val = 1;
    for (auto const &field : s->fields()) {
      alignment_val = std::max(alignment_val, this->alignment(field.type));
    }
    return alignment_val;
  } else if (t->is<type::Callable>()) {
    return ptr_align_;
  } else if (t->is<type::Enum>()) {
    return 8;  // TODO
  } else if (t->is<type::Flags>()) {
    return 8;  // TODO
  } else if (auto *v = t->if_as<type::Variant>()) {
    size_t alignment_val = this->alignment(type::Type_);
    for (type::Type const *type : v->variants_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else if (auto *tup = t->if_as<type::Tuple>()) {
    size_t alignment_val = 1;
    for (type::Type const *type : tup->entries_) {
      alignment_val = std::max(alignment_val, this->alignment(type));
    }
    return alignment_val;
  } else if (auto *intf = t->if_as<type::Interface>()) {
    return ptr_align_; // TODO is this right?
  } else {
    UNREACHABLE(t);
  }
}

size_t Architecture::bytes(type::Type const *t) const {
  if (ASSERT_NOT_NULL(t)->is<type::Primitive>()) {
    switch (t->as<type::Primitive>().type_) {
      case type::PrimType::Module: return local_ptr_bytes_;
      case type::PrimType::Block: return local_ptr_bytes_;
      case type::PrimType::OptBlock: return local_ptr_bytes_;
      case type::PrimType::RepBlock: return local_ptr_bytes_;
      case type::PrimType::Intf: return local_ptr_bytes_;
      case type::PrimType::EmptyArray:
      case type::PrimType::Bool:
      case type::PrimType::Int8: return 1;
      case type::PrimType::Int16: return 2;
      case type::PrimType::Int32: return 4;
      case type::PrimType::Int64: return 8;
      case type::PrimType::Nat8: return 1;
      case type::PrimType::Nat16: return 2;
      case type::PrimType::Nat32: return 4;
      case type::PrimType::Nat64: return 8;
      case type::PrimType::Float32: return 4;
      case type::PrimType::Float64: return 8;
      case type::PrimType::Type_:
      case type::PrimType::NullPtr:
      case type::PrimType::Scope:
      case type::PrimType::StatefulScope: return 8;
      case type::PrimType::ByteView: return sizeof(std::string_view);
    }

    UNREACHABLE(t);
  } else if (t->is<type::GenericStruct>()) {
    return local_ptr_bytes_;
  } else if (t->is<type::Pointer>()) {
    return ptr_bytes_;
  } else if (auto *array_type = t->if_as<type::Array>()) {
    // TODO skip the last alignment requirement?
    return array_type->len *
           MoveForwardToAlignment(array_type->data_type,
                                  bytes(array_type->data_type));
  } else if (auto *struct_type = t->if_as<type::Struct>()) {
    size_t num_bytes  = 0;
    for (auto const &field : struct_type->fields()) {
      num_bytes += this->bytes(field.type);
      num_bytes = this->MoveForwardToAlignment(field.type, num_bytes);
    }

    return MoveForwardToAlignment(struct_type, num_bytes);
  } else if (auto *tuple_type = t->if_as<type::Tuple>()) {
    size_t num_bytes = 0;
    for (auto const &entry_type : tuple_type->entries_) {
      num_bytes += this->bytes(entry_type);
      num_bytes = this->MoveForwardToAlignment(entry_type, num_bytes);
    }

    return MoveForwardToAlignment(t, num_bytes);
  } else if (t->is<type::Callable>()) {
    return sizeof(ir::AnyFunc);
    // TODO it's weird that this is 8 and not ptr_bytes_ which may be larger. On
    // the interpretting machinge, it seems like we aren't just returning a
    // pointer type but sometimes an actual ir::Func* which is smaller.
  } else if (t->is<type::Enum>()) {
    return 8;  // TODO
  } else if (t->is<type::Flags>()) {
    return 8;  // TODO
  } else if (auto *v = t->if_as<type::Variant>()) {
    size_t num_bytes = 0;
    for (type::Type const *type : v->variants_) {
      num_bytes = std::max(num_bytes, this->bytes(type));
    }
    return num_bytes + ptr_bytes_;
  } else if (auto *intf = t->if_as<type::Interface>()) {
    return ptr_bytes_; // TODO is this right
  } else {
    UNREACHABLE(t);
  }
}
