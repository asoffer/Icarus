#include "type/primitive.h"

#include "type/array.h"
#include "type/pointer.h"

namespace type {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  Type const *EnumName = new Primitive(BasicType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

void Primitive::WriteTo(std::string *result) const {
  switch (type_) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case BasicType::EnumName:                                                     \
    result->append(name);                                                      \
    return;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
    default: UNREACHABLE();
  }
}

bool Primitive::is_integral() const {
  switch (type_) {
    case BasicType::Int8:
    case BasicType::Int16:
    case BasicType::Int32:
    case BasicType::Int64:
    case BasicType::Nat8:
    case BasicType::Nat16:
    case BasicType::Nat32:
    case BasicType::Nat64: return true;
    default: return false;
  }
}

core::Bytes Primitive::bytes(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case BasicType::Type_: return core::Host().ptr_bytes;
    case BasicType::NullPtr: return a.ptr_bytes;
    case BasicType::EmptyArray: return core::Bytes{0};
    case BasicType::Bool: return core::Bytes{1};
    case BasicType::Int8: return core::Bytes{1};
    case BasicType::Int16: return core::Bytes{2};
    case BasicType::Int32: return core::Bytes{4};
    case BasicType::Int64: return core::Bytes{8};
    case BasicType::Nat8: return core::Bytes{1};
    case BasicType::Nat16: return core::Bytes{2};
    case BasicType::Nat32: return core::Bytes{4};
    case BasicType::Nat64: return core::Bytes{8};
    case BasicType::Float32: return core::Bytes{4};
    case BasicType::Float64: return core::Bytes{8};
    case BasicType::Module: return core::Host().ptr_bytes;
    case BasicType::Scope: return core::Host().ptr_bytes;
    case BasicType::Block: return core::Host().ptr_bytes;
    case BasicType::ByteView:
      // TODO generalize to other architectures.
      return core::Bytes{sizeof(std::string_view)};
    default:;
  }
  UNREACHABLE(to_string());
}

core::Alignment Primitive::alignment(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case BasicType::Type_: return core::Host().ptr_alignment;
    case BasicType::NullPtr: return a.ptr_alignment;
    case BasicType::EmptyArray: return core::Alignment{1};
    case BasicType::Bool: return core::Alignment{1};
    case BasicType::Int8: return core::Alignment{1};
    case BasicType::Int16: return core::Alignment{2};
    case BasicType::Int32: return core::Alignment{4};
    case BasicType::Int64: return core::Alignment{8};
    case BasicType::Nat8: return core::Alignment{1};
    case BasicType::Nat16: return core::Alignment{2};
    case BasicType::Nat32: return core::Alignment{4};
    case BasicType::Nat64: return core::Alignment{8};
    case BasicType::Float32: return core::Alignment{4};
    case BasicType::Float64: return core::Alignment{8};
    case BasicType::Module: return core::Host().ptr_alignment;
    case BasicType::Scope: return core::Host().ptr_alignment;
    case BasicType::Block: return core::Host().ptr_alignment;
    case BasicType::ByteView:
      // TODO generalize to other architectures.
      return core::Alignment{alignof(std::string_view)};
    default:;
  }
  UNREACHABLE(to_string());
}

bool Primitive::TestEquality(void const *lhs, void const *rhs) const {
  switch (type_) {
    case BasicType::Int64:
      return reinterpret_cast<uintptr_t>(lhs) ==
             reinterpret_cast<uintptr_t>(rhs);
    default:;
  }
  UNREACHABLE();
}

}  // namespace type
