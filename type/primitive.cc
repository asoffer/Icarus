#include "type/primitive.h"

#include "ir/value/string.h"

namespace type {

void Primitive::WriteTo(std::string *result) const {
  switch (type_) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case BasicType::EnumName:                                                    \
    result->append(name);                                                      \
    return;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  }
  UNREACHABLE();
}

bool Primitive::is_integral() const {
  switch (type_) {
    case BasicType::I8:
    case BasicType::I16:
    case BasicType::I32:
    case BasicType::I64:
    case BasicType::U8:
    case BasicType::U16:
    case BasicType::U32:
    case BasicType::U64: return true;
    default: return false;
  }
}

core::Bytes Primitive::bytes(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case BasicType::Void: return core::Bytes{0};
    case BasicType::Type_: return core::Bytes::Get<Type>();
    case BasicType::NullPtr: return a.pointer().bytes();
    case BasicType::EmptyArray: return core::Bytes{0};
    case BasicType::Bool: return core::Bytes{1};
    case BasicType::I8: return core::Bytes{1};
    case BasicType::I16: return core::Bytes{2};
    case BasicType::I32: return core::Bytes{4};
    case BasicType::I64: return core::Bytes{8};
    case BasicType::U8: return core::Bytes{1};
    case BasicType::U16: return core::Bytes{2};
    case BasicType::U32: return core::Bytes{4};
    case BasicType::U64: return core::Bytes{8};
    case BasicType::F32: return core::Bytes{4};
    case BasicType::F64: return core::Bytes{8};
    case BasicType::Module: return core::Host.pointer().bytes();
    case BasicType::Scope: return core::Host.pointer().bytes();
    case BasicType::Block: return core::Host.pointer().bytes();
    case BasicType::Label: return core::Host.pointer().bytes();
    case BasicType::ByteView:
      // TODO generalize to other architectures.
      return core::Bytes{sizeof(ir::String)};
    default:;
  }
  UNREACHABLE(to_string());
}

core::Alignment Primitive::alignment(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case BasicType::Void: return core::Alignment{0};
    case BasicType::Type_: return core::Alignment::Get<Type>();
    case BasicType::NullPtr: return a.pointer().alignment();
    case BasicType::EmptyArray: return core::Alignment{1};
    case BasicType::Bool: return core::Alignment{1};
    case BasicType::I8: return core::Alignment{1};
    case BasicType::I16: return core::Alignment{2};
    case BasicType::I32: return core::Alignment{4};
    case BasicType::I64: return core::Alignment{8};
    case BasicType::U8: return core::Alignment{1};
    case BasicType::U16: return core::Alignment{2};
    case BasicType::U32: return core::Alignment{4};
    case BasicType::U64: return core::Alignment{8};
    case BasicType::F32: return core::Alignment{4};
    case BasicType::F64: return core::Alignment{8};
    case BasicType::Module: return core::Host.pointer().alignment();
    case BasicType::Scope: return core::Host.pointer().alignment();
    case BasicType::Block: return core::Host.pointer().alignment();
    case BasicType::Label: return core::Host.pointer().alignment();
    case BasicType::ByteView:
      // TODO generalize to other architectures.
      return core::Alignment{alignof(ir::String)};
    default:;
  }
  UNREACHABLE(to_string());
}

}  // namespace type
