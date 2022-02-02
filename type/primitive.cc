#include "type/primitive.h"

#include <cstring>

#include "absl/hash/hash.h"

namespace type {

void Primitive::WriteTo(std::string *result) const {
  switch (kind_) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case Kind::EnumName:                                                         \
    result->append(name);                                                      \
    return;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  }
  UNREACHABLE();
}

core::Bytes Primitive::bytes(core::Arch const &a) const {
  switch (kind_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case Kind::Void: return core::Bytes{0};
    case Kind::Type_: return core::Bytes::Get<Type>();
    case Kind::NullPtr: return a.pointer().bytes();
    case Kind::EmptyArray: return core::Bytes{0};
    case Kind::Bool: return core::Bytes{1};
    case Kind::Char: return core::Bytes{1};
    case Kind::Integer: return core::Bytes{8};  // TODO: Does this matter?
    case Kind::I8: return core::Bytes{1};
    case Kind::I16: return core::Bytes{2};
    case Kind::I32: return core::Bytes{4};
    case Kind::I64: return core::Bytes{8};
    case Kind::U8: return core::Bytes{1};
    case Kind::U16: return core::Bytes{2};
    case Kind::U32: return core::Bytes{4};
    case Kind::U64: return core::Bytes{8};
    case Kind::F32: return core::Bytes{4};
    case Kind::F64: return core::Bytes{8};
    case Kind::Byte: return core::Bytes{1};
    case Kind::Module: return core::Host.pointer().bytes();
    case Kind::Label: return core::Host.pointer().bytes();
    case Kind::Interface: return core::Host.pointer().bytes();
    case Kind::ScopeContext: return core::Host.pointer().bytes();
    case Kind::UnboundScope: return core::Host.pointer().bytes();
    default:;
  }
  UNREACHABLE(to_string());
}

core::Alignment Primitive::alignment(core::Arch const &a) const {
  switch (kind_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case Kind::Void: return core::Alignment{0};
    case Kind::Type_: return core::Alignment::Get<Type>();
    case Kind::NullPtr: return a.pointer().alignment();
    case Kind::EmptyArray: return core::Alignment{1};
    case Kind::Bool: return core::Alignment{1};
    case Kind::Char: return core::Alignment{1};
    case Kind::Integer: return core::Alignment{8};  // TODO: Does this matter?
    case Kind::I8: return core::Alignment{1};
    case Kind::I16: return core::Alignment{2};
    case Kind::I32: return core::Alignment{4};
    case Kind::I64: return core::Alignment{8};
    case Kind::U8: return core::Alignment{1};
    case Kind::U16: return core::Alignment{2};
    case Kind::U32: return core::Alignment{4};
    case Kind::U64: return core::Alignment{8};
    case Kind::F32: return core::Alignment{4};
    case Kind::F64: return core::Alignment{8};
    case Kind::Byte: return core::Alignment{1};
    case Kind::Module: return core::Host.pointer().alignment();
    case Kind::Label: return core::Host.pointer().alignment();
    case Kind::Interface: return core::Host.pointer().alignment();
    case Kind::ScopeContext: return core::Host.pointer().alignment();
    case Kind::UnboundScope: return core::Host.pointer().alignment();
    default:;
  }
  UNREACHABLE(to_string());
}

bool Primitive::EqualsValue(ir::CompleteResultRef const &lhs,
                            ir::CompleteResultRef const &rhs) const {
  base::untyped_buffer_view lhs_view = lhs.raw();
  base::untyped_buffer_view rhs_view = rhs.raw();
  if (lhs_view.size() != rhs_view.size()) { return false; }
  return std::memcmp(lhs_view.data(), lhs_view.data(), lhs_view.size()) == 0;
}

size_t Primitive::HashValue(ir::CompleteResultRef const &value) const {
  return absl::Hash<absl::Span<std::byte const>>()(
      absl::Span<std::byte const>(value.raw().data(), value.raw().size()));
}

void Primitive::ShowValue(std::ostream &os,
                          ir::CompleteResultRef const &value) const {
  Apply([&]<typename T>() {
    if constexpr (base::meta<T> == base::meta<bool>) {
      os << (value.get<T>() ? "true" : "false");
    } else if constexpr (requires { os << std::declval<T>(); }) {
      os << value.get<T>();
    } else {
      NOT_YET();
    }
  });
}

}  // namespace type
