#include "ir/cmd.h"
#include "type/primitive.h"

struct Module;
struct Context;

namespace type {

void Primitive::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {}

void Primitive::EmitRepr(ir::Results const &val, Context *) const {
  switch (type_) {
    case PrimType::Bool: ir::Print(val.get<bool>(0)); break;
    case PrimType::Int8: ir::Print(val.get<int8_t>(0)); break;
    case PrimType::Int16: ir::Print(val.get<int16_t>(0)); break;
    case PrimType::Int32: ir::Print(val.get<int32_t>(0)); break;
    case PrimType::Int64: ir::Print(val.get<int64_t>(0)); break;
    case PrimType::Nat8: ir::Print(val.get<uint8_t>(0)); break;
    case PrimType::Nat16: ir::Print(val.get<uint16_t>(0)); break;
    case PrimType::Nat32: ir::Print(val.get<uint32_t>(0)); break;
    case PrimType::Nat64: ir::Print(val.get<uint64_t>(0)); break;
    case PrimType::Float32: ir::Print(val.get<float>(0)); break;
    case PrimType::Float64: ir::Print(val.get<double>(0)); break;
    case PrimType::Type_: ir::Print(val.get<Type const *>(0)); break;
    case PrimType::Ctx:
    case PrimType::Scope:
    case PrimType::StatefulScope:
    case PrimType::NullPtr:
    case PrimType::EmptyArray:
    case PrimType::Module:
    case PrimType::Block:
    case PrimType::OptBlock:
    case PrimType::RepBlock: UNREACHABLE();
    case PrimType::Intf: ir::Print(val.get<Interface const *>(0)); break;
    case PrimType::ByteView: ir::Print(val.get<std::string_view>(0)); break;
  }
}

void Primitive::WriteTo(std::string *result) const {
  switch (type_) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case PrimType::EnumName:                                                     \
    result->append(name);                                                      \
    return;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
    default: UNREACHABLE();
  }
}

ir::Results Primitive::PrepareArgument(Type const *from, ir::Results const &val,
                                       Context *ctx) const {
  if (from->is<Variant>()) {
    return ir::Results{
        ir::Load(ir::VariantValue(this, val.get<ir::Reg>(0)), this)};
  } else {
    ASSERT(this->to_string() == from->to_string());
    return val;
  }
}

Cmp Primitive::Comparator() const {
  if (type_ == PrimType::ByteView) { return Cmp::None; }
  // TODO is this right for floating-point type?
  return (type_ == PrimType::Int8 || type_ == PrimType::Int16 ||
          type_ == PrimType::Int32 || type_ == PrimType::Int64 ||
          type_ == PrimType::Nat8 || type_ == PrimType::Nat16 ||
          type_ == PrimType::Nat32 || type_ == PrimType::Nat64 ||
          type_ == PrimType::Float32 || type_ == PrimType::Float64)
             ? Cmp::Order
             : Cmp::Equality;
}

bool Primitive::is_integral() const {
  switch (type_) {
    case PrimType::Int8:
    case PrimType::Int16:
    case PrimType::Int32:
    case PrimType::Int64:
    case PrimType::Nat8:
    case PrimType::Nat16:
    case PrimType::Nat32:
    case PrimType::Nat64: return true;
    default: return false;
  }
}

core::Bytes Primitive::bytes(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case PrimType::Type_: return core::Host().ptr_bytes;
    case PrimType::NullPtr: return a.ptr_bytes;
    case PrimType::EmptyArray: return core::Bytes{0};
    case PrimType::Bool: return core::Bytes{1};
    case PrimType::Int8: return core::Bytes{1};
    case PrimType::Int16: return core::Bytes{2};
    case PrimType::Int32: return core::Bytes{4};
    case PrimType::Int64: return core::Bytes{8};
    case PrimType::Nat8: return core::Bytes{1};
    case PrimType::Nat16: return core::Bytes{2};
    case PrimType::Nat32: return core::Bytes{4};
    case PrimType::Nat64: return core::Bytes{8};
    case PrimType::Float32: return core::Bytes{4};
    case PrimType::Float64: return core::Bytes{8};
    case PrimType::Module: return core::Host().ptr_bytes;
    case PrimType::Scope: return core::Host().ptr_bytes;
    case PrimType::Block: return core::Host().ptr_bytes;
    case PrimType::OptBlock: return core::Host().ptr_bytes;
    case PrimType::RepBlock: return core::Host().ptr_bytes;
    case PrimType::ByteView:
      // TODO generalize to other architectures.
      return core::Bytes{sizeof(std::string_view)};
    default: UNREACHABLE(to_string());
  }
}

core::Alignment Primitive::alignment(core::Arch const &a) const {
  switch (type_) {
    // Types are stored as pointers on the host and integers on the target
    // machine that are as wide as host pointers.
    case PrimType::Type_: return core::Host().ptr_alignment;
    case PrimType::NullPtr: return a.ptr_alignment;
    case PrimType::EmptyArray: return core::Alignment{1};
    case PrimType::Bool: return core::Alignment{1};
    case PrimType::Int8: return core::Alignment{1};
    case PrimType::Int16: return core::Alignment{2};
    case PrimType::Int32: return core::Alignment{4};
    case PrimType::Int64: return core::Alignment{8};
    case PrimType::Nat8: return core::Alignment{1};
    case PrimType::Nat16: return core::Alignment{2};
    case PrimType::Nat32: return core::Alignment{4};
    case PrimType::Nat64: return core::Alignment{8};
    case PrimType::Float32: return core::Alignment{4};
    case PrimType::Float64: return core::Alignment{8};
    case PrimType::Module: return core::Host().ptr_alignment;
    case PrimType::Scope: return core::Host().ptr_alignment;
    case PrimType::Block: return core::Host().ptr_alignment;
    case PrimType::OptBlock: return core::Host().ptr_alignment;
    case PrimType::RepBlock: return core::Host().ptr_alignment;
    case PrimType::ByteView:
      // TODO generalize to other architectures.
      return core::Alignment{alignof(std::string_view)};
    default: UNREACHABLE(to_string());
  }
}

bool Primitive::ReinterpretAs(Type const *t) const {
  return t == this || (type_ == PrimType::NullPtr && t->is<Pointer>()) ||
         (type_ == PrimType::EmptyArray && t->is<Array>());
}

bool Primitive::TestEquality(void const *lhs, void const *rhs) const {
  switch (type_) {
    case PrimType::Int64:
      return *reinterpret_cast<int64_t const *>(lhs) ==
             *reinterpret_cast<int64_t const *>(rhs);
    default: UNREACHABLE();
  }
}

}  // namespace type
