#include "ir/cmd.h"
#include "type/primitive.h"

struct Module;
struct Context;

namespace type {
void Primitive::EmitInit(ir::Register id_reg, Context *) const {
  switch (type_) {
    case PrimType::Type_: ir::Store(type::Void(), id_reg); break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(false, id_reg); break;
    case PrimType::Int8: ir::Store(static_cast<int8_t>(0), id_reg); break;
    case PrimType::Int16: ir::Store(static_cast<int16_t>(0), id_reg); break;
    case PrimType::Int32: ir::Store(static_cast<int32_t>(0), id_reg); break;
    case PrimType::Int64: ir::Store(static_cast<int64_t>(0), id_reg); break;
    case PrimType::Nat8: ir::Store(static_cast<uint8_t>(0), id_reg); break;
    case PrimType::Nat16: ir::Store(static_cast<uint16_t>(0), id_reg); break;
    case PrimType::Nat32: ir::Store(static_cast<uint32_t>(0), id_reg); break;
    case PrimType::Nat64: ir::Store(static_cast<uint64_t>(0), id_reg); break;
    case PrimType::Float32: ir::Store(0.0f, id_reg); break;
    case PrimType::Float64: ir::Store(0.0, id_reg); break;
    default: UNREACHABLE();
  }
}

void Primitive::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                               ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  switch (this->type_) {
    case PrimType::Type_:
      ir::Store(from.reg_or<type::Type const *>(), to);
      break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(from.reg_or<bool>(), to); break;
    case PrimType::Int8: ir::Store(from.reg_or<int8_t>(), to); break;
    case PrimType::Int16: ir::Store(from.reg_or<int16_t>(), to); break;
    case PrimType::Int32: ir::Store(from.reg_or<int32_t>(), to); break;
    case PrimType::Int64: ir::Store(from.reg_or<int64_t>(), to); break;
    case PrimType::Nat8: ir::Store(from.reg_or<uint8_t>(), to); break;
    case PrimType::Nat16: ir::Store(from.reg_or<uint16_t>(), to); break;
    case PrimType::Nat32: ir::Store(from.reg_or<uint32_t>(), to); break;
    case PrimType::Nat64: ir::Store(from.reg_or<uint64_t>(), to); break;
    case PrimType::Float32: ir::Store(from.reg_or<float>(), to); break;
    case PrimType::Float64: ir::Store(from.reg_or<double>(), to); break;
    default: UNREACHABLE();
  }
}

void Primitive::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                               ir::RegisterOr<ir::Addr> to,
                               Context *ctx) const {
  EmitCopyAssign(from_type, from, to, ctx);
}

void Primitive::defining_modules(
    std::unordered_set<::Module const *> *modules) const {}

void Primitive::EmitRepr(ir::Val const &val, Context *) const {
  switch (type_) {
    case PrimType::Bool: ir::Print(val.reg_or<bool>()); break;
    case PrimType::Int8: ir::Print(val.reg_or<int8_t>()); break;
    case PrimType::Int16: ir::Print(val.reg_or<int16_t>()); break;
    case PrimType::Int32: ir::Print(val.reg_or<int32_t>()); break;
    case PrimType::Int64: ir::Print(val.reg_or<int64_t>()); break;
    case PrimType::Nat8: ir::Print(val.reg_or<uint8_t>()); break;
    case PrimType::Nat16: ir::Print(val.reg_or<uint16_t>()); break;
    case PrimType::Nat32: ir::Print(val.reg_or<uint32_t>()); break;
    case PrimType::Nat64: ir::Print(val.reg_or<uint64_t>()); break;
    case PrimType::Float32: ir::Print(val.reg_or<float>()); break;
    case PrimType::Float64: ir::Print(val.reg_or<double>()); break;
    case PrimType::Type_: ir::Print(val.reg_or<Type const *>()); break;
    case PrimType::Ctx:
    case PrimType::Scope:
    case PrimType::StatefulScope:
    case PrimType::NullPtr:
    case PrimType::EmptyArray:
    case PrimType::Module:
    case PrimType::Block:
    case PrimType::OptBlock:
    case PrimType::RepBlock: UNREACHABLE();
    case PrimType::Intf: ir::Print(val.reg_or<Interface const *>()); break;
    case PrimType::ByteView: ir::Print(val.reg_or<std::string_view>()); break;
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

ir::Val Primitive::PrepareArgument(Type const *from, ir::Val const &val,
                                   Context *ctx) const {
  if (from->is<Variant>()) {
    return ir::Val::Reg(
        ir::Load(ir::VariantValue(this, std::get<ir::Register>(val.value)),
                 this),
        this);
  } else {
    ASSERT(from == this);
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

}  // namespace type
