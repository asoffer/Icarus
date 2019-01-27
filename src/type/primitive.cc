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
    case PrimType::Int8: ir::Store(static_cast<i8>(0), id_reg); break;
    case PrimType::Int16: ir::Store(static_cast<i16>(0), id_reg); break;
    case PrimType::Int32: ir::Store(static_cast<i32>(0), id_reg); break;
    case PrimType::Int64: ir::Store(static_cast<i64>(0), id_reg); break;
    case PrimType::Nat8: ir::Store(static_cast<u8>(0), id_reg); break;
    case PrimType::Nat16: ir::Store(static_cast<u16>(0), id_reg); break;
    case PrimType::Nat32: ir::Store(static_cast<u32>(0), id_reg); break;
    case PrimType::Nat64: ir::Store(static_cast<u64>(0), id_reg); break;
    case PrimType::Float32: ir::Store(0.0f, id_reg); break;
    case PrimType::Float64: ir::Store(0.0, id_reg); break;
    default: UNREACHABLE();
  }
}

void Primitive::EmitAssign(Type const *from_type, ir::Val const &from,
                           ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  switch (this->type_) {
    case PrimType::Type_:
      ir::Store(from.reg_or<type::Type const *>(), to);
      break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(from.reg_or<bool>(), to); break;
    case PrimType::Int8: ir::Store(from.reg_or<i8>(), to); break;
    case PrimType::Int16: ir::Store(from.reg_or<i16>(), to); break;
    case PrimType::Int32: ir::Store(from.reg_or<i32>(), to); break;
    case PrimType::Int64: ir::Store(from.reg_or<i64>(), to); break;
    case PrimType::Nat8: ir::Store(from.reg_or<u8>(), to); break;
    case PrimType::Nat16: ir::Store(from.reg_or<u16>(), to); break;
    case PrimType::Nat32: ir::Store(from.reg_or<u32>(), to); break;
    case PrimType::Nat64: ir::Store(from.reg_or<u64>(), to); break;
    case PrimType::Float32: ir::Store(from.reg_or<float>(), to); break;
    case PrimType::Float64: ir::Store(from.reg_or<double>(), to); break;
    default: UNREACHABLE();
  }
}

void Primitive::defining_modules(
    std::unordered_set<::Module const *> *modules) const {}

void Primitive::EmitRepr(ir::Val const &val, Context *) const {
  switch (type_) {
    case PrimType::Bool: ir::Print(val.reg_or<bool>()); break;
    case PrimType::Int8: ir::Print(val.reg_or<i8>()); break;
    case PrimType::Int16: ir::Print(val.reg_or<i16>()); break;
    case PrimType::Int32: ir::Print(val.reg_or<i32>()); break;
    case PrimType::Int64: ir::Print(val.reg_or<i64>()); break;
    case PrimType::Nat8: ir::Print(val.reg_or<u8>()); break;
    case PrimType::Nat16: ir::Print(val.reg_or<u16>()); break;
    case PrimType::Nat32: ir::Print(val.reg_or<u32>()); break;
    case PrimType::Nat64: ir::Print(val.reg_or<u64>()); break;
    case PrimType::Float32: ir::Print(val.reg_or<float>()); break;
    case PrimType::Float64: ir::Print(val.reg_or<double>()); break;
    case PrimType::Type_: ir::Print(val.reg_or<Type const *>()); break;
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

}  // namespace type
