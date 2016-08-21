#ifndef ICARUS_UNITY
#include "Type.h"
#endif

IR::Value Primitive::EmitInitialValue() const {
  switch (type_) {
  case PrimType::Err: UNREACHABLE;
  case PrimType::Unknown: UNREACHABLE;
  case PrimType::Type: return IR::Value(Void);
  case PrimType::Void: UNREACHABLE;
  case PrimType::NullPtr: UNREACHABLE;
  case PrimType::Bool: return IR::Value(false);
  case PrimType::Char: return IR::Value('\0');
  case PrimType::Int: return IR::Value(0l);
  case PrimType::Real: return IR::Value(0.0);
  case PrimType::Uint: return IR::Value(0ul);
  case PrimType::Uint16: return IR::Value((uint16_t)0);
  case PrimType::Uint32: return IR::Value((uint32_t)0);
  case PrimType::String: return IR::Value(const_cast<char *>("\0"));
  }
}

// TODO ugly const_cast
IR::Value Pointer::EmitInitialValue() const {
  return IR::Value::Null(const_cast<Pointer *>(this));
}

IR::Value Function::EmitInitialValue() const {
  return IR::Value((IR::Func *)nullptr);
}

IR::Value Array::EmitInitialValue() const { NOT_YET; }
IR::Value Tuple::EmitInitialValue() const { NOT_YET; }
IR::Value Struct::EmitInitialValue() const { NOT_YET; }
IR::Value TypeVariable::EmitInitialValue() const { NOT_YET; }
IR::Value RangeType::EmitInitialValue() const { NOT_YET; }
IR::Value SliceType::EmitInitialValue() const { NOT_YET; }
IR::Value Scope_Type::EmitInitialValue() const { NOT_YET; }
