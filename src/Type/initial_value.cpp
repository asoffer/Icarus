#ifndef ICARUS_UNITY
#include "Type.h"
#endif

IR::Value Primitive::EmitInitialValue() const {
  switch (type_) {
  case TypeEnum::Err: UNREACHABLE;
  case TypeEnum::Unknown: UNREACHABLE;
  case TypeEnum::Type: UNREACHABLE;
  case TypeEnum::Void: UNREACHABLE;
  case TypeEnum::NullPtr: UNREACHABLE;
  case TypeEnum::Bool: return IR::Value(false);
  case TypeEnum::Char: return IR::Value('\0');
  case TypeEnum::Int: return IR::Value(0l);
  case TypeEnum::Real: return IR::Value(0.0);
  case TypeEnum::Uint: return IR::Value(0ul);
  case TypeEnum::Uint16: return IR::Value((uint16_t)0);
  case TypeEnum::Uint32: return IR::Value((uint32_t)0);
  }
}

// TODO ugly const_cast
IR::Value Pointer::EmitInitialValue() const {
  return IR::Value::Null(const_cast<Pointer *>(this));
}

IR::Value Enum::EmitInitialValue() const {
  switch (BytesAndAlignment()) {
  case 1: return IR::Value('\0');
  case 2: return IR::Value((uint16_t)0);
  case 4: return IR::Value((uint32_t)0);
  case 8: return IR::Value(0ul);
  default: UNREACHABLE;
  }
}

IR::Value Array::EmitInitialValue() const { NOT_YET; }
IR::Value Tuple::EmitInitialValue() const { NOT_YET; }
IR::Value Function::EmitInitialValue() const { NOT_YET; }
IR::Value Structure::EmitInitialValue() const { NOT_YET; }
IR::Value TypeVariable::EmitInitialValue() const { NOT_YET; }
IR::Value ParametricStructure::EmitInitialValue() const { NOT_YET; }
IR::Value RangeType::EmitInitialValue() const { NOT_YET; }
IR::Value SliceType::EmitInitialValue() const { NOT_YET; }
