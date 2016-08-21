#ifndef ICARUS_UNITY
#include "Type.h"
#endif

Time::Eval Primitive::time() {
  // Type::compile_time has a value of 1, and either_time has a value of 0
  // so we can use the casts bool -> int -> Time::eval
  return (Time::Eval)(this == Type_);
}

Time::Eval Array::time() { return data_type->time(); }
Time::Eval Function::time() { return input->time() | output->time(); }
Time::Eval Pointer::time() { return pointee->time(); }

Time::Eval Tuple::time() {
  Time::Eval output = Time::either;
  for (auto t : entries) { output |= t->time(); }

  return output;
}

Time::Eval Struct::time() {
  return has_vars() ? Time::compile : Time::run;
}

Time::Eval Enum::time() { return Time::either; }
Time::Eval RangeType::time() { return end_type->time(); }
Time::Eval SliceType::time() { return array_type->time(); }
Time::Eval TypeVariable::time() { return Time::compile; }
Time::Eval ParamStruct::time() { return Time::compile; }
Time::Eval Scope_Type::time() { return Time::compile; }
