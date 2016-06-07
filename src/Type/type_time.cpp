#ifndef ICARUS_UNITY
#include "Type.h"
#endif

Time::Eval Primitive::time() const {
  // Type::compile_time has a value of 1, and either_time has a value of 0
  // so we can use the casts bool -> int -> Time::eval
  return static_cast<Time::Eval>(this == Type_);
}

Time::Eval Array::time() const { return data_type->time(); }
Time::Eval Function::time() const { return input->time() | output->time(); }
Time::Eval Pointer::time() const { return pointee->time(); }

Time::Eval Tuple::time() const {
  Time::Eval output = Time::either;
  for (auto t : entries) { output |= t->time(); }

  return output;
}

Time::Eval Structure::time() const {
  return has_vars ? Time::compile : Time::run;
}

Time::Eval Enumeration::time() const { return Time::either; }
Time::Eval RangeType::time() const { return end_type->time(); }
Time::Eval SliceType::time() const { return array_type->time(); }
Time::Eval TypeVariable::time() const { return Time::compile; }
Time::Eval ParametricStructure::time() const { return Time::compile; }
