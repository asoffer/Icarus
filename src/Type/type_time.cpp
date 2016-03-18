#include "Type.h"

Time::Eval Primitive::time() const {
  // Type::compile_time has a value of 1, and either_time has a value of 0
  // so we can use the casts bool -> int -> Time::eval
  return static_cast<Time::Eval>(this == Type_);
}

Time::Eval Array::time() const {
  // has_dynamic_length() will either be 0 or 2.
  // As a Time::Eval object, that's either "either_time"
  // or "run_time" respectively.
  return data_type.get->time() & Time::run;
}

Time::Eval Function::time() const {
  return input.get->time() | output.get->time();
}

Time::Eval Pointer::time() const {
  // It's not allowed to be a pointer to a compile-time type,
  // but we need not check this here. This will be checked by
  // verify_types().
  return Time::run;
}

Time::Eval Tuple::time() const {
  Time::Eval output = Time::either;
  for (auto t : entries) {
    output |= t.get->time();
  }

  return output;
}

Time::Eval Structure::time() const {
  // TODO
  return Time::run;
}


Time::Eval Enumeration::time() const {
  // TODO
  return Time::run;
}

Time::Eval DependentType::time() const { return Time::compile; }
Time::Eval TypeVariable::time() const { return Time::compile; }
Time::Eval ForwardDeclaration::time() const { return Time::compile; }
