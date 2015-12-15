#include "Type.h"

Type::time_loc Primitive::type_time() const {
  // Type::compile_time has a value of 1, and either_time has a value of 0
  // so we can use the casts bool -> int -> time_loc
  return static_cast<Type::time_loc>(static_cast<int>(this == Type::get_type()));
}

Type::time_loc Array::type_time() const {
  // has_dynamic_length() will either be 0 or 2.
  // As a Type::time_loc object, that's either "either_time"
  // or "run_time" respectively.
  return static_cast<Type::time_loc>(
    static_cast<int>(data_type()->type_time())
    |
    (static_cast<int>(has_dynamic_length()) << 1));
}

Type::time_loc Function::type_time() const {
  return static_cast<Type::time_loc>(
      static_cast<int>(argument_type()->type_time())
      |
      static_cast<int>(return_type()->type_time()));
}

Type::time_loc Pointer::type_time() const {
  // It's not allowed to be a pointer to a compile-time type,
  // but we need not check this here. This will be checked by
  // verify_types().
  return Type::run_time;
}

Type::time_loc Tuple::type_time() const {
  int output = Type::either_time;
  for (auto t : tuple_types_) {
    output |= t->type_time();
  }

  return static_cast<Type::time_loc>(output);
}

Type::time_loc UserDefined::type_time() const {
  // TODO
  return run_time;
}
