#include "Type.h"

Type* Primitive::replace(Type* pattern, Type* replacement) {
  return (pattern == this) ? replacement : this;
}

Type* Function::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_function(
        input_type_->replace(pattern, replacement),
        output_type_->replace(pattern, replacement));
}

Type* Pointer::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_pointer(pointee_type_->replace(pattern, replacement));
}

Type* Tuple::replace(Type* pattern, Type* replacement) {
  if (pattern == this) return replacement;

  auto new_vec = std::vector<Type*>(entry_types_.size(), nullptr);
  for (size_t i = 0; i < entry_types_.size(); ++i) {
    new_vec[i] = entry_types_[i]->replace(pattern, replacement);
  }

  return Type::get_tuple(new_vec);
}

Type* Array::replace(Type* pattern, Type* replacement) {
  return (pattern == this)
    ? replacement
    : Type::get_array(type_->replace(pattern, replacement));
}

Type* UserDefined::replace(Type* pattern, Type* replacement) {
  if (pattern == this) {
    return replacement;
  }

  // TODO Should we be allowed to look inside structs?
  return this;
}
