#include "Type.h"

bool Primitive::has_variables() const { return false; }
bool Array::has_variables() const { return type_->has_variables(); }
bool Pointer::has_variables() const { return pointee_type_->has_variables(); }

bool Tuple::has_variables() const {
  for (const auto& entry : entry_types_) {
    if (entry->has_variables()) return true;
  }
  return false;
}

bool Function::has_variables() const {
  return input_type_->has_variables() || output_type_->has_variables();
}

bool Enumeration::has_variables() const { return false; }

bool Structure::has_variables() const { 
  for (const auto& field : fields_) {
    if (field.second->has_variables()) return true;
  }
  return false;
}

bool DependentType::has_variables() const { return false; }
bool TypeVariable::has_variables() const { return true; }

