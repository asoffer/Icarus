#include "Type.h"

bool Primitive::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  if (llvm_type_ == nullptr) return false;
  if (this != Void) llvm_in.push_back(*this);
  return true;
}

bool Array::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  llvm_in.push_back(*this);
  return true;
}

bool Tuple::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  // TODO expand the parameters in place, probably.
  // If I do this, then there's no need to condition on a tuple
  // in the Function ctor
  return false;
}

bool Pointer::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  llvm_in.push_back(*this);
  return true;
}

bool Function::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  // TODO
  return false;
}

bool Enum::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  llvm_in.push_back(*this);
  return true;
}

bool UserDefined::add_llvm_input(std::vector<llvm::Type*>& llvm_in) {
  llvm_in.push_back(*Ptr(this));
  return true;
}
