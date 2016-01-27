#include "Type.h"

Type* Error;
Type* Unknown;
Type* Bool;
Type* Char;
Type* Int;
Type* Real;
Type* Type_;
Type* Uint;
Type* Void;
Type* RawPtr;

namespace TypeSystem {
  std::map<std::string, Type*> Literals;
}  // namespace TypeSystem

Type* Ptr(Type* t) {
  for (const auto& ptr : Pointer::pointer_types_) {
    if (ptr->pointee_type_ == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  Pointer::pointer_types_.push_back(ptr_type);
  return ptr_type;
}
