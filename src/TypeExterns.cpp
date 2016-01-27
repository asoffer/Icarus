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

Type* Ptr(Type* t) {
  return Type::get_pointer(t);
}
