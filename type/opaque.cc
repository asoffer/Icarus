#include "type/opaque.h"

#include "base/debug.h"

namespace type {
void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

core::Bytes Opaque::bytes(core::Arch const &a) const {
  UNREACHABLE("Must not request the size of an opaque type");
}

core::Alignment Opaque::alignment(core::Arch const &a) const {
  UNREACHABLE("Must not request the alignment of an opaque type");
}

}  // namespace type
