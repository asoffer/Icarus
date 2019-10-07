#include "type/opaque.h"

#include "base/debug.h"

namespace type {
void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

core::Bytes Opaque::bytes(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

core::Alignment Opaque::alignment(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

}  // namespace type
