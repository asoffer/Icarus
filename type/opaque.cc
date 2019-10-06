#include "type/opaque.h"

#include "base/debug.h"

namespace type {
void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

void Opaque::defining_modules(
    absl::flat_hash_set<module::Module const *> *modules) const {
  modules->insert(mod_);
}

core::Bytes Opaque::bytes(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

core::Alignment Opaque::alignment(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

}  // namespace type
