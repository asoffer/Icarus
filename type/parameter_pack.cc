#include "type/parameter_pack.h"

#include "absl/container/flat_hash_map.h"
#include "base/guarded.h"

namespace type {
static base::guarded<
    absl::flat_hash_map<Type const *, std::unique_ptr<ParameterPack const>>>
    packs_;
ParameterPack const *Pack(Type const *t) {
  auto handle = packs_.lock();
  auto &p     = (*handle)[t];
  if (!p) { p = std::make_unique<ParameterPack>(t); }
  return p.get();
}

void ParameterPack::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  elem->defining_modules(modules);
}

void ParameterPack::WriteTo(std::string *r) const {
  r->append("..");
  elem->WriteTo(r);
}

core::Bytes ParameterPack::bytes(core::Arch const &a) const { UNREACHABLE(); }

core::Alignment ParameterPack::alignment(core::Arch const &a) const {
  UNREACHABLE();
}

}  // namespace type
