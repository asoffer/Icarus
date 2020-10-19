#include "type/parameter_pack.h"

#include "absl/container/flat_hash_map.h"
#include "base/global.h"

namespace type {
static base::Global<
    absl::flat_hash_map<Type, std::unique_ptr<ParameterPack const>>>
    packs_;
ParameterPack const *Pack(Type t) {
  auto handle = packs_.lock();
  auto &p     = (*handle)[t];
  if (not p) { p = std::make_unique<ParameterPack>(t); }
  return p.get();
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
