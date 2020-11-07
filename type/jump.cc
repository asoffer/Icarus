#include "type/jump.h"

#include "absl/container/node_hash_set.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "base/global.h"

namespace type {

static base::Global<absl::node_hash_set<Jump>> jmps;

Jump const *Jmp(Type state, core::Params<Type> const &params) {
  return &*jmps.lock()->insert(Jump(state, params)).first;
}

void Jump::WriteTo(std::string *r) const {
  if (state_) {
    absl::StrAppend(r, "jump [");
    state_.get()->WriteTo(r);
    absl::StrAppend(r, "] (");
  } else {
    absl::StrAppend(r, "jump (");
  }

  absl::StrAppend(r,
                  absl::StrJoin(params_, ", ",
                                [](std::string *out, auto const &p) {
                                  if (not p.name.empty()) {
                                    out->append(p.name);
                                    out->append(": ");
                                  }
                                  p.value.get()->WriteTo(out);
                                }),
                  ")");
}

core::Bytes Jump::bytes(core::Arch const &) const {
  return core::Host.pointer().bytes();
}

core::Alignment Jump::alignment(core::Arch const &) const {
  return core::Host.pointer().alignment();
}

}  // namespace type
