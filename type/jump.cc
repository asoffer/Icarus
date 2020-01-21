#include "type/jump.h"

#include "absl/container/node_hash_set.h"
#include "absl/strings/str_join.h"
#include "base/guarded.h"

namespace type {

static base::guarded<absl::node_hash_set<Jump>> jmps_;
Jump const *Jmp(core::FnParams<Type const *> const &args) {
  return &*jmps_.lock()->emplace(args).first;
}

void Jump::WriteTo(std::string *r) const {
  absl::StrAppend(r, "jump(",
                  absl::StrJoin(args_, ", ",
                                [](std::string *out, auto const &p) {
                                  if (not p.name.empty()) {
                                    out->append(p.name);
                                    out->append(": ");
                                  }
                                  p.value->WriteTo(out);
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
