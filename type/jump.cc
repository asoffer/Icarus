#include "type/jump.h"

#include "absl/container/node_hash_set.h"
#include "absl/strings/str_join.h"
#include "base/guarded.h"

namespace type {

static base::guarded<absl::node_hash_set<Jump>> jmps_;
Jump const *Jmp(std::vector<Type const *> const &args) {
  return &*jmps_.lock()->emplace(args).first;
}

void Jump::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  for (auto const *arg : args_) { arg->defining_modules(modules); }
}

void Jump::WriteTo(std::string *r) const {
  absl::StrAppend(
      r, "jump(",
      absl::StrJoin(args_, ", ",
                    [](std::string *out, Type const *t) { t->WriteTo(out); }),
      ")");
}

core::Bytes Jump::bytes(core::Arch const &) const {
  return core::Host().ptr_bytes;
}

core::Alignment Jump::alignment(core::Arch const &) const {
  return core::Host().ptr_alignment;
}

}  // namespace type
