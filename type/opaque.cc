#include "type/opaque.h"

#include "base/debug.h"

namespace type {
void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

ir::Results Opaque::PrepareArgument(const Type *t, const ir::Results &val,
                                    Context *ctx) const {
  UNREACHABLE();
}

void Opaque::EmitRepr(ir::Results const &id_val, Context *ctx) const {
  UNREACHABLE();
}

void Opaque::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(mod_);
}

core::Bytes Opaque::bytes(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

core::Alignment Opaque::alignment(core::Arch const &a) const {
  NOT_YET("figure out what to do here");
}

Cmp Opaque::Comparator() const { UNREACHABLE(); }

// TODO this is interesting. You could maybe be allowed to reinterpret any type
// as opaque, but we'd quickly run into problems. For example, there would be no
// checking that it's actually the correct opaque type. I.e., I could
// reinterpret int16 and int32 as the same opaque type. If a library actually
// knew what that opaque type was it would use them wrong. You should only be
// allowed to do this cast if you own the opaque type? Anyway, this doesn't go
// here... Opaque::ReinterpretAs should definitely just be cheking t == this,
// but other types maybe should be allowed to reinterpret to an opaque type.
bool Opaque::ReinterpretAs(Type const *t) const { return t == this; }

}  // namespace type
