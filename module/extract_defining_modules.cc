#include "module/extract_defining_modules.h"

#include "base/debug.h"
#include "module/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/opaque.h"
#include "type/parameter_pack.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/variant.h"

namespace module {
void ExtractDefiningModules::Extract(
    type::Type const *, absl::flat_hash_set<BasicModule const *> *modules) {}

void ExtractDefiningModules::Extract(
    type::Array const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  t->data_type->ExtractDefiningModules(modules);
}

void ExtractDefiningModules::Extract(
    type::Enum const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  modules->insert(t->mod_);
}

void ExtractDefiningModules::Extract(
    type::Flags const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  modules->insert(t->mod_);
}

void ExtractDefiningModules::Extract(
    type::Function const *t,
    absl::flat_hash_set<BasicModule const *> *modules) {
  NOT_YET();
}

void ExtractDefiningModules::Extract(
    type::GenericFunction const *t,
    absl::flat_hash_set<BasicModule const *> *modules) {
  NOT_YET();
}

void ExtractDefiningModules::Extract(
    type::GenericStruct const *t,
    absl::flat_hash_set<BasicModule const *> *modules) {
  modules->insert(t->defining_module());
}

void ExtractDefiningModules::Extract(
    type::Jump const *, absl::flat_hash_set<BasicModule const *> *) {}

void ExtractDefiningModules::Extract(
    type::Opaque const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  modules->insert(t->mod_);
}

void ExtractDefiningModules::Extract(
    type::ParameterPack const *t,
    absl::flat_hash_set<BasicModule const *> *modules) {
  t->elem->ExtractDefiningModules(modules);
}

void ExtractDefiningModules::Extract(
    type::Pointer const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  t->pointee->ExtractDefiningModules(modules);
}

void ExtractDefiningModules::Extract(
    type::Struct const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  modules->insert(t->defining_module());
}

void ExtractDefiningModules::Extract(
    type::Tuple const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  for (auto *entry : t->entries_) { entry->ExtractDefiningModules(modules); }
}

void ExtractDefiningModules::Extract(
    type::Variant const *t, absl::flat_hash_set<BasicModule const *> *modules) {
  for (auto *v : t->variants_) { v->ExtractDefiningModules(modules); }
}
}  // namespace module
