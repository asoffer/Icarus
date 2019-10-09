#ifndef ICARUS_MODULE_EXTRACT_DEFINING_MODULES_H
#define ICARUS_MODULE_EXTRACT_DEFINING_MODULES_H

#include "absl/container/flat_hash_set.h"
#include "module/module.h"
#include "type/type_fwd.h"

namespace module {

struct ExtractDefiningModules {
  static void Extract(type::Type const *,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Array const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Enum const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Flags const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Function const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::GenericFunction const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::GenericStruct const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Jump const *,
                      absl::flat_hash_set<BasicModule const *> *);

  static void Extract(type::Opaque const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::ParameterPack const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Pointer const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Struct const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Tuple const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);

  static void Extract(type::Variant const *t,
                      absl::flat_hash_set<BasicModule const *> *modules);
};

}  // namespace module

#endif  // ICARUS_MODULE_EXTRACT_ExtractDefiningModules_H
