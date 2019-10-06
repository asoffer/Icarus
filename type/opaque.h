#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "type/type.h"

struct Module;

namespace type {
struct Opaque : public Type {
  Opaque(module::Module const *mod) : mod_(mod) {}
  ~Opaque() override {}

#include ICARUS_TYPE_VISITOR_METHODS

  void WriteTo(std::string *result) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void defining_modules(
      absl::flat_hash_set<module::Module const *> *modules) const override;

  module::Module const *mod_;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
