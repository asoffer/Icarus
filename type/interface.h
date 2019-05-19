#ifndef ICARUS_TYPE_INTERFACE_H
#define ICARUS_TYPE_INTERFACE_H

#include "core/scope.h"
#include "type/type.h"

namespace type {
struct Interface : public Type {
  Interface() = delete;
  ~Interface() {}
  Interface(core::Scope const *scope, ::Module const *mod)
      : scope_(scope), mod_(mod) {}

#include "visitor/type_visitors.xmacro.h"

  void WriteTo(std::string *result) const override;

  bool matches(Type const *t) const;

  bool ReinterpretAs(Type const *t) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

   Cmp Comparator() const override { UNREACHABLE(); }

  ::Module const *defining_module() const { return mod_; }

  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

  core::Scope const *scope_ = nullptr;
  ::Module const *mod_  = nullptr;
};

}  // namespace type

#endif  // ICARUS_TYPE_INTERFACE_H
