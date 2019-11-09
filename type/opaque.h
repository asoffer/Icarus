#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "type/type.h"

namespace type {
struct Opaque : public Type {
  Opaque(module::BasicModule const *mod) : mod_(mod) {}
  ~Opaque() override {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void WriteTo(std::string *result) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  // TODO is this right?
  bool IsDefaultInitializable() const { return false; }

  module::BasicModule const *mod_;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
