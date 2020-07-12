#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "module/module.h"
#include "type/type.h"

namespace type {
struct Opaque : public Type {
  explicit Opaque(module::BasicModule const *)
      : Type(Type::Flags{.is_default_initializable = 0,
                         .is_copyable              = 0,
                         .is_movable               = 0,
                         .has_destructor           = 0}) {}
  ~Opaque() override {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void WriteTo(std::string *result) const override {
    result->append("<opaque>");
  }

  Completeness completeness() const override { return Completeness::Incomplete; }

  core::Bytes bytes(core::Arch const &arch) const override {
    UNREACHABLE("Must not request the size of an opaque type");
  }

  core::Alignment alignment(core::Arch const &arch) const override {
    UNREACHABLE("Must not request the alignment of an opaque type");
  }

  // TODO is this right?
  bool IsDefaultInitializable() const { return false; }
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
