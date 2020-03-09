#ifndef ICARUS_TYPE_GENERIC_FUNCTION_H
#define ICARUS_TYPE_GENERIC_FUNCTION_H

#include <string>

#include "core/arch.h"
#include "ir/value/native_fn.h"
#include "type/type.h"

namespace type {

struct GenericFunction : public Type {
  explicit GenericFunction(ir::NativeFn fn) : gen_fn_(fn) {}
  ~GenericFunction() override {}
  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

 private:
  [[maybe_unused]] ir::NativeFn gen_fn_;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_FUNCTION_H
