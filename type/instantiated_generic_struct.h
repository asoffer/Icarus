#ifndef ICARUS_TYPE_INSTANTIATED_GENERIC_STRUCT_H
#define ICARUS_TYPE_INSTANTIATED_GENERIC_STRUCT_H

#include "module/module.h"
#include "type/generic_struct.h"
#include "type/struct.h"

namespace type {

struct InstantiatedGenericStruct : Struct {
  InstantiatedGenericStruct(module::BasicModule const *mod, Options options,
                            GenericStruct const *g)
      : Struct(mod, options), generic_(ASSERT_NOT_NULL(g)) {}

  GenericStruct const &generic() const { return *generic_; }

  void set_arguments(core::Arguments<type::Typed<ir::Value>> args) {
    arguments_ = std::move(args);
  }

  core::Arguments<type::Typed<ir::Value>> const &arguments() const {
    return arguments_;
  }

 private:
  GenericStruct const *generic_;
  core::Arguments<type::Typed<ir::Value>> arguments_;
};

}  // namespace type

#endif  // ICARUS_TYPE_INSTANTIATED_GENERIC_STRUCT_H
