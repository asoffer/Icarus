#ifndef ICARUS_MODULE_SYMBOL_H
#define ICARUS_MODULE_SYMBOL_H

#include "core/type_system/type.h"
#include "data_types/fn.h"

namespace module {

struct TypedFunction {
  core::Type type;
  data_types::Fn function;
};

struct Symbol {
  enum class Kind { Type, Function };
  Symbol(core::Type t) : symbol_(t) {}
  Symbol(TypedFunction f) : symbol_(f) {}

  core::Type type() const;

  template <typename T>
  T const &as() const {
    return std::get<T>(symbol_);
  }

 private:
  std::variant<core::Type, TypedFunction> symbol_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SYMBOL_H
