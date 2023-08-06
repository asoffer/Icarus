#ifndef ICARUS_MODULE_SYMBOL_H
#define ICARUS_MODULE_SYMBOL_H

#include "core/type_system/type.h"
#include "jasmin/value.h"
#include "module/function_id.h"
#include "nth/debug/debug.h"

namespace module {

struct TypedFunction {
  core::Type type;
  LocalFnId function;
};

struct TypedValue {
  core::Type type;
  jasmin::Value value;
};

struct Symbol {
  enum class Kind { Type, Function };

  Symbol(core::Type t) : symbol_(t) {}
  Symbol(TypedValue v) : symbol_(v) {}
  Symbol(TypedFunction f) : symbol_(f) {}

  core::Type type() const;

  template <typename T>
  T const &as() const {
    NTH_ASSERT(std::holds_alternative<T>(symbol_));
    return std::get<T>(symbol_);
  }

 private:
  std::variant<core::Type, TypedFunction, TypedValue> symbol_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SYMBOL_H
