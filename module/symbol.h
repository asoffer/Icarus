#ifndef ICARUS_MODULE_SYMBOL_H
#define ICARUS_MODULE_SYMBOL_H

#include "core/type_system/type.h"

namespace module {

struct Symbol {
  Symbol(core::Type t) : symbol_(t) {}

  core::Type type() const { return SymbolTypes[symbol_.index()]; }

  template <typename T>
  T const &as() const {
    return std::get<T>(symbol_);
  }

 private:
  static std::array<core::Type, 1> SymbolTypes;
  std::variant<core::Type> symbol_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SYMBOL_H
