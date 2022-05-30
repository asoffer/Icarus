#include "module/module.h"

namespace module {

Module::exported_range_type Module::Exported(std::string_view name) const {
  return SymbolsWithVisibility<Visibility::Exported>(Symbols(name));
}

Module::private_range_type Module::Private(std::string_view name) const {
  return SymbolsWithVisibility<Visibility::Private>(Symbols(name));
}

}  // namespace module
