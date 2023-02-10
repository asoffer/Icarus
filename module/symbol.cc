#include "module/symbol.h"

#include "base/debug.h"
#include "semantic_analysis/type_system.h"

namespace module {

core::Type Symbol::type() const {
  switch (symbol_.index()) {
    case 0: return semantic_analysis::Type;
    case 1: return std::get<TypedFunction>(symbol_).type;
    default: UNREACHABLE();
  }
}

}  // namespace module
