#include "module/symbol.h"

#include "semantic_analysis/type_system.h"

namespace module {

std::array<core::Type, 1> Symbol::SymbolTypes{semantic_analysis::Type};

}  // namespace module
