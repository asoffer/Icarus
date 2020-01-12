#include "compiler/data.h"

namespace compiler {

CompilationData::CompilationData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {
  current_constants_ = constants_.root();
}

CompilationData::~CompilationData() {
  ASSERT(deferred_work_.lock()->empty() == true);
}
}  // namespace compiler
