#include "compiler/data.h"

namespace compiler {
CompilationData::CompilationData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {
  constants_ = &dep_data_.emplace_front();
}

CompilationData::~CompilationData() {
  ASSERT(deferred_work_.lock()->empty() == true);
}
}  // namespace compiler
