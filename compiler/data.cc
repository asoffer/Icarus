#include "compiler/data.h"

namespace compiler {
CompilationData::CompilationData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {
  dep_data_.emplace_back();
  constants_ = &dep_data_.front();
}

CompilationData::~CompilationData() {
  ASSERT(data_.deferred_work_.lock()->empty() == true);
}
}  // namespace compiler
