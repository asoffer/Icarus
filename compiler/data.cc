#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {}

DependentComputedData::~DependentComputedData() {
  // TODO figure out what's being dropped?
  // ASSERT(deferred_work_.lock()->empty() == true);
}

}  // namespace compiler
