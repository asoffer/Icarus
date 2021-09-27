#ifndef ICARUS_COMPILER_RESOURCES_H
#define ICARUS_COMPILER_RESOURCES_H

#include <functional>

#include "ast/node.h"
#include "compiler/context.h"
#include "compiler/transient_state.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/consumer.h"
#include "module/importer.h"

namespace compiler {
// Resources and pointers/references to data that are guaranteed to outlive
// any Compiler construction.
struct PersistentResources {
  Context* context;
  diagnostic::DiagnosticConsumer* diagnostic_consumer;
  module::Importer* importer;
  std::function<void(WorkItem, absl::flat_hash_set<WorkItem>)> enqueue;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_RESOURCES_H
