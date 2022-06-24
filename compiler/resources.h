#ifndef ICARUS_COMPILER_RESOURCES_H
#define ICARUS_COMPILER_RESOURCES_H

#include "ast/expression.h"
#include "ast/node.h"
#include "compiler/transient_state.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/buffering.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/value/interface.h"
#include "module/importer.h"
#include "module/shared_context.h"
#include "type/typed_value.h"

namespace compiler {

inline ir::InterfaceManager GlobalInterfaceManager;

// Resources and pointers/references to data that are guaranteed to outlive
// any Compiler construction.
struct PersistentResources {
  WorkSet* work;
  CompiledModule* module;
  diagnostic::DiagnosticConsumer* diagnostic_consumer;
  module::Importer* importer;
  module::SharedContext* shared_context;
  ir::InterfaceManager* interface_manager = &GlobalInterfaceManager;
  core::Arch architecture                 = core::Host;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_RESOURCES_H
