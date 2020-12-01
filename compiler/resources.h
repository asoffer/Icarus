#ifndef ICARUS_COMPILER_RESOURCES_H
#define ICARUS_COMPILER_RESOURCES_H

#include "compiler/context.h"
#include "module/importer.h"
#include "diagnostic/consumer/consumer.h"

namespace compiler {

// Resources and pointers/references to data that are guaranteed to outlive
// any Compiler construction.
struct PersistentResources {
  Context &data;
  diagnostic::DiagnosticConsumer &diagnostic_consumer;
  module::Importer &importer;
};

}

#endif  // ICARUS_COMPILER_RESOURCES_H