#ifndef ICARUS_COMPILER_RESOURCES_H
#define ICARUS_COMPILER_RESOURCES_H

#include <queue>

#include "ast/node.h"
#include "compiler/context.h"
#include "compiler/transient_state.h"
#include "diagnostic/consumer/consumer.h"
#include "module/importer.h"

namespace compiler {
struct WorkQueue;

// Resources and pointers/references to data that are guaranteed to outlive
// any Compiler construction.
struct PersistentResources {
  Context &context;
  diagnostic::DiagnosticConsumer &diagnostic_consumer;
  module::Importer &importer;
  WorkQueue &work_queue;
};

struct WorkItem : base::Extend<WorkItem>::With<base::AbslHashExtension> {
  enum class Result { Success, Failure, Deferred };
  enum class Kind {
    VerifyEnumBody,
    VerifyFunctionBody,
    VerifyStructBody,
    CompleteStructMembers,
    EmitFunctionBody,
    EmitShortFunctionBody,
    EmitJumpBody,
  };

  Result Process(PersistentResources, TransientState const &) const;

  Kind kind;
  ast::Node const *node;
};

struct WorkQueue {
  size_t size() const { return items_.size(); }
  bool empty() const { return items_.empty(); }

  void Enqueue(WorkItem item, PersistentResources resources,
               TransientState state,
               absl::flat_hash_set<WorkItem> prerequisites) {
    prerequisites_.emplace(item, std::move(prerequisites));
    items_.emplace(std::move(item),resources, std::move(state));
  }

  void ProcessOneItem();

  void Complete() {
    while (not empty()) { ProcessOneItem(); }
  }

#if defined(ICARUS_DEBUG)
  ~WorkQueue() { ASSERT(empty() == true); }
#endif

 private:
  bool Process(WorkItem const &item);

  std::queue<std::tuple<WorkItem, PersistentResources, TransientState>> items_;

  // TODO: We could combine these because we only ever need one populated, but
  // sometimes end up looking at both. If we combined them, that would reduce
  // the number of hashes we need to do.
  absl::flat_hash_map<WorkItem, absl::flat_hash_set<WorkItem>> prerequisites_;
  absl::flat_hash_set<WorkItem> completed_;

#if defined(ICARUS_DEBUG)
  size_t cycle_breaker_count_ = 0;
#endif
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_RESOURCES_H
