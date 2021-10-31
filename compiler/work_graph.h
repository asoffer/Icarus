#ifndef ICARUS_COMPILER_WORK_GRAPH_H
#define ICARUS_COMPILER_WORK_GRAPH_H

#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/buffering.h"
#include "type/typed_value.h"

namespace compiler {

// A `WorkGraph` is a data structure that tracks dependencies between work items
// as well as which work items have already been completed.
struct WorkGraph {
  // `WorkGraph` will construct Compilers that have references back to itself
  // through the `enqueue` callable installed on `PersistentResources`. This
  // makes it unsafe to either copy or move a `workGraph`.
  WorkGraph(WorkGraph const &) = delete;
  WorkGraph(WorkGraph &&)      = delete;
  WorkGraph &operator=(WorkGraph const &) = delete;
  WorkGraph &operator=(WorkGraph &&) = delete;

  explicit WorkGraph(PersistentResources const &resources)
      : resources_(resources) {
    resources_.enqueue = [this](WorkItem item,
                                absl::flat_hash_set<WorkItem> prerequisites) {
      ASSERT(item.context != nullptr);
      this->emplace(item, std::move(prerequisites));
    };
    resources_.evaluate = std::bind_front(&WorkGraph::EvaluateToBuffer, this);
    resources_.complete = std::bind_front(&WorkGraph::Execute, this);
  }

  void ExecuteCompilationSequence(
      base::PtrSpan<ast::Node const> nodes,
      std::invocable<WorkGraph &, base::PtrSpan<ast::Node const>> auto
          &&... steps) {
    ((steps(*this, nodes), complete()), ...);
    resources().context->module().CompilationComplete();
  }

  void emplace(WorkItem const &w,
               absl::flat_hash_set<WorkItem> const &dependencies = {}) {
    dependencies_.emplace(w, dependencies);
  }

  void emplace(WorkItem const &w,
               absl::flat_hash_set<WorkItem> &&dependencies) {
    dependencies_.emplace(w, std::move(dependencies));
  }

  // Ensure that the given `WorkItem` has been completed. If the item had
  // previously been executed, nothing happens. If the item has not been
  // previously executed, this function will also ensure that all transitively
  // depended-on `WorkItem`s are executed before executing `w`.
  bool Execute(WorkItem const &w);

  // Complete all work in the work queue.
  void complete() {
    while (not dependencies_.empty()) { Execute(dependencies_.begin()->first); }
  }

  PersistentResources const &resources() const { return resources_; }

  std::variant<ir::CompleteResultBuffer,
               std::vector<diagnostic::ConsumedMessage>>
  EvaluateToBuffer(type::Typed<ast::Expression const *> expr,
                   bool must_complete);

// private:
  PersistentResources resources_;
  absl::flat_hash_map<WorkItem, absl::flat_hash_set<WorkItem>> dependencies_;
  absl::flat_hash_map<WorkItem, bool> work_;
};

void CompileLibrary(PersistentResources const &resources,
                    base::PtrSpan<ast::Node const> nodes);
ir::CompiledFn CompileExecutable(PersistentResources const &resources,
                                 base::PtrSpan<ast::Node const> nodes);

}  // namespace compiler

#endif  // ICARUS_COMPILER_WORK_GRAPH_H