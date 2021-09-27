#ifndef ICARUS_COMPILER_WORK_GRAPH_H
#define ICARUS_COMPILER_WORK_GRAPH_H

#include <utility>
#include <type_traits>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/resources.h"
#include "compiler/work_item.h"

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
      this->emplace(item, std::move(prerequisites));
    };
  }

  base::PtrSpan<ast::Node const> ExecuteCompilationSequence(
      std::vector<std::unique_ptr<ast::Node>> nodes,
      std::invocable<WorkGraph &, base::PtrSpan<ast::Node const>> auto
          &&... steps) {
    ((steps(*this, nodes), this->complete()), ...);
    return resources().context->module().insert(nodes.begin(), nodes.end());
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
  bool Execute(WorkItem const &w) {
    auto dep_iter = dependencies_.find(w);
    if (dep_iter != dependencies_.end()) {
      auto nh = dependencies_.extract(dep_iter);
      for (auto const &n : nh.mapped()) {
        if (not Execute(n)) {
          work_[w] = false;
          return false;
        }
      }
    }

    auto [work_iter, inserted] = work_.try_emplace(w);
    if (not inserted) { return work_iter->second; }
    Compiler c(resources_);
    return work_iter->second = [&] {
      switch (w.kind) {
        case WorkItem::Kind::VerifyType:
          c.VerifyType(w.node);
          return resources().diagnostic_consumer->num_consumed() == 0;
        case WorkItem::Kind::VerifyEnumBody:
          return c.VerifyBody(&w.node->as<ast::EnumLiteral>());
        case WorkItem::Kind::VerifyFunctionBody:
          return c.VerifyBody(&w.node->as<ast::FunctionLiteral>());
        case WorkItem::Kind::VerifyStructBody:
          return c.VerifyBody(&w.node->as<ast::StructLiteral>());
        case WorkItem::Kind::CompleteStructMembers:
          return c.CompleteStruct(&w.node->as<ast::StructLiteral>());
        case WorkItem::Kind::EmitJumpBody: return c.EmitJumpBody(&w.node->as<ast::Jump>());
        case WorkItem::Kind::EmitFunctionBody:
          return c.EmitFunctionBody(&w.node->as<ast::FunctionLiteral>());
        case WorkItem::Kind::EmitShortFunctionBody:
          return c.EmitShortFunctionBody(
              &w.node->as<ast::ShortFunctionLiteral>());
      }
    }();
  }

  // Complete all work in the work queue.
  void complete() {
    while (not dependencies_.empty()) {
      auto node = dependencies_.begin()->first;
      Execute(node);
    }
  }

  PersistentResources const &resources() const { return resources_; }

 private:
  PersistentResources resources_;
  absl::flat_hash_map<WorkItem, absl::flat_hash_set<WorkItem>> dependencies_;
  absl::flat_hash_map<WorkItem, bool> work_;
};

inline bool IsConstantDeclaration(ast::Node const *n) {
  auto const *decl = n->if_as<ast::Declaration>();
  if (not decl) { return false; }
  return (decl->flags() & ast::Declaration::f_IsConst);
}

inline bool IsNotConstantDeclaration(ast::Node const *n) {
  return not IsConstantDeclaration(n);
}

void VerifyNodesSatisfying(std::predicate<ast::Node const *> auto &&predicate,
                           WorkGraph &work_graph,
                           base::PtrSpan<ast::Node const> nodes) {
  for (ast::Node const *node : nodes) {
    if (not predicate(node)) { continue; }
    work_graph.emplace(WorkItem::VerifyTypeOf(node));
  }
}

inline void ProcessExecutableBody(PersistentResources const &resources,
                                  base::PtrSpan<ast::Node const> nodes,
                                  ir::CompiledFn &main_fn) {
  Compiler c(resources);

  ICARUS_SCOPE(ir::SetCurrent(main_fn, c.builder())) {
    if (nodes.empty()) {
      EmitIrForStatements(c, nodes);
    } else {
      ast::ModuleScope *mod_scope =
          &nodes.front()->scope()->as<ast::ModuleScope>();
      MakeAllStackAllocations(c, mod_scope);
      EmitIrForStatements(c, nodes);
      MakeAllDestructions(c, mod_scope);
      // TODO determine under which scenarios destructors can be skipped.
    }
    c.builder().ReturnJump();
  }
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_WORK_GRAPH_H
