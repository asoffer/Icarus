#ifndef ICARUS_COMPILER_TRANSIENT_STATE_H
#define ICARUS_COMPILER_TRANSIENT_STATE_H

#include <iterator>
#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/resources.h"
#include "ir/blocks/basic.h"
#include "ir/instruction/core.h"
#include "ir/value/label.h"

namespace compiler {

struct WorkItem {
  enum class Result { Success, Failure, Deferred };
  enum class Kind {
    VerifyEnumBody,
    VerifyFunctionBody,
    VerifyStructBody,
    CompleteStructMembers,
  };

  Result Process() const;

  Kind kind;
  ast::Node const *node;
  PersistentResources &resources;
};

struct WorkQueue {
  bool empty() const { return items_.empty(); }

  void Enqueue(WorkItem item) { items_.push(std::move(item)); }

  void ProcessOneItem();

 private:
  bool Process(WorkItem const &item);

  std::queue<WorkItem> items_;

#if defined(ICARUS_DEBUG)
  size_t cycle_breaker_count_ = 0;
#endif
};

// Compiler state that needs to be tracked during the compilation of a single
// function or jump, but otherwise does not need to be saved.
struct TransientState {
  TransientState()                  = default;
  TransientState(TransientState &&) = default;
  TransientState &operator=(TransientState &&) = default;
  ~TransientState() {
    while (not work_queue.empty()) { work_queue.ProcessOneItem(); }
    for (auto &work : deferred_work) {
      if (work and *work) { std::move (*work)(); }
    }
  }

  struct ScopeLandingState {
    ir::Label label;
    ir::Scope scope;
    type::QualType result_type;
    ir::BasicBlock *block;
  };
  std::vector<ScopeLandingState> scope_landings;

  WorkQueue work_queue;

  absl::flat_hash_map<ast::YieldStmt const *, core::Arguments<ir::Value>>
      yields;
  bool must_complete = true;

  std::vector<std::unique_ptr<base::move_func<void()>>> deferred_work;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_TRANSIENT_STATE_H
