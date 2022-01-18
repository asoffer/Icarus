#ifndef ICARUS_COMPILER_COMPILATION_DATA_H
#define ICARUS_COMPILER_COMPILATION_DATA_H

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "base/any_invocable.h"
#include "compiler/context.h"
#include "compiler/resources.h"
#include "compiler/transient_state.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/buffer.h"
#include "frontend/source/view.h"
#include "ir/value/integer.h"
#include "ir/value/module_id.h"
#include "ir/value/result_buffer.h"
#include "module/importer.h"
#include "module/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

struct CompilationData {
  Context *context;
  WorkResources work_resources;
  PersistentResources resources;
  TransientState state;
};

struct CompilationDataReference {
  explicit CompilationDataReference(CompilationData *data)
      : data_(*ASSERT_NOT_NULL(data)) {}

  Context &context() const { return *data_.context; }
  GroupBlockReference &current() {
    auto &v = state().current;
    ASSERT(v.size() != 0u);
    return v.back();
  };

  diagnostic::DiagnosticConsumer &diag() const {
    return *data_.resources.diagnostic_consumer;
  }
  ir::BasicBlock *&current_block() { return current().block; }

  void push_current(ir::internal::BlockGroupBase *group) {
    state().current.push_back({.group = group, .block = group->entry()});
  }

  ir::BasicBlock *EmitDestructionPath(ast::Scope const *start,
                                      ast::Scope const *end) {
    return state().scaffolding.back().EmitDestructionPath(current_block(),
                                                          start, end);
  }

  module::Importer &importer() const { return *data_.resources.importer; }
  TransientState &state() { return data_.state; }

  PersistentResources &resources() { return data_.resources; }
  void set_work_resources(WorkResources wr) {
    data_.work_resources = std::move(wr);
  }
  WorkResources const &work_resources() { return data_.work_resources; }

  void Enqueue(WorkItem const &w,
               absl::flat_hash_set<WorkItem> prerequisites = {}) {
    data_.work_resources.enqueue(w, std::move(prerequisites));
  }
  void EnsureComplete(WorkItem const &w) { data_.work_resources.complete(w); }

  std::variant<ir::CompleteResultBuffer,
               std::vector<diagnostic::ConsumedMessage>>
  EvaluateToBuffer(type::Typed<ast::Expression const *> expr) {
    ASSERT(data_.work_resources.evaluate != nullptr);
    return data_.work_resources.evaluate(context(), expr);
  }

  std::optional<ir::CompleteResultBuffer> EvaluateToBufferOrDiagnose(
      type::Typed<ast::Expression const *> expr) {
    auto maybe_result = EvaluateToBuffer(expr);
    if (auto *diagnostics =
            std::get_if<std::vector<diagnostic::ConsumedMessage>>(
                &maybe_result)) {
      for (auto &d : *diagnostics) { diag().Consume(std::move(d)); }
      return std::nullopt;
    } else {
      return std::get<ir::CompleteResultBuffer>(std::move(maybe_result));
    }
  }

  // Evaluates `expr` in the current context as a value of type `T`. If
  // evaluation succeeds, returns the vaule, otherwise adds a diagnostic for the
  // failure and returns `nullopt`. If the expresison is no tof type `T`, the
  // behavior is undefined.
  template <typename T>
  std::optional<T> EvaluateOrDiagnoseAs(ast::Expression const *expr) {
    type::Type t = [] {
      constexpr auto type = base::meta<T>;
      if constexpr (type == base::meta<bool>) { return type::Bool; }
      if constexpr (type == base::meta<type::Type>) { return type::Type_; }
      if constexpr (type == base::meta<ir::ModuleId>) { return type::Module; }
      if constexpr (type == base::meta<ir::Integer>) { return type::Integer; }
      if constexpr (type == base::meta<ir::UnboundScope>) {
        return type::UnboundScope;
      }
      if constexpr (type == base::meta<ir::ScopeContext>) {
        return type::ScopeContext;
      }
    }();
    auto result = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(expr, t));
    if (not result) return std::nullopt;
    return result->get<T>(0);
  }


  CompilationData const &data() const { return data_; }

 private:
  CompilationData &data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_COMPILATION_DATA_H
