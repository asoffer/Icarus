#ifndef ICARUS_COMPILER_CYCLIC_DEPENDENCY_TRACKER_H
#define ICARUS_COMPILER_CYCLIC_DEPENDENCY_TRACKER_H

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/view.h"
#include "module/module.h"

namespace compiler {
namespace {

struct CyclicDependency {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cyclic-dependency";

  diagnostic::DiagnosticMessage ToMessage() const {
    diagnostic::SourceQuote quote;
    for (auto const &view : cycle) {
      quote = quote.Highlighted(view.range(), diagnostic::Style::ErrorText());
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found a cyclic dependency:"), std::move(quote));
  }

  std::vector<frontend::SourceView> cycle;
};

}  // namespace

// During type-verification, when a dependency on an identifier is encountered,
// we write it down here. If the same dependency is encountered more than once,
// it means we have a cycle in the dependency graph and should emit a
// diagnostic.
struct CyclicDependencyTracker {
 public:
  // Returned by `PushDependency`. Removes the dependency from the tracker when
  // it is destroyed.
  struct DependencyToken {
    DependencyToken(DependencyToken const &) = delete;
    DependencyToken(DependencyToken &&d)
        : ptr_(std::exchange(d.ptr_, nullptr)) {}

    explicit operator bool() { return ptr_; }
    ~DependencyToken() {
      if (ptr_) {
        ASSERT(ptr_->dependencies_.size() != 0u);
        ptr_->dependencies_.pop_back();
      }
    }

   private:
    friend CyclicDependencyTracker;
    DependencyToken(CyclicDependencyTracker *ptr = nullptr) : ptr_(ptr) {}

    CyclicDependencyTracker *ptr_ = nullptr;
  };

  DependencyToken PushDependency(ast::Identifier const *id,
                                 diagnostic::DiagnosticConsumer &diag) {
    dependencies_.push_back(id);
    auto iter = dependencies_.begin();
    for (; iter != dependencies_.end(); ++iter) {
      if (*iter == id) { break; }
    }

    if (iter == std::prev(dependencies_.end())) {
      return DependencyToken(this);
    }

    std::vector<frontend::SourceView> views;
    views.reserve(std::distance(iter, dependencies_.end()));
    for (auto it = iter; it != dependencies_.end(); ++it) {
      ast::Identifier const *id = *it;
      views.push_back(frontend::SourceView(SourceBufferFor(id), id->range()));
    }

    diag.Consume(CyclicDependency{.cycle = std::move(views)});
    for (; iter != dependencies_.end(); ++iter) { error_ids_.insert(id); }

    return DependencyToken();
  }

  bool has_error(ast::Identifier const *id) const {
    return error_ids_.contains(id);
  }

 private:
  friend DependencyToken;
  std::vector<ast::Identifier const *> dependencies_;

  // Collection of identifiers that are already known to have errors. This
  // allows us to emit cyclic dependencies exactly once rather than one time per
  // loop in the cycle.
  absl::flat_hash_set<ast::Identifier const *> error_ids_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CYCLIC_DEPENDENCY_TRACKER_H
