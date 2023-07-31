#ifndef ICARUS_CORE_CYCLE_TRACKER_H
#define ICARUS_CORE_CYCLE_TRACKER_H

#include <algorithm>
#include <iterator>
#include <span>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/functional/function_ref.h"
#include "nth/debug/debug.h"

namespace core {

// `CycleTracker` represents a path in a dependency graph that enables users to
// find dependency cycles.
template <typename T>
struct CycleTracker {
  using tracked_type = T;

  // If `t` is already tracked by the `CycleTracker`, call `process` with a span
  // of tracked nodes. Otherwise, add `t` as a dependency.
  void push(tracked_type t,
            absl::FunctionRef<void(std::span<tracked_type const>)> process) {
    if (auto iter = std::find(dependencies_.begin(), dependencies_.end(), t);
        iter != dependencies_.end()) {
      process(std::span<tracked_type const>(iter, dependencies_.end()));

      for (; iter != dependencies_.end(); ++iter) {
        error_elements_.insert(*iter);
      }
    } else {
      dependencies_.push_back(std::move(t));
    }
  }

  // Remove the most recently pushed item as a dependency.
  void pop() {
    NTH_ASSERT(dependencies_.size() != 0);
    dependencies_.pop_back();
  }

  bool has_error(tracked_type const &t) const {
    return error_elements_.contains(t);
  }

 private:
  std::vector<tracked_type> dependencies_;

  // Collection of elements that are already known to have errors. This allows
  // us to emit cyclic dependencies exactly once rather than one time per loop
  // in the cycle.
  absl::flat_hash_set<tracked_type> error_elements_;
};

}  // namespace core

#endif  // ICARUS_CORE_CYCLE_TRACKER_H
