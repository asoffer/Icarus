#ifndef ICARUS_SEMANTIC_ANALYSIS_TASK_H
#define ICARUS_SEMANTIC_ANALYSIS_TASK_H

#include <coroutine>
#include <queue>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/functional/any_invocable.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/meta.h"

namespace semantic_analysis {

template <typename KeyType, base::is_enum PhaseIdentifier>
struct Scheduler;

template <typename TaskType,
          typename TaskType::phase_identifier_type PhaseIdentifier>
struct TaskPhaseType;

template <auto>
using AlwaysVoid = void;

template <typename KeyType, base::is_enum PhaseIdentifier,
          template <PhaseIdentifier> typename ReturnType = AlwaysVoid>
struct Task {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifier;
  using scheduler_type        = Scheduler<key_type, phase_identifier_type>;
  template <phase_identifier_type p>
  using return_type = ReturnType<p>;

  struct promise_type {
    promise_type(scheduler_type& s, key_type const&) : scheduler_(s) {}

    Task get_return_object() {
      static_assert(
          std::is_same_v<std::decay_t<decltype(*this)>, promise_type>);
      return Task(std::coroutine_handle<promise_type>::from_promise(*this));
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    void return_void() {}

    template <typename Phase>
    void resume_after(Phase&& phase) {
      scheduler().order_after(
          std::coroutine_handle<promise_type>::from_promise(*this),
          std::forward<Phase>(phase));
    }

    scheduler_type& scheduler() const { return scheduler_; }

   private:
    scheduler_type& scheduler_;
  };

  void resume() { handle_.resume(); }

  std::coroutine_handle<promise_type> handle() { return handle_; }

 private:
  friend promise_type;

  explicit Task(std::coroutine_handle<promise_type> handle) : handle_(handle) {}

  std::coroutine_handle<promise_type> handle_;
};

template <typename TaskType,
          typename TaskType::phase_identifier_type PhaseIdentifier>
struct TaskPhaseType {
  using task_type             = TaskType;
  using key_type              = typename task_type::key_type;
  using phase_identifier_type = typename task_type::phase_identifier_type;
  using return_type = typename task_type::template return_type<PhaseIdentifier>;

  TaskPhaseType(key_type const& key) : key_(key) {}
  TaskPhaseType(key_type&& key) : key_(std::move(key)) {}

  static constexpr phase_identifier_type phase_identifier() {
    return PhaseIdentifier;
  }

  key_type const& key() { return key_; }

  constexpr bool await_ready() const noexcept { return false; }

  void await_suspend(
      std::coroutine_handle<
          typename Task<key_type, phase_identifier_type>::promise_type>
          handle) const noexcept {
    handle.promise().resume_after(*this);
  }

  return_type await_resume() const noexcept {}

 private:
  key_type key_;
};

template <auto PhaseIdentifier, typename KeyType>
auto TaskPhase(KeyType&& key) {
  return TaskPhaseType<Task<std::decay_t<KeyType>, decltype(PhaseIdentifier)>,
                       PhaseIdentifier>(std::forward<KeyType>(key));
}

template <typename KeyType, base::is_enum PhaseIdentifierType>
struct Scheduler {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifierType;
  using task_type             = Task<key_type, phase_identifier_type>;
  using promise_type          = typename task_type::promise_type;

  Scheduler(absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator)
      : task_creator_(std::move(task_creator)) {}

  template <base::DecaysTo<key_type> K>
  void schedule(K&& key) {
    auto [iter, inserted] =
        keys_.try_emplace(std::forward<K>(key), phase_identifier_type{});
    if (inserted) {
      auto handle = task_creator_(*this, iter->first).handle();
      ready_.push(handle);
    }
  }

  template <int&..., phase_identifier_type PhaseIdentifier>
  void order_after(std::coroutine_handle<promise_type> awaiting,
                   TaskPhaseType<task_type, PhaseIdentifier> prerequisite) {
    schedule(prerequisite.key());
    auto phase = keys_.find(prerequisite.key())->second;
    if (phase <= prerequisite.phase_identifier()) {
      prereqs_[prerequisite.key()][prerequisite.phase_identifier()].push_back(
          awaiting);
    } else {
      ready_.push(awaiting);
    }
  }

  template <int&..., phase_identifier_type PhaseIdentifier>
  void set_completed(TaskPhaseType<task_type, PhaseIdentifier> phase) {
    using underlying_type = std::underlying_type_t<phase_identifier_type>;

    auto key_iter = keys_.find(phase.key());
    ASSERT(key_iter != keys_.end());
    key_iter->second = static_cast<phase_identifier_type>(
        static_cast<underlying_type>(phase.phase_identifier()) + 1);

    auto iter = prereqs_.find(phase.key());
    if (iter == prereqs_.end()) { return; }
    auto prereq_iter = iter->second.find(phase.phase_identifier());
    if (prereq_iter == iter->second.end()) { return; }

    auto node_handle = iter->second.extract(prereq_iter);
    for (auto coroutine_handle : node_handle.mapped()) {
      ready_.push(coroutine_handle);
    }
    if (iter->second.empty()) { prereqs_.erase(iter); }
  }

  void complete() {
    while (not ready_.empty()) {
      auto task = ready_.front();
      ready_.pop();
      if (task and not task.done()) { task.resume(); }
    }
    ASSERT(prereqs_.size() == 0);
  }

 private:
  absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator_;
  absl::flat_hash_map<key_type, phase_identifier_type> keys_;
  absl::flat_hash_map<
      key_type,
      absl::flat_hash_map<phase_identifier_type,
                          std::vector<std::coroutine_handle<promise_type>>>>
      prereqs_;
  std::queue<std::coroutine_handle<promise_type>> ready_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TASK_H
