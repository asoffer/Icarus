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

template <typename KeyType, base::is_enum PhaseIdentifier>
struct TaskPhase;

template <typename KeyType, base::is_enum PhaseIdentifier>
struct Task {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifier;
  using task_phase_type       = TaskPhase<key_type, phase_identifier_type>;
  using scheduler_type        = Scheduler<key_type, phase_identifier_type>;

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

    template <base::DecaysTo<task_phase_type> Phase>
    void resume_after(Phase&& phase);

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

template <typename KeyType, base::is_enum PhaseIdentifier>
struct TaskPhase : base::Extend<TaskPhase<KeyType, PhaseIdentifier>,
                                2>::template With<base::AbslHashExtension> {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifier;
  explicit TaskPhase(phase_identifier_type phase_id, key_type key)
      : key_(key), phase_id_(phase_id) {}

  key_type const& key() { return key_; }
  constexpr phase_identifier_type phase_identifier() const { return phase_id_; }

  constexpr bool await_ready() const noexcept { return false; }

  void await_suspend(
      std::coroutine_handle<
          typename Task<key_type, phase_identifier_type>::promise_type>
          handle) const noexcept {
    handle.promise().resume_after(*this);
  }

  void await_resume() const noexcept {}

 private:
  friend base::EnableExtensions;

  key_type key_;
  phase_identifier_type phase_id_;
};

template <typename KeyType, base::is_enum PhaseIdentifier>
struct Scheduler {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifier;
  using task_phase_type       = TaskPhase<key_type, phase_identifier_type>;
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

  void order_after(std::coroutine_handle<promise_type> awaiting,
                   task_phase_type prerequisite) {
    schedule(prerequisite.key());
    auto phase = keys_.find(prerequisite.key())->second;
    if (phase <= prerequisite.phase_identifier()) {
      prereqs_[prerequisite.key()][prerequisite.phase_identifier()].push_back(
          awaiting);
    } else {
      ready_.push(awaiting);
    }
  }

  void set_completed(task_phase_type phase) {
    using underlying_type = std::underlying_type_t<phase_identifier_type>;
    keys_.find(phase.key())->second = static_cast<phase_identifier_type>(
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

template <typename KeyType, base::is_enum PhaseIdentifier>
template <base::DecaysTo<struct TaskPhase<KeyType, PhaseIdentifier>> Phase>
void Task<KeyType, PhaseIdentifier>::promise_type::resume_after(Phase&& phase) {
  scheduler().order_after(
      std::coroutine_handle<promise_type>::from_promise(*this),
      std::forward<Phase>(phase));
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TASK_H
