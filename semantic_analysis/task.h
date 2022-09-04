#ifndef ICARUS_SEMANTIC_ANALYSIS_TASK_H
#define ICARUS_SEMANTIC_ANALYSIS_TASK_H

#include <coroutine>
#include <optional>
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

template <typename TaskType>
struct Scheduler;

template <auto>
using AlwaysVoid = void;

template <typename KeyType, base::is_enum PhaseIdentifier,
          template <PhaseIdentifier> typename ReturnType = AlwaysVoid>
struct Task {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseIdentifier;
  using scheduler_type        = Scheduler<Task>;
  template <phase_identifier_type p>
  using return_type = ReturnType<p>;

  template <phase_identifier_type P,
            bool ReturnsVoid = std::is_void_v<return_type<P>>>
  struct Phase;

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

    // Returns whether `phase` has already been completed.
    template <typename Phase>
    bool resume_after(Phase& phase) {
      return scheduler().order_after(
          std::coroutine_handle<promise_type>::from_promise(*this), phase);
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

template <typename TaskType, typename TaskType::phase_identifier_type P>
struct PhaseBase {
  using task_type             = TaskType;
  using key_type              = typename task_type::key_type;
  using phase_identifier_type = typename task_type::phase_identifier_type;
  using return_type           = typename task_type::template return_type<P>;

  explicit PhaseBase(key_type const& key) : key_(key) {}
  explicit PhaseBase(key_type&& key) : key_(std::move(key)) {}

  PhaseBase(PhaseBase const&) = delete;
  PhaseBase(PhaseBase&&)      = delete;

  key_type const& key() { return key_; }
  static constexpr phase_identifier_type phase_identifier() { return P; }

  constexpr bool await_ready() const noexcept { return false; }

 protected:
  key_type key_;
};

template <typename KeyType, base::is_enum PhaseIdentifier,
          template <PhaseIdentifier> typename ReturnType>
template <PhaseIdentifier P>
struct Task<KeyType, PhaseIdentifier, ReturnType>::Phase<P, true>
    : PhaseBase<Task<KeyType, PhaseIdentifier, ReturnType>, P> {
 private:
  using Base = PhaseBase<Task<KeyType, PhaseIdentifier, ReturnType>, P>;

 public:
  using key_type              = typename Base::key_type;
  using phase_identifier_type = typename Base::phase_identifier_type;
  using return_type           = typename Base::return_type;
  using task_type             = typename Base::task_type;

  explicit Phase(key_type const& key) : Base(key) {}
  explicit Phase(key_type&& key) : Base(std::move(key)) {}

  bool await_suspend(
      std::coroutine_handle<typename task_type::promise_type> handle) noexcept {
    bool ready_to_continue = handle.promise().resume_after(*this);
    return not ready_to_continue;
  }

  return_type await_resume() const noexcept {}

  void* return_slot() { return nullptr; }
};

template <typename KeyType, base::is_enum PhaseIdentifier,
          template <PhaseIdentifier> typename ReturnType>
template <PhaseIdentifier P>
struct Task<KeyType, PhaseIdentifier, ReturnType>::Phase<P, false>
    : PhaseBase<Task<KeyType, PhaseIdentifier, ReturnType>, P> {
 private:
  using Base = PhaseBase<Task<KeyType, PhaseIdentifier, ReturnType>, P>;

 public:
  using key_type              = typename Base::key_type;
  using phase_identifier_type = typename Base::phase_identifier_type;
  using return_type           = typename Base::return_type;
  using task_type             = typename Base::task_type;

  explicit Phase(key_type const& key) : Base(key) {}
  explicit Phase(key_type&& key) : Base(std::move(key)) {}

  bool await_suspend(
      std::coroutine_handle<typename task_type::promise_type> handle) noexcept {
    bool ready_to_continue = handle.promise().resume_after(*this);
    return not ready_to_continue;
  }

  return_type await_resume() const noexcept {
    return *reinterpret_cast<return_type const*>(return_slot_);
  }

  void* return_slot() { return return_slot_; }

 private:
  alignas(return_type) char return_slot_[sizeof(return_type)];
};

template <typename TaskType>
struct Scheduler {
  using task_type             = TaskType;
  using key_type              = typename task_type::key_type;
  using phase_identifier_type = typename task_type::phase_identifier_type;
  using promise_type          = typename task_type::promise_type;

  Scheduler(absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator)
      : task_creator_(std::move(task_creator)) {}

  template <base::DecaysTo<key_type> K>
  void schedule(K&& key) {
    auto [iter, inserted] = keys_.try_emplace(std::forward<K>(key));
    if (inserted) {
      auto handle = task_creator_(*this, iter->first).handle();
      ready_.push(handle);
    }
  }

  // Schedules `awaiting_handle` after `prerequisite`. Returns whether
  // `prerequisite` has already completed.
  template <int&..., phase_identifier_type P>
  bool order_after(std::coroutine_handle<promise_type> awaiting_handle,
                   typename task_type::template Phase<P>& prerequisite) {
    schedule(prerequisite.key());

    auto key_iter = keys_.find(prerequisite.key());
    ASSERT(key_iter != keys_.end());
    auto& [current_phase, phase_entries] = key_iter->second;

    auto [phase_entry_iter, inserted] =
        phase_entries.try_emplace(prerequisite.phase_identifier());

    bool already_completed = (current_phase > prerequisite.phase_identifier());
    if (already_completed) {
      using return_type = typename task_type::template Phase<P>::return_type;
      if constexpr (not std::is_void_v<return_type>) {
        std::cerr << prerequisite.phase_identifier() << "\n";
        ASSERT(inserted == false);
        auto* return_slot_ptr = prerequisite.return_slot();
        if (return_slot_ptr) {
          new (return_slot_ptr) return_type(
              *static_cast<return_type*>(phase_entry_iter->second.results));
        }
      }
    } else {
      phase_entry_iter->second.awaiting.emplace_back(
          awaiting_handle, prerequisite.return_slot());
    }
    return already_completed;
  }

  template <phase_identifier_type P>
  void set_completed(key_type const& key) requires(
      std::is_void_v<typename task_type::template Phase<P>::return_type>) {
    using underlying_type = std::underlying_type_t<phase_identifier_type>;
    auto key_iter         = keys_.find(key);
    ASSERT(key_iter != keys_.end());
    auto& [current_phase, phase_entries] = key_iter->second;

    current_phase =
        static_cast<phase_identifier_type>(static_cast<underlying_type>(P) + 1);

    auto [phase_entry_iter, inserted] = phase_entries.try_emplace(P);

    // Add all the entries awaiting the completion of this phase to the
    // ready-queue.
    auto awaiting_entries = phase_entry_iter->second.awaiting;
    for (auto [coroutine_handle, return_slot] : awaiting_entries) {
      ready_.push(coroutine_handle);
    }
    awaiting_entries.clear();
  }

  template <phase_identifier_type P, typename ReturnType>
  void
  set_completed(key_type const& key, ReturnType&& phase_return_value) requires(
      not std::is_void_v<typename task_type::template Phase<P>::return_type>) {
    using return_type     = typename task_type::template Phase<P>::return_type;
    using underlying_type = std::underlying_type_t<phase_identifier_type>;

    auto* result_ptr = static_cast<return_type*>(
        results_
            .emplace_back(
                new return_type(std::forward<ReturnType>(phase_return_value)),
                [](void* ptr) { delete static_cast<return_type*>(ptr); })
            .get());

    auto key_iter = keys_.find(key);
    ASSERT(key_iter != keys_.end());
    auto& [current_phase, phase_entries] = key_iter->second;

    current_phase =
        static_cast<phase_identifier_type>(static_cast<underlying_type>(P) + 1);

    auto [phase_entry_iter, inserted] = phase_entries.try_emplace(P);

    // Add all the entries awaiting the completion of this phase to the
    // ready-queue.
    auto awaiting_entries = phase_entry_iter->second.awaiting;
    for (auto [coroutine_handle, return_slot] : awaiting_entries) {
      new (return_slot) return_type(*result_ptr);
      phase_entry_iter->second.results = result_ptr;
      ready_.push(coroutine_handle);
    }
    awaiting_entries.clear();
    phase_entry_iter->second.results = result_ptr;
  }

  void complete() {
    while (not ready_.empty()) {
      auto task = ready_.front();
      ready_.pop();
      if (task and not task.done()) { task.resume(); }
    }
  }

 private:
  // Called to create a `task_type` if one has not yet been added to the
  // scheduler corresponding to a given value of type `key_type`.
  absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator_;

  // Represents either the address of results already completed by a given
  // phase, or the container consisting of all tasks awaiting a task phase
  // completion if the phase has yet to be completed.
  struct PhaseEntry {
    void* results;
    std::vector<std::pair<std::coroutine_handle<promise_type>, void*>> awaiting;
  };

  // Represents the state of a given scheduled task, including which phase it is
  // in, and which other tasks are awaiting which phases of it to complete.
  struct TaskState {
    phase_identifier_type current_phase = {};

    // Map from a phase to the set of coroutine handles awaiting the completion
    // of the given phase for the corresponding task.
    absl::flat_hash_map<phase_identifier_type, PhaseEntry> phase_entries;
  };

  absl::flat_hash_map<key_type, TaskState> keys_;

  std::vector<std::unique_ptr<void, void (*)(void*)>> results_;
  std::queue<std::coroutine_handle<promise_type>> ready_;
  };

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TASK_H
