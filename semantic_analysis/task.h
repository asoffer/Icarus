#ifndef ICARUS_SEMANTIC_ANALYSIS_TASK_H
#define ICARUS_SEMANTIC_ANALYSIS_TASK_H

#include <coroutine>
#include <queue>
#include <span>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/functional/any_invocable.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "nth/meta/concepts.h"

namespace semantic_analysis {
namespace internal_task {

struct Empty {};

}  // namespace internal_task

template <typename TaskType>
struct Scheduler;

template <auto>
using AlwaysVoid = void;

// Represents a phase of a task (tasks consist of one or more sequential
// phases). The enumerators must be sequentially valued, where the initial task
// has an underlying value of zero. Each enum must also have an enumerater named
// `E::Completed` indicating the task has been completed and whose underlying
// value is largest in the enum.
template <typename E>
concept PhaseIdentifier = nth::enumeration<E> and requires {
  { E::Completed } -> std::same_as<E>;
};

template <typename KeyType, PhaseIdentifier PhaseId,
          template <PhaseId> typename ReturnType = AlwaysVoid>
struct Task {
  using key_type              = KeyType;
  using phase_identifier_type = PhaseId;
  using scheduler_type        = Scheduler<Task>;
  template <phase_identifier_type p>
  using return_type = ReturnType<p>;

  template <phase_identifier_type P,
            bool ReturnsVoid = std::is_void_v<return_type<P>>>
  struct Phase;
  template <phase_identifier_type P,
            bool ReturnsVoid = std::is_void_v<return_type<P>>>
  struct YieldResult;

  struct promise_type {
    promise_type(scheduler_type& s, key_type const&) : scheduler_(s) {}

    Task get_return_object() {
      return Task(std::coroutine_handle<promise_type>::from_promise(*this));
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}

    template <phase_identifier_type P>
    auto return_value(YieldResult<P> y) {
      return yield_value(y);
    }

    template <phase_identifier_type P>
    auto yield_value(YieldResult<P> y) {
      if constexpr (P != phase_identifier_type::Completed) {
        if constexpr (std::is_void_v<return_type<P>>) {
          scheduler().template set_completed<P>(y.key());
        } else {
          scheduler().template set_completed<P>(y.key(), y.value());
        }
      }

      struct {
        constexpr bool await_ready() const noexcept { return true; }
        bool await_suspend(std::coroutine_handle<promise_type>) noexcept {
          return false;
        }
        void await_resume() const noexcept {}
      } result;
      return result;
    }

    // Returns whether `phase` has already been completed.
    template <typename PhaseType>
    bool resume_after(PhaseType& phase) {
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

template <typename KeyType, PhaseIdentifier PhaseId,
          template <PhaseId> typename ReturnType>
template <PhaseId P>
struct Task<KeyType, PhaseId, ReturnType>::Phase<P, true>
    : PhaseBase<Task<KeyType, PhaseId, ReturnType>, P> {
 private:
  using Base = PhaseBase<Task<KeyType, PhaseId, ReturnType>, P>;

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
};

template <typename KeyType, PhaseIdentifier PhaseId,
          template <PhaseId> typename ReturnType>
template <PhaseId P>
struct Task<KeyType, PhaseId, ReturnType>::Phase<P, false>
    : PhaseBase<Task<KeyType, PhaseId, ReturnType>, P> {
 private:
  using Base = PhaseBase<Task<KeyType, PhaseId, ReturnType>, P>;

 public:
  using key_type              = typename Base::key_type;
  using phase_identifier_type = typename Base::phase_identifier_type;
  using return_type           = typename Base::return_type;
  using task_type             = typename Base::task_type;

  explicit Phase(key_type const& key) : Base(key), return_slot_{} {}
  explicit Phase(key_type&& key) : Base(std::move(key)), return_slot_{} {}

  bool await_suspend(
      std::coroutine_handle<typename task_type::promise_type> handle) noexcept {
    bool ready_to_continue = handle.promise().resume_after(*this);
    return not ready_to_continue;
  }

  return_type await_resume() const noexcept { return return_slot_; }

  return_type& return_slot() { return return_slot_; }
  return_type const& return_slot() const { return return_slot_; }

 private:
  return_type return_slot_;
};

template <typename KeyType, PhaseIdentifier PhaseId,
          template <PhaseId> typename ReturnType>
template <PhaseId P, bool ReturnsVoid>
struct Task<KeyType, PhaseId, ReturnType>::YieldResult {
 public:
  using key_type              = KeyType;
  using phase_identifier_type = PhaseId;
  using task_type             = Task<KeyType, PhaseId, ReturnType>;
  using return_type           = typename task_type::template return_type<P>;

  template <std::convertible_to<key_type> K>
  explicit YieldResult(K&& key) requires(ReturnsVoid)
      : key_(std::forward<K>(key)) {}

  template <std::convertible_to<key_type> K, typename... Arguments>
  explicit YieldResult(K&& key, Arguments&&... arguments) requires(
      std::constructible_from<return_type, Arguments...>)
      : key_(std::forward<K>(key)),
        value_(std::forward<Arguments>(arguments)...) {}

 private:
  friend task_type::promise_type;

  constexpr key_type const& key() const { return key_; }

  template <typename R = return_type>
  constexpr R const& value() const requires(not ReturnsVoid) {
    return value_;
  }

  using return_storage_type =
      std::conditional_t<ReturnsVoid, internal_task::Empty, return_type>;
  key_type key_;
  [[no_unique_address]] return_storage_type value_;
};

template <typename TaskType>
struct Scheduler {
  using task_type             = TaskType;
  using key_type              = typename task_type::key_type;
  using phase_identifier_type = typename task_type::phase_identifier_type;
  using promise_type          = typename task_type::promise_type;

  Scheduler(absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator)
      : task_creator_(std::move(task_creator)) {}

  template <typename K>
  void schedule(K&& key) {
    auto [iter, inserted] = keys_.try_emplace(std::forward<K>(key));
    if (inserted) { ready_.push(task_creator_(*this, iter->first).handle()); }
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

    auto& phase_entry =
        phase_entries[static_cast<size_t>(prerequisite.phase_identifier())];

    bool already_completed = (current_phase > prerequisite.phase_identifier());
    using return_type      = typename task_type::template Phase<P>::return_type;
    if (already_completed) {
      if constexpr (not std::is_void_v<return_type>) {
        if (auto* results_ptr = phase_entry.template results<return_type>()) {
          prerequisite.return_slot() = *results_ptr;
        }
      }
    } else {
      if constexpr (std::is_void_v<return_type>) {
        phase_entry.await(awaiting_handle, nullptr);
      } else {
        phase_entry.await(awaiting_handle, &prerequisite.return_slot());
      }
    }
    return already_completed;
  }

  template <phase_identifier_type P>
  static constexpr phase_identifier_type NextAfter() {
    return static_cast<phase_identifier_type>(static_cast<size_t>(P) + 1);
  }

  void complete() {
    while (not ready_.empty()) {
      auto task = ready_.front();
      ready_.pop();
      if (task) {
        if (task.done()) {
          std::exchange(task, nullptr).destroy();
        } else {
          task.resume();
        }
      }
    }
  }

 private:
  friend promise_type;

  // Called to create a `task_type` if one has not yet been added to the
  // scheduler corresponding to a given value of type `key_type`.
  absl::AnyInvocable<task_type(Scheduler&, key_type)> task_creator_;

  // Represents either the address of results already completed by a given
  // phase, or the container consisting of all tasks awaiting a task phase
  // completion if the phase has yet to be completed.
  struct PhaseEntry {
    PhaseEntry()
        : PhaseEntry(std::vector<
                     std::pair<std::coroutine_handle<promise_type>, void*>>{}) {
    }

    static PhaseEntry Awaiting(
        std::vector<std::pair<std::coroutine_handle<promise_type>, void*>>
            awaiting) {
      return PhaseEntry(std::move(awaiting));
    }

    template <typename T>
    T const* results() const {
      return static_cast<T const*>(results_);
    }

    static PhaseEntry ResultPointer(void* results) {
      return PhaseEntry(results);
    }

    void await(std::coroutine_handle<promise_type> handle, void* result) {
      awaiting_.emplace_back(handle, result);
    }

    std::span<std::pair<std::coroutine_handle<promise_type>, void*> const>
    awaiting() const {
      return awaiting_;
    }

   private:
    explicit PhaseEntry(
        std::vector<std::pair<std::coroutine_handle<promise_type>, void*>>
            awaiting)
        : results_(nullptr), awaiting_(std::move(awaiting)) {}
    explicit PhaseEntry(void* results) : results_(results) {}

    void* results_;
    std::vector<std::pair<std::coroutine_handle<promise_type>, void*>>
        awaiting_;
  };

  // Represents the state of a given scheduled task, including which phase it is
  // in, and which other tasks are awaiting which phases of it to complete.
  struct TaskState {
    phase_identifier_type current_phase = {};

    // Map from a phase to the set of coroutine handles awaiting the completion
    // of the given phase for the corresponding task.
    using underlying_type = std::underlying_type_t<phase_identifier_type>;
    std::array<PhaseEntry,
               static_cast<underlying_type>(phase_identifier_type::Completed)>
        phase_entries;
  };

  template <phase_identifier_type P>
  void set_completed(key_type const& key) requires(
      std::is_void_v<typename task_type::template Phase<P>::return_type>) {
    auto& [current_phase, phase_entries] = keys_.try_emplace(key).first->second;
    current_phase                        = NextAfter<P>();
    auto& phase_entry = phase_entries[static_cast<size_t>(P)];

    // Add all the entries awaiting the completion of this phase to the
    // ready-queue.
    for (auto [coroutine_handle, return_slot] : phase_entry.awaiting()) {
      ready_.push(coroutine_handle);
    }
    phase_entry = PhaseEntry::ResultPointer(nullptr);
  }

  template <phase_identifier_type P, typename ReturnType>
  void
  set_completed(key_type const& key, ReturnType&& phase_return_value) requires(
      not std::is_void_v<typename task_type::template Phase<P>::return_type>) {
    using return_type = typename task_type::template Phase<P>::return_type;

    auto* result_ptr = static_cast<return_type*>(
        results_
            .emplace_back(
                new return_type(std::forward<ReturnType>(phase_return_value)),
                [](void* ptr) { delete static_cast<return_type*>(ptr); })
            .get());

    auto& [current_phase, phase_entries] = keys_.try_emplace(key).first->second;

    current_phase     = NextAfter<P>();
    auto& phase_entry = phase_entries[static_cast<size_t>(P)];

    // Add all the entries awaiting the completion of this phase to the
    // ready-queue.
    for (auto [coroutine_handle, return_slot] : phase_entry.awaiting()) {
      *reinterpret_cast<return_type*>(return_slot) = *result_ptr;
      ready_.push(coroutine_handle);
    }
    phase_entry = PhaseEntry::ResultPointer(result_ptr);
  }

  absl::flat_hash_map<key_type, TaskState> keys_;

  std::vector<std::unique_ptr<void, void (*)(void*)>> results_;
  std::queue<std::coroutine_handle<promise_type>> ready_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TASK_H
