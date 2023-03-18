#ifndef ICARUS_SEMANTIC_ANALYSIS_PHASE_H
#define ICARUS_SEMANTIC_ANALYSIS_PHASE_H

#include <type_traits>
#include <utility>

namespace semantic_analysis {

// Represents a phase of a task (tasks consist of one or more sequential
// phases). The enumerators must be sequentially valued, where the initial task
// has an underlying value of zero. Each enum must also have an enumerater named
// `E::Completed` indicating the task has been completed and whose underlying
// value is largest in the enum.
template <typename E>
concept PhaseIdentifier = nth::enumeration<E> and requires {
  { E::Completed } -> std::same_as<E>;
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

template <typename TaskType, typename TaskType::phase_identifier_type P,
          bool ReturnsVoid =
              std::is_void_v<typename TaskType::template return_type<P>>>
struct Phase;

template <typename TaskType, typename TaskType::phase_identifier_type P>
struct Phase<TaskType, P, true> : PhaseBase<TaskType, P> {
 private:
  using Base = PhaseBase<TaskType, P>;

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

template <typename TaskType, typename TaskType::phase_identifier_type P>
struct Phase<TaskType, P, false> : PhaseBase<TaskType, P> {
 private:
  using Base = PhaseBase<TaskType, P>;

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

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_PHASE_H
