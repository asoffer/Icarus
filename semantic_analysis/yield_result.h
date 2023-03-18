#ifndef ICARUS_SEMANTIC_ANALYSIS_YIELD_RESULT_H
#define ICARUS_SEMANTIC_ANALYSIS_YIELD_RESULT_H

#include <type_traits>
#include <utility>

namespace semantic_analysis {
namespace internal_task {

struct Empty {};

}  // namespace internal_task

template <typename TaskType, typename TaskType::phase_identifier_type P>
struct YieldResult {
 private:
  using task_type   = TaskType;
  using key_type    = typename task_type::key_type;
  using return_type = typename task_type::template return_type<P>;

 public:
  template <std::convertible_to<key_type> K>
  explicit YieldResult(K&& key) requires(std::is_void_v<return_type>)
      : key_(std::forward<K>(key)) {}

  template <std::convertible_to<key_type> K, typename... Arguments>
  explicit YieldResult(K&& key, Arguments&&... arguments) requires(
      std::constructible_from<return_type, Arguments...>)
      : key_(std::forward<K>(key)),
        value_(std::forward<Arguments>(arguments)...) {}

 private:
  friend typename task_type::promise_type;

  constexpr key_type const& key() const { return key_; }

  template <typename R = return_type>
  constexpr R const& value() const requires(not std::is_void_v<return_type>) {
    return value_;
  }

  using return_storage_type =
      std::conditional_t<std::is_void_v<return_type>, internal_task::Empty,
                         return_type>;
  key_type key_;
  [[no_unique_address]] return_storage_type value_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_YIELD_RESULT_H
