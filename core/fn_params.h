#ifndef ICARUS_CORE_FN_PARAMS_H
#define ICARUS_CORE_FN_PARAMS_H

#include <algorithm>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "core/fn_args.h"

namespace type {
struct Type;
}  // namespace type

namespace core {
enum FnParamFlags : uint8_t {
  HAS_DEFAULT = 1,
  // At moste one of MUST_NAME and MUST_NOT_NAME should be set.
  MUST_NAME     = 2,  // TODO: Not yet supported
  MUST_NOT_NAME = 4,  // TODO: Not yet supported
  VARIADIC      = 8   // TODO: Not yet supported
};

template <typename T, bool = std::is_copy_constructible_v<T>,
          bool = std::is_move_constructible_v<T>>
struct Param {
  Param() = default;
  Param(std::string_view s, T t, FnParamFlags f = FnParamFlags{})
      : name(s), value(std::move(t)), flags(f) {}
  Param(Param&&) noexcept = default;
  Param& operator=(Param&&) noexcept = default;

  Param(Param const&) noexcept = default;
  Param& operator=(Param const&) noexcept = default;

  std::string_view name = "";
  T value{};
  FnParamFlags flags{};
};

template <typename T>
struct Param<T, false, true> {
  Param() = default;
  Param(std::string_view s, T t, FnParamFlags f = FnParamFlags{})
      : name(s), value(std::move(t)), flags(f) {}
  Param(Param const&) = delete;
  Param& operator=(Param const&) = delete;
  Param(Param&&) noexcept        = default;
  Param& operator=(Param&&) noexcept = default;

  std::string_view name = "";
  T value{};
  FnParamFlags flags{};
};


template <typename T>
struct Param<T, true, false> {
  Param() = default;
  Param(std::string_view s, T t, FnParamFlags f = FnParamFlags{})
      : name(s), value(std::move(t)), flags(f) {}
  Param(Param const&) noexcept = default;
  Param& operator=(Param const&) noexcept = default;
  Param(Param&&)                          = delete;
  Param& operator=(Param&&) = delete;

  std::string_view name = "";
  T value{};
  FnParamFlags flags{};
};

template <typename T>
struct Param<T, false, false> {
  Param() = default;
  Param(std::string_view s, T t, FnParamFlags f = FnParamFlags{})
      : name(s), value(std::move(t)), flags(f) {}
  Param(Param const&) = delete;
  Param& operator=(Param const&) = delete;
  Param(Param&&)                 = delete;
  Param& operator=(Param&&) = delete;

  std::string_view name = "";
  T value{};
  FnParamFlags flags{};
};

template <typename T>
inline bool operator==(Param<T> const& lhs, Param<T> const& rhs) {
  return lhs.name == rhs.name && lhs.value == rhs.value &&
         lhs.flags == rhs.flags;
}

template <typename T>
inline bool operator!=(Param<T> const& lhs, Param<T> const& rhs) {
  return !(lhs == rhs);
}

template <typename T>
struct FnParams {
  // Construct a FnParams object representing `n` parameters all of which must
  // not be named.

  FnParams() {}
  explicit FnParams(size_t n) : params_(n) {
    for (auto& p : params_) { p = Param<T>("", T{}, MUST_NOT_NAME); }
  }

  template <typename... Ps>
  explicit FnParams(Param<T> param, Ps&&... params) {
    params_.reserve(1 + sizeof...(Ps));
    params_.push_back(std::move(param));
    (params_.push_back(std::forward<Ps>(params)), ...);
    size_t i = 0;
    for (auto const& p : params_) {
      if (!p.name.empty()) { lookup_.emplace(p.name, i); }
      ++i;
    }
  }

  void set(size_t index, Param<T> param) {
    ASSERT(params_.at(index).name == "");
    lookup_.emplace(param.name, index);
    params_.at(index) = std::move(param);
  }

  template <typename Fn>
  auto Transform(Fn &&fn) const {
    using out_t = decltype(fn(params_[0].value));
    FnParams<out_t> result;
    result.params_.reserve(params_.size());
    for (auto const& param : params_) {
      result.params_.emplace_back(param.name, fn(param.value), param.flags);
    }
    result.lookup_ = lookup_;
    return result;
  }

  constexpr size_t size() const { return params_.size(); }
  constexpr bool empty() const { return params_.empty(); }
  void reserve(size_t n) { params_.reserve(n); }

  constexpr auto begin() const { return params_.begin(); }
  constexpr auto end() const { return params_.end(); }

  constexpr auto begin() { return params_.begin(); }
  constexpr auto end() { return params_.end(); }

  size_t* at_or_null(std::string_view s) {
    auto iter = lookup_.find(s);
    if (iter == lookup_.end()) { return nullptr; }
    return &iter->second;
  }

  size_t const* at_or_null(std::string_view s) const {
    auto iter = lookup_.find(s);
    if (iter == lookup_.end()) { return nullptr; }
    return &iter->second;
  }

  Param<T> const& at(size_t i) const& { return params_.at(i); }

  // TODO deprecate. this is super dangerous.
  Param<T>& at(size_t i) & { return params_.at(i); }

  void append(std::string_view name, T val,
              FnParamFlags flags = FnParamFlags{}) {
    if (!name.empty()) { lookup_.emplace(name, params_.size()); }
    params_.emplace_back(name, std::move(val), flags);
  }

  // TODO hide this
  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  absl::flat_hash_map<std::string_view, size_t> lookup_;
 private:
  template <typename U>
  friend struct FnParams;

  std::vector<Param<T>> params_;

};

template <typename T, typename AmbiguityFn>
bool AmbiguouslyCallable(FnParams<T> const& params1, FnParams<T> const& params2,
                         AmbiguityFn&& ambiguity) {
  // In order to determine ambiguity, we consider separately each case where we
  // have a given number of positional arguments. This allows us to use an
  // interesting property: Suppose there is an ambiguous call to these two
  // parameter sets which is ambiguous that has exactly `N` positional
  // arguments. Any parameter with the same name in both parameter sets must fit
  // one of three possibilities:
  //    1. It must be in a position <= N for both parameters (i.e., positional
  //       in both calls).
  //    2. It must be in a position > N for both parameters (i.e., named in both
  //       calls).
  //    3. It must have a default in the parameters for which it appears later.
  //
  // This means we should first determine at which positions ambiguity is even
  // possible. Thus, we want to know at each possible number of positional
  // arguments, how many parameters are there would show up as a positional
  // argument in one version of the call, but named in the other and for which
  // the named argument cannot be defaulted. If the count is ever non-zero, we
  // can skip further checks.
  //
  // However, we don't need to create a vector of all of these counts. It is
  // simpler/faster to create a vector of the diffs of these counts.
  size_t min_size = std::min(params1.size(), params2.size());
  std::vector<int> diffs(1 + min_size, 0);
  for (size_t i = 0; i < min_size; ++i) {
    auto const &p1 = params1.at(i);
    if (size_t const *j = params2.at_or_null(p1.name)) {
      auto const &p2 = params2.at(*j);
      if (p2.flags & HAS_DEFAULT) { continue; }
      auto [min, max] = std::minmax(i, *j);
      diffs[min]++;
      diffs[max]--;
      if (max > min_size) { return false; }
    } else {
      continue;
    }
  }

  size_t accumulator                  = 0;
  size_t checked_type_matches_through = 0;
  for (size_t i = 0; i < diffs.size(); ++i) {
    accumulator += diffs[i];
    if (accumulator != 0) { continue; }
    // Ensure that any parameter name has a default value if it only appears in
    // one parameter set.
    for (auto [name, index1] : params1.lookup_) {
      if (index1 < i) { continue; }
      auto const &p1 = params1.at(index1);
      if (p1.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const *index2 = params2.at_or_null(name)) {
        auto const &p2 = params2.at(*index2);
        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (auto [name, index2] : params2.lookup_) {
      if (index2 < i) { continue; }
      auto const &p2 = params2.at(index2);
      if (p2.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const *index1 = params1.at_or_null(name)) {
        auto const &p1 = params1.at(*index1);

        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (; checked_type_matches_through < i; ++checked_type_matches_through) {
      if (!ambiguity(params1.at(checked_type_matches_through).value,
                     params2.at(checked_type_matches_through).value)) {
        return false;
      }
    }

    return true;

  next_named_positional_breakpoint:;
  }

  return false;
}

}  // namespace core

#endif  // ICARUS_CORE_FN_PARAMS_H
