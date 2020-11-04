#ifndef ICARUS_CORE_PARAMS_H
#define ICARUS_CORE_PARAMS_H

#include <algorithm>
#include <iostream>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_join.h"
#include "base/debug.h"
#include "base/macros.h"
#include "core/arguments.h"

namespace core {
enum ParamFlags : uint8_t {
  HAS_DEFAULT = 1,
  // At most one of MUST_NAME and MUST_NOT_NAME should be set.
  MUST_NAME = 2,  // TODO: Not yet supported
  // TODO, if you must not name something, the name shouldn't even be available.
  MUST_NOT_NAME = 4,  // TODO: semi-supported, used by foreign functions.
  VARIADIC      = 8   // TODO: Not yet supported
};

template <typename T>
struct Param {
  Param() = default;
  Param(std::string_view s, T t, ParamFlags f = ParamFlags{})
      : name(s), value(std::move(t)), flags(f) {}

  template <typename U, std::enable_if_t<not std::is_same_v<T, U> and
                                             std::is_convertible_v<U, T>,
                                         int> = 0>
  Param(Param<U> const& p) : Param(p.name, static_cast<T>(p.value), p.flags) {}

  Param(Param&&) noexcept(std::is_nothrow_move_constructible_v<T>) = default;
  Param& operator=(Param&&) noexcept(std::is_nothrow_move_assignable_v<T>) =
      default;

  Param(Param const&) noexcept(std::is_nothrow_copy_constructible_v<T>) =
      default;
  Param& operator=(Param const&)  // clang-format goof
      noexcept(std::is_nothrow_copy_assignable_v<T>) = default;

  friend constexpr bool operator==(Param const& lhs, Param const& rhs) {
    return lhs.name == rhs.name and lhs.value == rhs.value and
           lhs.flags == rhs.flags;
  }

  friend constexpr bool operator!=(Param const& lhs, Param const& rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, Param const& p) {
    return H::combine(std::move(h), p.name, p.value, p.flags);
  }

  friend std::ostream& operator<<(std::ostream& os, Param const& param) {
    using base::stringify;
    return os << param.name << ": " << stringify(param.value)
              << "(flags = " << static_cast<int>(param.flags) << ")";
  }

  // TODO: It would be really nice to have a `string_view` here instead of a
  // `string`, and this mostly works. The `string_view` would reference into the
  // syntax tree which is long-lived. Unfortunately, this "long-lived"
  // assumption is not always accurate in tests. We often create and destroy
  // modules. If these parameters get stored as part of some type information,
  // we need to do one of two things:
  //
  // 1. Make sure type information lives no longer that AST information. This
  //    means it needs to be per-module, which is maybe not a bad idea anyway.
  // 2. Use a string here.
  //
  // I think long-term #1 is the right option (especially in a world where
  // modules may not be compiled in the same process.
  std::string name = "";
  T value{};
  ParamFlags flags{};
};

template <typename T>
Param<std::decay_t<T>> AnonymousParam(T&& val) {
  using type = std::decay_t<T>;
  return Param<type>("", std::forward<T>(val), MUST_NOT_NAME);
}

// TODO ParamRef would be useful here.

template <typename T>
struct Params {
  using value_type     = Param<T>;
  using const_iterator = typename std::vector<Param<T>>::const_iterator;

  // Construct a Params object representing `n` parameters all of which must
  // not be named.

  Params() {}
  explicit Params(size_t n) : params_(n) {
    for (auto& p : params_) { p = Param<T>("", T{}, MUST_NOT_NAME); }
  }

  Params(std::initializer_list<Param<T>> params) : params_(params) {
    size_t i = 0;
    for (auto const& p : params_) {
      if (not p.name.empty()) { lookup_.emplace(p.name, i); }
      ++i;
    }
  }

  template <typename U, std::enable_if_t<not std::is_same_v<T, U> and
                                             std::is_convertible_v<U, T>,
                                         int> = 0>
  Params(std::initializer_list<Param<U>> params) {
    params_.reserve(params.size());
    for (auto const& p : params) {
      params_.push_back(static_cast<Param<T>>(p));
    }
    size_t i = 0;
    for (auto const& p : params_) {
      if (not p.name.empty()) { lookup_.emplace(p.name, i); }
      ++i;
    }
  }

  void set(size_t index, Param<T> param) {
    ASSERT(params_[index].name == "");
    lookup_.emplace(param.name, index);
    params_[index] = std::move(param);
  }

  template <typename Fn>
  auto Transform(Fn&& fn) const {
    using out_t = decltype(fn(params_[0].value));
    Params<out_t> result;
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

  Param<T> const& operator[](size_t i) const& { return params_[i]; }
  Param<T>& operator[](size_t i) & { return params_[i]; }

  void append(Param<T> p) {
    if (not p.name.empty()) { lookup_.emplace(p.name, params_.size()); }
    params_.push_back(std::move(p));
  }

  void append(std::string_view name, T val, ParamFlags flags = ParamFlags{}) {
    if (not name.empty()) { lookup_.emplace(name, params_.size()); }
    params_.emplace_back(name, std::move(val), flags);
  }

  template <typename H>
  friend H AbslHashValue(H h, Params const& params) {
    return H::combine_contiguous(std::move(h), params.params_.data(),
                                 params.params_.size());
  }

  friend bool operator==(Params const& lhs, Params const& rhs) {
    return lhs.params_ == rhs.params_;
  }

  friend bool operator!=(Params const& lhs, Params const& rhs) {
    return not(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& os, Params const& fn_params) {
    return os << "params["
              << absl::StrJoin(fn_params, ", ",
                               [](std::string* out, auto const& param) {
                                 using base::stringify;
                                 absl::StrAppend(out, param.name, ": ",
                                                 stringify(param.value));
                               })
              << "]";
  }

 private:
  template <typename U>
  friend struct Params;
  template <typename U, typename AmbiguityFn>
  friend bool AmbiguouslyCallable(Params<U> const& params1,
                                  Params<U> const& params2,
                                  AmbiguityFn&& ambiguity);

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  //
  // TODO: This could be keyed on `string_view` rather than `string` if the
  // parameters themselves were also keyed on `string_view`.
  absl::flat_hash_map<std::string, size_t> lookup_;

  std::vector<Param<T>> params_;
};

}  // namespace core

#endif  // ICARUS_CORE_PARAMS_H
