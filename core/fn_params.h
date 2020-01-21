#ifndef ICARUS_CORE_FN_PARAMS_H
#define ICARUS_CORE_FN_PARAMS_H

#include <algorithm>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_join.h"
#include "base/debug.h"
#include "base/macros.h"
#include "core/fn_args.h"

namespace core {
enum FnParamFlags : uint8_t {
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
  Param(std::string_view s, T t, FnParamFlags f = FnParamFlags{})
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

  friend constexpr bool operator==(Param<T> const& lhs, Param<T> const& rhs) {
    return lhs.name == rhs.name and lhs.value == rhs.value and
           lhs.flags == rhs.flags;
  }

  friend constexpr bool operator!=(Param<T> const& lhs, Param<T> const& rhs) {
    return not(lhs == rhs);
 }

 template <typename H>
 friend H AbslHashValue(H h, Param const& p) {
   return H::combine(std::move(h), p.name, p.value, p.flags);
  }

  std::string_view name = "";
  T value{};
  FnParamFlags flags{};
};

template <typename T>
Param<std::decay_t<T>> AnonymousParam(T&& val) {
  using type = std::decay_t<T>;
  return Param<type>("", std::forward<T>(val), MUST_NOT_NAME);
}

// TODO ParamRef would be useful here.

template <typename T>
struct FnParams {
  using value_type     = Param<T>;
  using const_iterator = typename std::vector<Param<T>>::const_iterator;

  // Construct a FnParams object representing `n` parameters all of which must
  // not be named.

  FnParams() {}
  explicit FnParams(size_t n) : params_(n) {
    for (auto& p : params_) { p = Param<T>("", T{}, MUST_NOT_NAME); }
  }

  FnParams(std::initializer_list<Param<T>> params) : params_(params) {
    size_t i = 0;
    for (auto const& p : params_) {
      if (not p.name.empty()) { lookup_.emplace(p.name, i); }
      ++i;
    }
  }

  template <typename U, std::enable_if_t<not std::is_same_v<T, U> and
                                             std::is_convertible_v<U, T>,
                                         int> = 0>
  FnParams(std::initializer_list<Param<U>> params) {
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
    ASSERT(params_.at(index).name == "");
    lookup_.emplace(param.name, index);
    params_.at(index) = std::move(param);
  }

  template <typename Fn>
  auto Transform(Fn&& fn) const {
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

  void append(Param<T> p) {
    if (not p.name.empty()) { lookup_.emplace(p.name, params_.size()); }
    params_.push_back(std::move(p));
  }

  void append(std::string_view name, T val,
              FnParamFlags flags = FnParamFlags{}) {
    if (not name.empty()) { lookup_.emplace(name, params_.size()); }
    params_.emplace_back(name, std::move(val), flags);
  }

  template <typename H>
  friend H AbslHashValue(H h, FnParams const& params) {
    return H::combine_contiguous(std::move(h), params.params_.data(),
                                 params.params_.size());
  }

  friend bool operator==(FnParams const& lhs, FnParams const& rhs) {
    return lhs.params_ == rhs.params_;
  }

  friend bool operator!=(FnParams const& lhs, FnParams const& rhs) {
    return not(lhs == rhs);
  }

 private:
  template <typename U>
  friend struct FnParams;
  template <typename U, typename AmbiguityFn>
  friend bool AmbiguouslyCallable(FnParams<U> const& params1,
                                  FnParams<U> const& params2,
                                  AmbiguityFn&& ambiguity);

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  absl::flat_hash_map<std::string_view, size_t> lookup_;

  std::vector<Param<T>> params_;
};

template <typename T>
std::string stringify(FnParams<T> const& fn_params) {
  return absl::StrCat("fnparams[",
                      absl::StrJoin(fn_params, ", ",
                                    [](std::string* out, auto const& param) {
                                      using base::stringify;
                                      absl::StrAppend(out, param.name, ": ",
                                                      stringify(param.value));
                                    }),
                      "]");
}

// Returns the index just after the last instance of MUST_NOT_NAME. If
// MUST_NOT_NAME is the last parameter, then we return params.size(). If it is
// not present at all, we return 0.
template <typename T>
size_t MustNotNameTailIndex(FnParams<T> const& params) {
  if (params.empty()) { return 0; }
  for (int i = params.size() - 1; i >= 0; --i) {
    if (params.at(i).flags & MUST_NOT_NAME) { return i + 1; }
  }
  return 0;
}

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
    auto const& p1 = params1.at(i);
    if (size_t const* j = params2.at_or_null(p1.name)) {
      auto const& p2 = params2.at(*j);
      if (p2.flags & HAS_DEFAULT) { continue; }
      auto [min, max] = std::minmax(i, *j);
      diffs[min]++;
      diffs[max]--;
      if (max > min_size) { return false; }
    } else {
      continue;
    }
  }

  // No need to attempt naming parameters that must not be named.
  size_t starting_named_index =
      std::max(MustNotNameTailIndex(params1), MustNotNameTailIndex(params2));

  size_t accumulator                  = 0;
  size_t checked_type_matches_through = 0;
  for (size_t i = 0; i < diffs.size(); ++i) {
    accumulator += diffs[i];
    if (accumulator != 0 or i < starting_named_index) { continue; }
    // Ensure that any parameter name has a default value if it only appears in
    // one parameter set.
    for (auto [name, index1] : params1.lookup_) {
      if (index1 < i) { continue; }
      auto const& p1 = params1.at(index1);
      if (p1.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const* index2 = params2.at_or_null(name)) {
        auto const& p2 = params2.at(*index2);
        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (auto [name, index2] : params2.lookup_) {
      if (index2 < i) { continue; }
      auto const& p2 = params2.at(index2);
      if (p2.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const* index1 = params1.at_or_null(name)) {
        auto const& p1 = params1.at(*index1);

        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (; checked_type_matches_through < i; ++checked_type_matches_through) {
      if (not ambiguity(params1.at(checked_type_matches_through).value,
                        params2.at(checked_type_matches_through).value)) {
        return false;
      }
    }

    return true;

  next_named_positional_breakpoint:;
  }

  return false;
}

// Returns true if and only if a callable with `params` can be called with
// `args`.
template <typename T, typename U, typename ConvertibleFn>
bool IsCallable(FnParams<T> const& params, FnArgs<U> const& args,
                ConvertibleFn fn) {
  if (params.size() < args.size()) {
    DEBUG_LOG("core::IsCallable")
    ("IsCallable = false due to size mismatch (", params.size(), " vs ",
     args.size());
    return false;
  }

  for (size_t i = 0; i < args.pos().size(); ++i) {
    if (not fn(args.pos().at(i), params.at(i).value)) {
      DEBUG_LOG("core::IsCallable")
      ("IsCallable = false due to convertible failure at ", i);
      return false;
    }
  }

  for (auto const& [name, type] : args.named()) {
    ASSIGN_OR(
        {
          DEBUG_LOG("core::IsCallable")
          ("No such parameter named \"", name, "\"");
          return false;
        },
        auto const& index, params.at_or_null(name));
    if (not fn(type, params.at(index).value)) {
      DEBUG_LOG("core::IsCallable")
      ("IsCallable = false due to convertible failure on \"", name, "\"");
      return false;
    }
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const& param = params.at(i);
    if (param.flags & HAS_DEFAULT) { continue; }
    if (args.at_or_null(param.name) == nullptr) {
      DEBUG_LOG("core::IsCallable")
      ("No argument for non-default parameter named \"", param.name, "\"");
      return false;
    }
  }

  DEBUG_LOG("core::IsCallable")("Yes, it's callable");
  return true;
}

}  // namespace core

#endif  // ICARUS_CORE_FN_PARAMS_H
