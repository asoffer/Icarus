#ifndef ICARUS_CORE_FN_ARGS_H
#define ICARUS_CORE_FN_ARGS_H

#include <string_view>
#include <type_traits>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/stringify.h"

namespace core {
// FnArgs represent arguments passed in to a function or other callable object.
// They contain a collection of positional arguments as well as a collection of
// named arguments which are keyed with a std::string or std::string_view.
//
// Note that FnArgs do not remember ordering; the named arguments are stored in
// a container that does not guarantee any consistent ordering. For this reason,
// this struct alone is unsuitable for use in situations where text-based
// user-input must be preserved. In those situations, something like
// OrderedFnArgs is usually more appropriate.
template <typename T, typename StringType = std::string_view,
          typename std::enable_if_t<
              std::is_same_v<StringType, std::string> ||
              std::is_same_v<StringType, std::string_view>> * = nullptr>
struct FnArgs {
  FnArgs() = default;
  FnArgs(std::vector<T> pos, absl::flat_hash_map<StringType, T> named)
      : pos_(std::move(pos)), named_(std::move(named)) {}

  absl::Span<T const> pos() const & { return pos_; }
  std::vector<T> &&pos() && { return pos_; }

  absl::flat_hash_map<StringType, T> const &named() const & { return named_; }
  absl::flat_hash_map<StringType, T> &&named() && { return named_; }

  template <typename... Args>
  void pos_emplace(Args &&... args) & {
    pos_.emplace_back(std::forward<Args>(args)...);
  }

  template <typename... Args>
  void named_emplace(Args &&... args) & {
    named_.emplace(std::forward<Args>(args)...);
  }

  T &at(size_t i) { return pos_.at(i); }
  T const &at(size_t i) const { return pos_.at(i); }

  T &at(std::string_view s) { return named_.at(s); }
  T const &at(std::string_view s) const { return named_.at(s); }

  T *at_or_null(std::string_view s) {
    auto iter = named_.find(s);
    if (iter == named_.end()) { return nullptr; }
    return &iter->second;
  }
  T const *at_or_null(std::string_view s) const {
    auto iter = named_.find(s);
    if (iter == named_.end()) { return nullptr; }
    return &iter->second;
  }

  std::string to_string() const {
    using base::stringify;
    std::string result;
    for (auto &&val : pos_) { absl::StrAppend(&result, stringify(val), ", "); }
    for (auto &&[key, val] : named_) {
      absl::StrAppend(&result, key, ": ", stringify(val), ", ");
    }
    return result;
  }

  template <typename Fn>
  void Apply(Fn &&fn) const {
    for (auto const &val : pos_) { fn(val); }
    for (auto const &[key, val] : named_) { fn(val); }
  }

  template <typename Fn>
  void ApplyWithIndex(Fn &&fn) const {
    size_t i = 0;
    for (auto const &val : pos_) { fn(i++, val); }
    for (auto const &[key, val] : named_) { fn(key, val); }
  }

  template <typename Fn>
  auto Transform(Fn &&fn) const {
    using out_t = decltype(fn(pos_[0]));
    std::vector<out_t> pos;
    pos.reserve(pos_.size());
    absl::flat_hash_map<StringType, out_t> named;
    for (auto &&val : pos_) { pos.push_back(fn(val)); }
    for (auto &&[key, val] : named_) { named.emplace(key, fn(val)); }
    return FnArgs<out_t, StringType>(std::move(pos), std::move(named));
  }

  size_t size() const { return pos().size() + named().size(); }
  bool empty() const { return size() == 0; }

 private:
  std::vector<T> pos_;
  absl::flat_hash_map<StringType, T> named_;
};

}  // namespace core

#endif  // ICARUS_CORE_FN_ARGS_H
