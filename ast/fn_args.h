#ifndef ICARUS_AST_FN_ARGS_H
#define ICARUS_AST_FN_ARGS_H

#include <unordered_map>
#include <vector>

#include "base/stringify.h"

namespace ast {
template <typename T>
struct FnArgs {
  template <typename S>
  auto find(S &&name) -> decltype(auto) {
    auto iter = named_.begin();
    for (; iter != named_.end(); ++iter) {
      if (iter->first == name) { return iter; }
    }
    return iter;
  }
  template <typename S>
  auto find(S &&name) const -> decltype(auto) {
    auto iter = named_.begin();
    for (; iter != named_.end(); ++iter) {
      if (iter->first == name) { return iter; }
    }
    return iter;
  }

  std::string to_string() const {
    using base::stringify;
    std::string result;
    for (auto &&val : pos_) { result += stringify(val) + ", "; }
    for (auto &&[key, val] : named_) {
      result += key + ": " + stringify(val) + ", ";
    }
    return result;
  }

  // TODO const version (would be useful in extract_return_types.cc
  template <typename Fn>
  void Apply(Fn &&fn) {
    for (auto &&val : pos_) { fn(val); }
    for (auto &&[key, val] : named_) { fn(val); }
  }

  template <typename Fn>
  void Apply(Fn &&fn) const {
    for (const auto &val : pos_) { fn(val); }
    for (const auto &[key, val] : named_) { fn(val); }
  }

  template <typename Fn>
  auto Transform(Fn &&fn) const {
    using out_t = decltype(fn(pos_[0]));
    FnArgs<out_t> result;
    result.pos_.reserve(pos_.size());
    for (auto &&val : pos_) { result.pos_.push_back(fn(val)); }
    for (auto &&[key, val] : named_) { result.named_.emplace(key, fn(val)); }
    return result;
  }

  size_t size() const { return pos_.size() + named_.size(); }
  bool empty() const { return this->size() == 0; }

  std::vector<T> pos_;
  std::unordered_map<std::string, T> named_;
};

template <typename T>
bool operator<(const FnArgs<T> &lhs, const FnArgs<T> &rhs) {
  if (lhs.pos_.size() != rhs.pos_.size()) {
    return lhs.pos_.size() < rhs.pos_.size();
  }
  if (lhs.named_.size() != rhs.named_.size()) {
    return lhs.named_.size() < rhs.named_.size();
  }
  if (lhs.pos_ < rhs.pos_) { return true; }
  if (lhs.pos_ > rhs.pos_) { return false; }

  auto l_iter = lhs.named_.begin();
  auto r_iter = rhs.named_.begin();
  while (l_iter != lhs.named_.end() && *l_iter == *r_iter) {
    ++l_iter;
    ++r_iter;
  }
  if (l_iter == lhs.named_.end()) { return false; }
  return *l_iter < *r_iter;
}
}  // namespace ast

#endif  // ICARUS_AST_FN_ARGS_H
