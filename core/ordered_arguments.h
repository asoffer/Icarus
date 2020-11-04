#ifndef ICARUS_CORE_ORDERED_ARGUMENTS_H
#define ICARUS_CORE_ORDERED_ARGUMENTS_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "core/arguments.h"

namespace core {
template <typename T>
struct OrderedArguments {
 public:
  explicit OrderedArguments() {}
  explicit OrderedArguments(
      std::vector<std::pair<std::string, std::unique_ptr<T>>> args)
      : ordered_args_(std::move(args)) {
    for (auto const &[name, arg] : ordered_args_) {
      if (name.empty()) {
        arguments_.pos_emplace(arg.get());
      } else {
        arguments_.named_emplace(name, arg.get());
      }
    }
  }

  size_t size() const { return arguments_.size(); }
  bool empty() const { return arguments_.empty(); }

  absl::Span<T const *const> pos() const & { return arguments_.pos(); }
  absl::flat_hash_map<std::string, T const *> const &named() const & {
    return arguments_.named();
  }
  Arguments<T const *, std::string> const &args() const { return arguments_; }

  // TODO test
  template <typename Fn>
  void Apply(Fn &&fn) {
    for (auto &[key, val] : ordered_args_) { std::forward<Fn>(fn)(val.get()); }
  }

  absl::Span<std::pair<std::string, std::unique_ptr<T>> const> ordered_args()
      const {
    return ordered_args_;
  }

  Arguments<std::unique_ptr<T>> DropOrder() && {
    Arguments<std::unique_ptr<T>> args;
    for (auto &[name, arg] : ordered_args_) {
      if (name.empty()) {
        args.pos_emplace(std::move(arg));
      } else {
        args.named_emplace(std::move(name), std::move(arg));
      }
    }
    return args;
  }

 private:
  Arguments<T const *, std::string> arguments_;
  std::vector<std::pair<std::string, std::unique_ptr<T>>> ordered_args_;
};
}  // namespace core

#endif  // ICARUS_CORE_ORDERED_ARGUMENTS_H
