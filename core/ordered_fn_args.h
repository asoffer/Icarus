#ifndef ICARUS_CORE_ORDERED_FN_ARGS_H
#define ICARUS_CORE_ORDERED_FN_ARGS_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "core/fn_args.h"

namespace core {
template <typename T>
struct OrderedFnArgs {
 public:
  explicit OrderedFnArgs() {}
  explicit OrderedFnArgs(
      std::vector<std::pair<std::string, std::unique_ptr<T>>> args)
      : ordered_args_(std::move(args)) {
    for (auto const & [ name, arg ] : ordered_args_) {
      if (name.empty()) {
        fn_args_.pos_emplace(arg.get());
      } else {
        fn_args_.named_emplace(name, arg.get());
      }
    }
  }

  size_t size() const { return fn_args_.size(); }
  bool empty() const { return fn_args_.empty(); }

  absl::Span<T const *const> pos() const & { return fn_args_.pos(); }
  absl::flat_hash_map<std::string_view, T const *> const &named() const & {
    return fn_args_.named();
  }
  FnArgs<T const *, std::string_view> const &args() const { return fn_args_; }

  // TODO test
  template <typename Fn>
  void Apply(Fn &&fn) {
    for (auto & [ key, val ] : ordered_args_) {
      std::forward<Fn>(fn)(val.get());
    }
  }

  absl::Span<std::pair<std::string, std::unique_ptr<T>> const> ordered_args()
      const {
    return ordered_args_;
  }

  FnArgs<std::unique_ptr<T>> DropOrder() && {
    FnArgs<std::unique_ptr<T>> args;
    for (auto & [ name, arg ] : ordered_args_) {
      if (name.empty()) {
        args.pos_emplace(std::move(arg));
      } else {
        args.named_emplace(std::move(name), std::move(arg));
      }
    }
    return args;
  }

 private:
  FnArgs<T const *, std::string_view> fn_args_;
  std::vector<std::pair<std::string, std::unique_ptr<T>>> ordered_args_;
};
}  // namespace core

#endif  // ICARUS_CORE_ORDERED_FN_ARGS_H
