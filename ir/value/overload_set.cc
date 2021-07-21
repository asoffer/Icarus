#include "ir/value/overload_set.h"

namespace ir {

std::optional<Fn> OverloadSet::Lookup(
    core::Arguments<type::QualType> const &args) {
  // TODO: search doesn't need to be linear.
  for (auto const &[cached_args, index] : cache_) {
    if (args != cached_args) { continue; }
    if (index == -1) { return std::nullopt; }
    return fns_[index];
  }

  size_t i = 0;
  for (Fn const &fn : fns_) {
    auto result = core::Callability(
        fn.type()->params(), args, [](type::QualType a, type::QualType p) {
          return type::CanCastImplicitly(p.type(), a.type());
        });
    if (result.ok()) {
      cache_.emplace_back(args, i);
      return fn;
    }
  }

  if (auto maybe_fn = create_(args)) {
    cache_.emplace_back(args, fns_.size());
    fns_.emplace_back(*std::move(maybe_fn));
  } else {
    cache_.emplace_back(args, -1);
  }
  return std::nullopt;
}

}  // namespace ir
