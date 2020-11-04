#ifndef ICARUS_CORE_CALL_H
#define ICARUS_CORE_CALL_H

#include <algorithm>
#include <vector>

#include "absl/types/span.h"
#include "base/log.h"
#include "core/arguments.h"
#include "core/dependency_node.h"
#include "core/params.h"
#include "core/params_ref.h"

namespace core {

template <typename T, typename AmbiguityFn>
bool AmbiguouslyCallable(Params<T> const& params1, Params<T> const& params2,
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
    auto const& p1 = params1[i];
    if (size_t const* j = params2.at_or_null(p1.name)) {
      auto const& p2 = params2[*j];
      if (p2.flags & HAS_DEFAULT) { continue; }
      auto [min, max] = std::minmax(i, *j);
      diffs[min]++;
      diffs[max]--;
      if (max > min_size) { return false; }
    } else {
      continue;
    }
  }

  // Returns the index just after the last instance of MUST_NOT_NAME. If
  // MUST_NOT_NAME is the last parameter, then we return params.size(). If it is
  // not present at all, we return 0.
  constexpr auto MustNotNameTailIndex = [](Params<T> const& params) -> size_t {
    if (params.empty()) { return 0; }
    for (int i = params.size() - 1; i >= 0; --i) {
      if (params[i].flags & MUST_NOT_NAME) { return i + 1; }
    }
    return 0;
  };

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
      auto const& p1 = params1[index1];
      if (p1.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const* index2 = params2.at_or_null(name)) {
        auto const& p2 = params2[*index2];
        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (auto [name, index2] : params2.lookup_) {
      if (index2 < i) { continue; }
      auto const& p2 = params2[index2];
      if (p2.flags & HAS_DEFAULT) {
        continue;
      } else if (size_t const* index1 = params1.at_or_null(name)) {
        auto const& p1 = params1[*index1];

        if (ambiguity(p1.value, p2.value)) { continue; }
        goto next_named_positional_breakpoint;
      } else {
        goto next_named_positional_breakpoint;
      }
    }

    for (; checked_type_matches_through < i; ++checked_type_matches_through) {
      if (not ambiguity(params1[checked_type_matches_through].value,
                        params2[checked_type_matches_through].value)) {
        return false;
      }
    }

    return true;

  next_named_positional_breakpoint:;
  }

  return false;
}

// For each parameter in `params` for which `args` has chosen to use the default
// value, update `args` to contain the appropriate default value, as chosen by
// `fn(param.value)`.
//
// TODO this offset is a hack to get scope state working. Simplify.
template <typename P, typename A, typename Fn>
void FillMissingArgs(ParamsRef<P> params, Arguments<A>* args, Fn fn,
                     size_t offset = 0) {
  for (size_t i = args->pos().size(); i < params.size(); ++i) {
    ASSERT(i + offset < params.size());
    auto const& p = params[i + offset];
    if (p.name.empty()) { continue; }
    LOG("fill-missing-args", "For named-parameter %s inserting.", p.name);
    args->named_emplace(p.name,
                        base::lazy_convert{[&]() { return fn(p.value); }});
  }
}

// Returns true if and only if a callable with `params` can be called with
// `args`.
template <typename T, typename U, typename ConvertibleFn>
bool IsCallable(ParamsRef<T> params, Arguments<U> const& args,
                ConvertibleFn fn) {
  if (params.size() < args.size()) {
    LOG("core::IsCallable",
        "IsCallable = false due to size mismatch (%u vs %u)", params.size(),
        args.size());
    return false;
  }

  for (size_t i = 0; i < args.pos().size(); ++i) {
    if (not fn(args.pos()[i], params[i].value)) {
      LOG("core::IsCallable",
          "IsCallable = false due to convertible failure at %u", i);
      return false;
    }
  }

  for (auto const& [name, type] : args.named()) {
    int index = params.index(name);
    if (index < 0) {
      LOG("core::IsCallable", "No such parameter named \"%s\"", name);
      return false;
    }

    if (not fn(type, params[index].value)) {
      LOG("core::IsCallable",
          "IsCallable = false due to convertible failure on \"%s\"", name);
      return false;
    }
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const& param = params[i];
    if (param.flags & HAS_DEFAULT) { continue; }
    if (args.at_or_null(param.name) == nullptr) {
      LOG("core::IsCallable",
          "No argument for non-default parameter named \"%s\"", param.name);
      return false;
    }
  }

  LOG("core::IsCallable", "Yes, it's callable");
  return true;
}

}  // namespace core

#endif  // ICARUS_CORE_CALL_H
