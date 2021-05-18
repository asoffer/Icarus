#ifndef ICARUS_CORE_CALL_H
#define ICARUS_CORE_CALL_H

#include <algorithm>
#include <vector>

#include "absl/types/span.h"
#include "base/extend.h"
#include "base/extend/equality.h"
#include "core/arguments.h"
#include "core/dependency_node.h"
#include "core/params.h"

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

struct CallabilityResult
    : base::Extend<CallabilityResult, 1>::With<base::EqualityExtension> {
  // Error indicating that too many arguments have been provided for the
  // callable.
  struct TooManyArguments
      : base::Extend<TooManyArguments>::With<base::EqualityExtension> {
    size_t num_provided;
    size_t max_num_accepted;
  };

  // Error indicating that a parameter does not have an argument bound to it
  // nor does it have a default value.
  struct MissingNonDefaultableArguments {
    absl::flat_hash_set<std::string> names;

    friend bool operator==(MissingNonDefaultableArguments const& lhs,
                           MissingNonDefaultableArguments const& rhs) {
      if (lhs.names.size() != rhs.names.size()) { return false; }
      for (std::string_view name : lhs.names) {
        if (not rhs.names.contains(name)) { return false; }
      }
      return true;
    }
  };

  // Error indicating that the type of an argument an dthe parameter to which
  // it is bound are incompatible.
  struct TypeMismatch
      : base::Extend<TypeMismatch>::With<base::EqualityExtension> {
    std::variant<std::string, size_t> parameter, argument;
  };

  // Error indicating that a named argument has a name not matching any
  // parameter.
  struct NoParameterNamed
      : base::Extend<NoParameterNamed>::With<base::EqualityExtension> {
    std::string name;
  };

  // Error indicating that a named argument binds to a parameter before the
  // parameter to which the final positional argument is bound.
  struct PositionalArgumentNamed
      : base::Extend<PositionalArgumentNamed>::With<base::EqualityExtension> {
    size_t index;
    std::string name;
  };

  template <typename Fn>
  auto Visit(Fn&& f) const {
    return std::visit(std::forward<Fn>(f), data_);
  }

  // Not an error. The call is valid.
  struct Valid : base::Extend<Valid>::With<base::EqualityExtension> {};

  CallabilityResult() : data_(Valid{}) {}

  template <typename Error>
  CallabilityResult(Error&& e) : data_(std::forward<Error>(e)) {}

  bool ok() const { return std::holds_alternative<Valid>(data_); }

 private:
  friend base::EnableExtensions;

  std::variant<Valid, TooManyArguments, MissingNonDefaultableArguments,
               TypeMismatch, NoParameterNamed, PositionalArgumentNamed>
      data_;
};

// Returns true if and only if a callable with `params` can be called with
// `args`.
template <typename T, typename U>
CallabilityResult Callability(Params<T> const& params, Arguments<U> const& args,
                              std::invocable<U, T> auto fn) {
  if (params.size() < args.size()) {
    return CallabilityResult::TooManyArguments{
        .num_provided = args.size(), .max_num_accepted = params.size()};
  }

  for (size_t i = 0; i < args.pos().size(); ++i) {
    if (not fn(args.pos()[i], params[i].value)) {
      return CallabilityResult::TypeMismatch{.parameter = i, .argument = i};
    }
  }

  for (auto const& [name, type] : args.named()) {
    auto* index = params.at_or_null(name);
    if (not index) { return CallabilityResult::NoParameterNamed{.name = name}; }

    if (*index < args.pos().size()) {
      return CallabilityResult::PositionalArgumentNamed{.index = *index,
                                                        .name  = name};
    }

    if (not fn(type, params[*index].value)) {
      return CallabilityResult::TypeMismatch{.parameter = name,
                                             .argument  = name};
    }
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const& param = params[i];
    if (param.flags & HAS_DEFAULT) { continue; }
    if (args.at_or_null(param.name) == nullptr) {
      return CallabilityResult::MissingNonDefaultableArguments{
          .names = {param.name}};
    }
  }

  return {};
}

}  // namespace core

#endif  // ICARUS_CORE_CALL_H
