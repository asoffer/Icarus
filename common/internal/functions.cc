#include "common/internal/functions.h"

#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"

namespace ic::internal_common {
namespace {

absl::flat_hash_set<size_t, FunctionHash, FunctionEq>& FunctionsImpl(
    std::vector<ConstantComponent> const* components) {
  // Functions are stored with a first component indicating the length followed
  // by that number of components. The number of components is twice the number
  // of parameters, where pairs of components in order represent the name (as an
  // identifier) followed by the parameters type index.
  static nth::NoDestructor<
      absl::flat_hash_set<size_t, FunctionHash, FunctionEq>>
      parameters_{0,
                  ([&] { NTH_REQUIRE((v.harden), components != nullptr); }(),
                   FunctionHash(*components)),
                  FunctionEq(*components)};
  return *parameters_;
}

}  // namespace

absl::flat_hash_set<size_t, FunctionHash, FunctionEq>& Functions() {
  return FunctionsImpl(nullptr);
}

void InitializeFunctions(std::vector<ConstantComponent> const& components) {
  (void)FunctionsImpl(&components);
}

}  // namespace ic::internal_common

