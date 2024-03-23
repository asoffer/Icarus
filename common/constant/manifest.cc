#include "common/constant/manifest.h"

#include "common/internal/functions.h"
#include "common/internal/parameters.h"
#include "nth/debug/debug.h"
#include "nth/utility/no_destructor.h"

namespace ic {
namespace {

nth::NoDestructor<ConstantManifest> global_manifest_;

}  // namespace

ConstantManifest const& ConstantManifest::Global() {
  [[maybe_unused]] static const bool init = [&] {
    internal_common::InitializeParameters(global_manifest_->table());
    internal_common::InitializeFunctions(global_manifest_->table());
    return false;
  }();
  return *global_manifest_;
}

ConstantManifest& ConstantManifest::MutableGlobal() {
  return *global_manifest_;
}

Constant ConstantManifest::NextSlot() const {
  return Constant(components_.size());
}

ConstantComponent const& ConstantManifest::operator[](size_t n) const {
  NTH_REQUIRE((v.harden), n < components_.size());
  return components_[n];
}

void InsertIntoGlobalConstantManifest(ConstantCategory category, uint32_t n) {
  global_manifest_->components_.emplace_back(category, n);
  NTH_LOG("{} INserting. size  is now {}")<<={category, global_manifest_->components_.size()};
}

}  // namespace ic
