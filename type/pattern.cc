#include "type/pattern.h"

#include "absl/container/flat_hash_map.h"
#include "common/constant/category.h"
#include "common/constant/entry.h"
#include "common/constant/manifest.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic::type {
namespace {

// Pointers, slices, and patterns are stored with a single component whose
// value is the index of the pointee-type.
nth::NoDestructor<absl::flat_hash_map<Type, Constant>> patterns_;

}  // namespace

Type PatternType::match_type() const {
  return Type(Type::from_index, ConstantManifest::Global()[index()].value());
}

PatternType Pattern(Type t) {
  auto [iter, inserted] = patterns_->emplace(t, Constant::NewSlot());
  if (inserted) {
    InsertIntoGlobalConstantManifest(ConstantCategory::PatternType, t.index());
  }
  return PatternType(iter->second.value());
}

}  // namespace ic::type
