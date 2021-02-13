#include "type/qual_type.h"

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "base/global.h"
#include "type/type.h"

namespace type {
namespace internal_type {
namespace {

base::Global<absl::flat_hash_set<std::vector<Type>>> packs;

constexpr QualType kError;

}  // namespace

// Even on rehash the spans returned here are never moved.
absl::Span<Type const> AddPack(absl::Span<Type const> types) {
  return *packs.lock()->emplace(types.begin(), types.end()).first;
}

}  // namespace internal_type

absl::Span<type::QualType const> QualType::ErrorSpan() {
  return absl::MakeConstSpan(&internal_type::kError, 1);
}

std::ostream &operator<<(std::ostream &os, QualType q) {
  if (not q) { return os << "error"; }
  return os << q.quals() << "(" << q.type().to_string() << ")";
}

}  // namespace type
