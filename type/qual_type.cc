#include "type/qual_type.h"

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "base/global.h"
#include "type/type.h"

namespace type {
namespace {

// TODO: Make this constexpr again.
QualType kError;

}  // namespace

absl::Span<type::QualType const> QualType::ErrorSpan() {
  return absl::MakeConstSpan(&kError, 1);
}

std::ostream &operator<<(std::ostream &os, QualType q) {
  if (not q) { return os << "error"; }
  return os << q.quals() << "(" << q.type() << ")";
}

}  // namespace type
