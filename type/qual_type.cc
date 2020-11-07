#include "type/qual_type.h"

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "base/global.h"
#include "type/type.h"

namespace type {
namespace internal_type {

static base::Global<absl::flat_hash_set<std::vector<Type>>> packs;

// Even on rehash the spans returned here are never moved.
absl::Span<Type const> AddPack(absl::Span<Type const> types) {
  return *packs.lock()->emplace(types.begin(), types.end()).first;
}

}  // namespace internal_type

std::ostream &operator<<(std::ostream &os, QualType q) {
  if (not q) { return os << "error"; }
  return os << q.quals() << "("
            << (q.expansion_size() == 1
                    ? q.type().to_string()
                    : absl::StrJoin(q.expanded(), ", ",
                                    [](std::string *out, type::Type t) {
                                      out->append(t.to_string());
                                    }))
            << ")";
}

}  // namespace type
