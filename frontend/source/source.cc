#include "frontend/source/source.h"

namespace frontend {

absl::flat_hash_set<std::unique_ptr<Source>> Source::sources_;

}  // namespace frontend
