#include "frontend/source/file_name.h"

namespace frontend {

base::expected<CanonicalFileName> CanonicalFileName::Make(FileName name) {
  return CanonicalFileName{name.value};
}

}  // namespace frontend
