#include "frontend/source/file_name.h"

namespace frontend {

CanonicalFileName CanonicalFileName::Make(FileName name) {
  // TODO implement me.
  return CanonicalFileName{name.value};
}

}  // namespace frontend
