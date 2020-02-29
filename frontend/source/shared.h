#ifndef ICARUS_FRONTEND_SOURCE_SHARED_H
#define ICARUS_FRONTEND_SOURCE_SHARED_H

#include "frontend/source/source.h"

namespace frontend {

// Returns a single stub of a source that can be used in situations where we do
// not have a real source (e.g. if we want to emit a diagnostic because a source
// could not be loaded. This source cannot be read from nor can lines be loaded
// from it.
frontend::Source const *SharedSource();

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_SHARED_H
