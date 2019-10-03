#include "frontend/source/id.h"

#include <atomic>

namespace frontend {
static std::atomic<uint64_t> counter_ = 0;

SourceId SourceId::Make() { return SourceId(counter_++); }

}  // namespace frontend
