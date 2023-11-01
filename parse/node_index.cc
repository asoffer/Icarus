#include "parse/node_index.h"

#include <limits>

namespace ic {

ParseNodeIndex ParseNodeIndex::Invalid() {
  return ParseNodeIndex(std::numeric_limits<underlying_type>::max());
}

}  // namespace ic
