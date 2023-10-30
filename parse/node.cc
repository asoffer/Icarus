#include "node.h"

#include <limits>

namespace ic {

ParseNode::Index ParseNode::Index::Invalid() {
  return Index(std::numeric_limits<underlying_type>::max());
}

}  // namespace ic
