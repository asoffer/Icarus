#ifndef ICARUS_PARSE_NODE_INDEX_H
#define ICARUS_PARSE_NODE_INDEX_H

#include <cstdint>

#include "common/strong_index_type.h"

namespace ic {

// Represents the location of a `ParseNode` within the `ParseTree`. A
// `ParseTree` is layed out linearly in memory so that traversing nodes in
// order represents a post-order traversal of the tree. See "parse/tree.h" for
// more details.
struct ParseNodeIndex : StrongIndexType<ParseNodeIndex, uint32_t, int32_t> {
  using StrongIndexType::StrongIndexType;

  static ParseNodeIndex Invalid();

  friend void NthPrint(auto &p, auto &f, ParseNodeIndex const &i) {
    p.write("ParseNodeIndex{");
    f(p, i.value());
    p.write("}");
  }
};

}  // namespace ic

#endif  // ICARUS_PARSE_NODE_INDEX_H
