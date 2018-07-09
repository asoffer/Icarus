#ifndef ICARUS_IR_BLOCK_SEQUENCE_H
#define ICARUS_IR_BLOCK_SEQUENCE_H

#include "base/container/vector.h"

namespace AST {
struct BlockLiteral;
}  // namespace AST

namespace IR {
struct BlockSequence {
  base::vector<AST::BlockLiteral*> seq_;
};

inline bool operator==(const BlockSequence& lhs, const BlockSequence& rhs) {
  return lhs.seq_ == rhs.seq_;
}

// TODO not really comparable. just for variant? :(
inline bool operator<(const BlockSequence& lhs, const BlockSequence& rhs) {
  return lhs.seq_ < rhs.seq_;
}
}  // namespace IR

#endif  // ICARUS_IR_BLOCK_SEQUENCE_H
