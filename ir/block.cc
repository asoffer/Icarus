#include "ir/block.h"

namespace ir {
BlockDef const *BlockDef::Start() {
  static BlockDef b;
  return &b;
}
BlockDef const *BlockDef::Exit() {
  static BlockDef b;
  return &b;
}
}  // namespace ir
