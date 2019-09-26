#include "ir/block_def.h"

namespace ir {
BlockDef const *BlockDef::Start() {
  static BlockDef const *b = new BlockDef({}, {});
  return b;
}
BlockDef const *BlockDef::Exit() {
  static BlockDef const *b = new BlockDef({}, {});
  return b;
}
}  // namespace ir
