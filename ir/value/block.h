#ifndef ICARUS_IR_VALUE_BLOCK_H
#define ICARUS_IR_VALUE_BLOCK_H

#include <iostream>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"

namespace ir {
struct CompiledBlock;

struct Block : base::Extend<Block, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "Block(%p)";

  constexpr Block() : Block(nullptr) {}
  explicit constexpr Block(CompiledBlock *block) : block_(block) {}

 private:
  friend base::EnableExtensions;
  friend CompiledBlock;
  CompiledBlock *block_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_BLOCK_H
