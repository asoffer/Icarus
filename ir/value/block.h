#ifndef ICARUS_IR_VALUE_BLOCK_H
#define ICARUS_IR_VALUE_BLOCK_H

#include <cstdint>
#include <limits>
#include <string_view>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"

namespace ir {

// Each scope consists of a sequence of blocks. Blocks are identified by their
// position in that sequence. It only makes sense to compare two blocks for
// equality if they belong to the same scope.
struct Block : base::Extend<Block, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  using representation_type = uint16_t;

  static constexpr std::string_view kAbslFormatString = "Block(%u)";

  explicit constexpr Block(representation_type value =
                               std::numeric_limits<representation_type>::max())
      : value_(value) {}

  static Block Invalid() { return Block(); }

  constexpr representation_type value() const { return value_; }

 private:
  friend base::EnableExtensions;

  representation_type value_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_BLOCK_H
