#ifndef ICARUS_IR_CMD_SCOPE_H
#define ICARUS_IR_CMD_SCOPE_H

#include "absl/types/span.h"
// #include "compiler/compiler.h"
#include "ir/block_def.h"
#include "ir/cmd/util.h"

namespace compiler {
struct Compiler;
}  // namespace compiler

namespace ir {

struct BlockCmd {
  constexpr static cmd_index_t index = 39;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct ScopeCmd {
  constexpr static cmd_index_t index = 40;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

}  // namespace ir
#endif  // ICARUS_IR_CMD_SCOPE_H
