#ifndef ICARUS_IR_INSTRUCTION_OP_CODES_H
#define ICARUS_IR_INSTRUCTION_OP_CODES_H

#include <cstddef>

namespace ir {
using cmd_index_t = uint16_t;

namespace internal {

inline constexpr cmd_index_t kReturnInstruction =
    std::numeric_limits<cmd_index_t>::max();
inline constexpr cmd_index_t kUncondJumpInstruction =
    std::numeric_limits<cmd_index_t>::max() - 1;
inline constexpr cmd_index_t kCondJumpInstruction =
    std::numeric_limits<cmd_index_t>::max() - 2;
inline constexpr cmd_index_t kLoadInstructionNumber =
    std::numeric_limits<cmd_index_t>::max() - 3;
inline constexpr cmd_index_t kGetReturnInstruction =
    std::numeric_limits<cmd_index_t>::max() - 4;

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_OP_CODES_H
