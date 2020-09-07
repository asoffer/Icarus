#ifndef ICARUS_IR_INSTRUCTION_OP_CODES_H
#define ICARUS_IR_INSTRUCTION_OP_CODES_H

#include <cstddef>

namespace ir {
using cmd_index_t = uint16_t;

namespace internal {
struct CmdRange {
  constexpr bool contains(uint16_t n) const {
    return start <= n and n < start + length;
  }
  constexpr uint16_t end() const { return start + length; }

  uint16_t start, length;
};

inline constexpr cmd_index_t kReturnInstruction =
    std::numeric_limits<cmd_index_t>::max();
inline constexpr cmd_index_t kUncondJumpInstruction =
    std::numeric_limits<cmd_index_t>::max() - 1;
inline constexpr cmd_index_t kCondJumpInstruction =
    std::numeric_limits<cmd_index_t>::max() - 2;
inline constexpr cmd_index_t kLoadInstructionNumber =
    std::numeric_limits<cmd_index_t>::max() - 3;

inline constexpr auto kPhiInstructionRange = CmdRange{
    .start  = 0,
    .length = 16,
};
inline constexpr auto kSetReturnInstructionRange = CmdRange{
    .start  = kPhiInstructionRange.end(),
    .length = 25,
};
inline constexpr cmd_index_t kEndRangedInstructions =
    kSetReturnInstructionRange.end();
inline constexpr uint16_t kAdHocStart = kEndRangedInstructions;

inline constexpr cmd_index_t kGetReturnInstructionIndex      = kAdHocStart + 0;
inline constexpr cmd_index_t kCallInstructionNumber          = kAdHocStart + 1;
inline constexpr cmd_index_t kStructInstructionNumber        = kAdHocStart + 2;
inline constexpr cmd_index_t kMakeBlockInstructionNumber     = kAdHocStart + 3;
inline constexpr cmd_index_t kMakeScopeInstructionNumber     = kAdHocStart + 4;
inline constexpr cmd_index_t kEnumerationInstructionNumber   = kAdHocStart + 5;
inline constexpr cmd_index_t kTypeInfoInstructionNumber      = kAdHocStart + 6;
inline constexpr cmd_index_t kTypeManipulationInstructionNumber =
    kAdHocStart + 7;

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_OP_CODES_H
