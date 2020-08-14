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

inline constexpr cmd_index_t kReturnInstruction     = 0;
inline constexpr cmd_index_t kUncondJumpInstruction = 1;
inline constexpr cmd_index_t kCondJumpInstruction   = 2;
inline constexpr cmd_index_t kLoadInstructionNumber = 3;

inline constexpr auto kAddInstructionRange = CmdRange{
    .start  = 4,
    .length = 10,
};
inline constexpr auto kSubInstructionRange = CmdRange{
    .start  = kAddInstructionRange.end(),
    .length = 10,
};
inline constexpr auto kMulInstructionRange = CmdRange{
    .start  = kSubInstructionRange.end(),
    .length = 10,
};
inline constexpr auto kDivInstructionRange = CmdRange{
    .start  = kMulInstructionRange.end(),
    .length = 10,
};
inline constexpr auto kModInstructionRange = CmdRange{
    .start  = kDivInstructionRange.end(),
    .length = 8,
};
inline constexpr auto kLtInstructionRange = CmdRange{
    .start  = kModInstructionRange.end(),
    .length = 14,
};
inline constexpr auto kLeInstructionRange = CmdRange{
    .start  = kLtInstructionRange.end(),
    .length = 14,
};
inline constexpr auto kEqInstructionRange = CmdRange{
    .start  = kLeInstructionRange.end(),
    .length = 14,
};
inline constexpr auto kNeInstructionRange = CmdRange{
    .start  = kEqInstructionRange.end(),
    .length = 14,
};
inline constexpr auto kNegInstructionRange = CmdRange{
    .start  = kNeInstructionRange.end(),
    .length = 10,
};
inline constexpr auto kRegisterInstructionRange = CmdRange{
    .start  = kNegInstructionRange.end(),
    .length = 15,
};
inline constexpr auto kStoreInstructionRange = CmdRange{
    .start  = kRegisterInstructionRange.end(),
    .length = 17,
};
inline constexpr auto kPhiInstructionRange = CmdRange{
    .start  = kStoreInstructionRange.end(),
    .length = 16,
};
inline constexpr auto kSetReturnInstructionRange = CmdRange{
    .start  = kPhiInstructionRange.end(),
    .length = 25,
};
inline constexpr auto kCastInstructionRange = CmdRange{
    .start  = kSetReturnInstructionRange.end(),
    .length = 10,
};

inline constexpr cmd_index_t kEndRangedInstructions =
    kCastInstructionRange.end();
inline constexpr uint16_t kAdHocStart = kEndRangedInstructions;

inline constexpr cmd_index_t kNotInstructionNumber           = kAdHocStart + 0;
inline constexpr cmd_index_t kXorFlagsInstructionNumber      = kAdHocStart + 1;
inline constexpr cmd_index_t kAndFlagsInstructionNumber      = kAdHocStart + 2;
inline constexpr cmd_index_t kOrFlagsInstructionNumber       = kAdHocStart + 3;
inline constexpr cmd_index_t kPtrInstructionNumber           = kAdHocStart + 4;
inline constexpr cmd_index_t kBufPtrInstructionNumber        = kAdHocStart + 5;
inline constexpr cmd_index_t kGetReturnInstructionIndex      = kAdHocStart + 6;
inline constexpr cmd_index_t kOpaqueTypeInstructionNumber    = kAdHocStart + 7;
inline constexpr cmd_index_t kArrowInstructionNumber         = kAdHocStart + 8;
inline constexpr cmd_index_t kCallInstructionNumber          = kAdHocStart + 9;
inline constexpr cmd_index_t kLoadSymbolInstructionNumber    = kAdHocStart + 10;
inline constexpr cmd_index_t kArrayInstructionNumber         = kAdHocStart + 11;
inline constexpr cmd_index_t kStructInstructionNumber        = kAdHocStart + 12;
inline constexpr cmd_index_t kMakeBlockInstructionNumber     = kAdHocStart + 13;
inline constexpr cmd_index_t kMakeScopeInstructionNumber     = kAdHocStart + 14;
inline constexpr cmd_index_t kStructIndexInstructionNumber   = kAdHocStart + 15;
inline constexpr cmd_index_t kTupleIndexInstructionNumber    = kAdHocStart + 16;
inline constexpr cmd_index_t kPtrIncrInstructionNumber       = kAdHocStart + 17;
inline constexpr cmd_index_t kTupleInstructionNumber         = kAdHocStart + 18;
inline constexpr cmd_index_t kEnumerationInstructionNumber   = kAdHocStart + 19;
inline constexpr cmd_index_t kTypeInfoInstructionNumber      = kAdHocStart + 20;
inline constexpr cmd_index_t kTypeManipulationInstructionNumber =
    kAdHocStart + 21;
inline constexpr cmd_index_t kByteViewLengthInstructionNumber =
    kAdHocStart + 22;
inline constexpr cmd_index_t kByteViewDataInstructionNumber = kAdHocStart + 23;
inline constexpr cmd_index_t kDebugIrInstructionNumber      = kAdHocStart + 24;

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_OP_CODES_H
