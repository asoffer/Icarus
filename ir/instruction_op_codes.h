#include <cstddef>

namespace ir {
using cmd_index_t = uint16_t;

namespace internal {
inline constexpr uint8_t kTypeBits    = 8;
inline constexpr uint16_t kAdHocStart = (18 << kTypeBits);

inline constexpr cmd_index_t kAddInstructionMask             = 0 << kTypeBits;
inline constexpr cmd_index_t kSubInstructionMask             = 1 << kTypeBits;
inline constexpr cmd_index_t kMulInstructionMask             = 2 << kTypeBits;
inline constexpr cmd_index_t kDivInstructionMask             = 3 << kTypeBits;
inline constexpr cmd_index_t kModInstructionMask             = 4 << kTypeBits;
inline constexpr cmd_index_t kLtInstructionMask              = 6 << kTypeBits;
inline constexpr cmd_index_t kLeInstructionMask              = 7 << kTypeBits;
inline constexpr cmd_index_t kEqInstructionMask              = 8 << kTypeBits;
inline constexpr cmd_index_t kNeInstructionMask              = 9 << kTypeBits;
inline constexpr cmd_index_t kNegInstructionMask             = 10 << kTypeBits;
inline constexpr cmd_index_t kPrintInstructionMask           = 11 << kTypeBits;
inline constexpr cmd_index_t kStoreInstructionMask           = 12 << kTypeBits;
inline constexpr cmd_index_t kPhiInstructionMask             = 13 << kTypeBits;
inline constexpr cmd_index_t kRegisterInstructionMask        = 14 << kTypeBits;
inline constexpr cmd_index_t kSetReturnInstructionMask       = 15 << kTypeBits;
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
inline constexpr cmd_index_t kVariantAccessInstructionNumber = kAdHocStart + 18;
inline constexpr cmd_index_t kTupleInstructionNumber         = kAdHocStart + 19;
inline constexpr cmd_index_t kVariantInstructionNumber       = kAdHocStart + 20;
inline constexpr cmd_index_t kEnumerationInstructionNumber   = kAdHocStart + 21;
inline constexpr cmd_index_t kTypeInfoInstructionNumber      = kAdHocStart + 22;
inline constexpr cmd_index_t kLoadInstructionNumber          = kAdHocStart + 23;
inline constexpr cmd_index_t kStructManipulationInstructionNumber =
    kAdHocStart + 24;

inline constexpr cmd_index_t kCastInstructionIndex     = 17 << kTypeBits;
inline constexpr cmd_index_t kDebugIrInstructionNumber = 254 << kTypeBits;

// Note: These are not used here but it's worthwhile to list them here so we
// know they're taken.
inline constexpr cmd_index_t kUncondJumpInstruction = (255 << kTypeBits) + 0;
inline constexpr cmd_index_t kCondJumpInstruction   = (255 << kTypeBits) + 1;
inline constexpr cmd_index_t kReturnInstruction     = (255 << kTypeBits) + 2;

}  // namespace internal
}  // namespace ir
