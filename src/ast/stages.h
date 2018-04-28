#ifndef ICARUS_AST_STAGES_H
#define ICARUS_AST_STAGES_H

inline constexpr int AssignScopeStage           = 0;
inline constexpr int StartTypeVerificationStage = 1;
inline constexpr int DoneTypeVerificationStage  = 2;
inline constexpr int StartBodyValidationStage   = 3;
inline constexpr int DoneBodyValidationStage    = 4;
inline constexpr int EmitStage                  = 5;

namespace AST {
struct StageRange {
  // Last stage completed so far.
  int low = -1;
  // Last stage you can safely compute.
  int high = std::numeric_limits<int>::max();
  static constexpr int Nothing() { return -1; }
  static constexpr int NoEmitIR() { return EmitStage - 1; }
};
}  // namespace AST
#endif  // ICARUS_AST_STAGES_H
