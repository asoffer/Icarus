#ifndef ICARUS_AST_STAGES_H
#define ICARUS_AST_STAGES_H

inline constexpr int AssignScopeStage           = 0;
inline constexpr int StartTypeVerificationStage = 1;
inline constexpr int DoneTypeVerificationStage  = 2;
inline constexpr int ValidatedStage             = 3;
inline constexpr int EmitStage                  = 4;

#endif // ICARUS_AST_STAGES_H
