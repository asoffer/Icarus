// Constants for associativity
constexpr size_t left_assoc  = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc   = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask  = 3;

namespace Time {
  using Eval = int;
  constexpr Eval either  = 0x0;
  constexpr Eval compile = 0x1;
  constexpr Eval run     = 0x2;
  constexpr Eval mixed   = 0x3;
  constexpr Eval error   = 0x7;
}  // namespace Time

enum ValueFlag : char {
  Not, // Not yet computed
  In,  // Startet to compute
  Done // Finished computing
};

namespace Language {
enum class Terminal {
  ASCII, Char, Else, False, Hole, Int, Null, Ord, Real, Return, StringLiteral,
  True, Type, Uint
};

enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

} // namespace Lanugage

enum class Assign : char { Unset, Const, LVal, RVal };

#define NORMAL_FLAG IR::Value('\0')
#define RESTART_FLAG IR::Value('\1')
#define CONTINUE_FLAG IR::Value('\2')
#define REPEAT_FLAG IR::Value('\3')
#define BREAK_FLAG IR::Value('\4')
#define RETURN_FLAG IR::Value('\5')
