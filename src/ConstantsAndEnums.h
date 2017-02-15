// Constants for associativity
constexpr size_t left_assoc  = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc   = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask  = 3;

enum class FileType { Bin, IR, Nat, None };

constexpr size_t FAIL = ~0ul;

namespace Time {
  using Eval = int;
  constexpr Eval either  = 0x0;
  constexpr Eval compile = 0x1;
  constexpr Eval run     = 0x2;
  constexpr Eval mixed   = 0x3;
  constexpr Eval error   = 0x7;
}  // namespace Time

enum class Order { Less, Equal, Greater };

namespace Language {
enum class Terminal {
  ASCII, Char, Else, False, Hole, Int, Null, Ord, Real, Return, StringLiteral,
  True, Type, Uint, Error
};

enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

} // namespace Lanugage

enum class Assign : char { Unset, Const, LVal, RVal };

#define NORMAL_FLAG IR::Value::Char('\0')
#define RESTART_FLAG IR::Value::Char('\1')
#define CONTINUE_FLAG IR::Value::Char('\2')
#define REPEAT_FLAG IR::Value::Char('\3')
#define BREAK_FLAG IR::Value::Char('\4')
#define RETURN_FLAG IR::Value::Char('\5')
