#ifndef ICARUS_CONSTANTS_AND_ENUMS_H
#define ICARUS_CONSTANTS_AND_ENUMS_H

#include <cstddef>

// Constants for associativity
constexpr size_t left_assoc  = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc   = 2;
constexpr size_t chain_assoc = 3;
constexpr size_t assoc_mask  = 3;

enum class FileType { Bin, IR, Nat, None };

constexpr size_t FAIL = ~0ul;

enum class Order { Less, Equal, Greater };

namespace Language {

enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

} // namespace Language

enum class Assign : char { Unset, Const, LVal, RVal };

namespace Language {
extern size_t precedence(Operator op);

enum NodeType : u64 {
  bof            = 1ull << 0,
  eof            = 1ull << 1,
  newline        = 1ull << 3,
  prog           = 1ull << 4,
  stmts          = 1ull << 5,
  braced_stmts   = 1ull << 6,
  expr           = 1ull << 7,
  fn_expr        = 1ull << 8,
  l_paren        = 1ull << 9,
  r_paren        = 1ull << 10,
  l_bracket      = 1ull << 11,
  r_bracket      = 1ull << 12,
  l_brace        = 1ull << 13,
  r_brace        = 1ull << 14,
  l_ref          = 1ull << 15,
  semicolon      = 1ull << 16,
  hashtag        = 1ull << 17,
  kw_expr_block  = 1ull << 18,
  kw_block       = 1ull << 19,
  kw_struct      = 1ull << 20,
  l_double_brace = 1ull << 21,
  r_double_brace = 1ull << 22,

  op_l     = 1ull << 23,
  op_b     = 1ull << 24,
  colon    = 1ull << 25,
  eq       = 1ull << 26,
  comma    = 1ull << 27,
  op_bl    = 1ull << 28,
  dots     = 1ull << 29,
  op_lt    = 1ull << 30,
  fn_arrow = 1ull << 31,
};

} // namespace Language
#endif // ICARUS_CONSTANTS_AND_ENUMS_H
