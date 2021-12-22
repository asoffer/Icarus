#ifndef ICARUS_FRONTEND_LEX_TAG_H
#define ICARUS_FRONTEND_LEX_TAG_H

#include <iostream>

namespace frontend {

enum Tag : uint64_t {
  empty_parens = 1ull << 0,
  eof          = 1ull << 1,
  newline      = 1ull << 2,

  braced_stmts    = 1ull << 3,
  scope_expr      = 1ull << 4,
  block_expr      = 1ull << 5,
  paren_call_expr = 1ull << 6,  // Represents a call of the syntactic form a(b)
  full_call_expr = 1ull << 7,  // Represents a call of the syntactic form a'b(c)

  stmt            = 1ull << 8,
  expr            = 1ull << 9,
  fn_expr         = 1ull << 10,
  l_paren         = 1ull << 11,
  r_paren         = 1ull << 12,
  l_bracket       = 1ull << 13,
  r_bracket       = 1ull << 14,
  l_brace         = 1ull << 15,
  r_brace         = 1ull << 16,
  expr_list       = 1ull << 17,
  semicolon       = 1ull << 18,
  kw_block_head   = 1ull << 19,
  kw_struct       = 1ull << 20,
  hashtag         = 1ull << 21,
  op_r            = 1ull << 22,
  op_l            = 1ull << 23,
  op_b            = 1ull << 24,
  colon           = 1ull << 25,
  eq              = 1ull << 26,
  comma           = 1ull << 27,
  op_bl           = 1ull << 28,
  op_lt           = 1ull << 29,
  fn_arrow        = 1ull << 30,
  kw_block        = 1ull << 31,
  yield           = 1ull << 32,
  label           = 1ull << 33,
  sop_l           = 1ull << 34,
  sop_lt          = 1ull << 35,
  paren_expr      = 1ull << 36,
  dot             = 1ull << 37,
  stmt_list       = 1ull << 38,
  decl            = 1ull << 39,
  colon_eq        = 1ull << 40,
  decl_list       = 1ull << 41,
  assignment      = 1ull << 42,
  assignment_list = 1ull << 43,
  paren_decl_list = 1ull << 44,
  bracket_expr    = 1ull << 45,
  empty_brackets  = 1ull << 46,
  rocket          = 1ull << 47,
  tick            = 1ull << 48,
  kw_scope        = 1ull << 49,
  builtin_if      = 1ull << 50,
  builtin_while   = 1ull << 51,
};

inline std::ostream& operator<<(std::ostream& os, Tag t) {
  static constexpr std::array tag_strs = {
      "empty_parens",
      "eof",
      "newline",
      "braced_stmts",
      "scope_expr",
      "block_expr",
      "paren_call_expr",
      "full_call_expr",
      "stmt",
      "expr",
      "fn_expr",
      "l_paren",
      "r_paren",
      "l_bracket",
      "r_bracket",
      "l_brace",
      "r_brace",
      "expr_list",
      "semicolon",
      "kw_block_head",
      "kw_struct",
      "hashtag",
      "op_r",
      "op_l",
      "op_b",
      "colon",
      "eq",
      "comma",
      "op_bl",
      "op_lt",
      "fn_arrow",
      "kw_block",
      "yield",
      "label",
      "sop_l",
      "sop_lt",
      "paren_expr",
      "dot",
      "stmt_list",
      "decl",
      "colon_eq",
      "decl_list",
      "assignment",
      "assignment_list",
      "paren_decl_list",
      "bracket_expr",
      "empty_brackets",
      "rocket",
      "tick",
      "scope",
      "if",
      "while",
  };
  const char* sep = "";
  os << "Tag(";
  for (size_t i = 0; i < 52; ++i) {
    if (t & (1ull << i)) { os << std::exchange(sep, ", ") << tag_strs[i]; }
  }
  return os << ")";
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_TAG_H
