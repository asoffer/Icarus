#ifndef ICARUS_NNT
#define ICARUS_NNT

#include <memory>
#include "text_span.h"

namespace Language {
inline constexpr size_t left_assoc  = 0;
inline constexpr size_t right_assoc = 1;
inline constexpr size_t non_assoc   = 2;
inline constexpr size_t chain_assoc = 3;
inline constexpr size_t assoc_mask  = 3;

// Associativity stored in the lowest two bits.
size_t precedence(Operator op);

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
  kw_expr_block  = 1ull << 17,
  kw_block       = 1ull << 18,
  kw_struct      = 1ull << 19,
  l_double_brace = 1ull << 20,
  r_double_brace = 1ull << 21,

  op_l     = 1ull << 22,
  op_b     = 1ull << 23,
  colon    = 1ull << 24,
  eq       = 1ull << 25,
  comma    = 1ull << 26,
  op_bl    = 1ull << 27,
  dots     = 1ull << 28,
  op_lt    = 1ull << 29,
  fn_arrow = 1ull << 30,
};
} // namespace Language

struct NNT {
  NNT() = default;
  std::unique_ptr<AST::Node> node = nullptr;
  Language::NodeType node_type = Language::bof;

  static NNT TerminalExpression(const TextSpan &span, IR::Val val) {
    return NNT(std::make_unique<AST::Terminal>(span, val), Language::expr);
  }

  NNT(const TextSpan &span, const std::string &token, Language::NodeType nt);

  NNT(std::unique_ptr<AST::Node> n, Language::NodeType nt)
      : node(std::move(n)), node_type(nt) {}
  static NNT Invalid() {
    // Name of this function is clearer than just using default constructor
    return NNT();
  }
};

inline bool operator==(const NNT& lhs, const NNT& rhs) {
  return lhs.node.get() == rhs.node.get() && lhs.node_type == rhs.node_type;
}
inline bool operator!=(const NNT& lhs, const NNT& rhs) { return (lhs == rhs); }

#endif // ICARUS_NNT
