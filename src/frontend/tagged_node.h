#ifndef ICARUS_FRONTEND_TAGGED_NODE_H
#define ICARUS_FRONTEND_TAGGED_NODE_H

#include <memory>

struct TextSpan;

namespace frontend {
enum Tag : u64 {
  bof            = 1ull << 0,
  eof            = 1ull << 1,
  newline        = 1ull << 2,
  prog           = 1ull << 3,
  stmts          = 1ull << 4,
  braced_stmts   = 1ull << 5,
  expr           = 1ull << 6,
  fn_expr        = 1ull << 7,
  scope_expr     = 1ull << 8,
  l_paren        = 1ull << 9,
  r_paren        = 1ull << 10,
  l_bracket      = 1ull << 11,
  r_bracket      = 1ull << 12,
  l_brace        = 1ull << 13,
  r_brace        = 1ull << 14,
  l_ref          = 1ull << 15,
  semicolon      = 1ull << 16,
  kw_block_head  = 1ull << 17,
  l_double_brace = 1ull << 18,
  r_double_brace = 1ull << 19,
  op_r           = 1ull << 20,
  op_l           = 1ull << 21,
  op_b           = 1ull << 22,
  colon          = 1ull << 23,
  eq             = 1ull << 24,
  comma          = 1ull << 25,
  op_bl          = 1ull << 26,
  op_lt          = 1ull << 28,
  fn_arrow       = 1ull << 29,
  kw_block       = 1ull << 30
};

struct TaggedNode {
  std::unique_ptr<AST::Node> node_;
  Tag tag_ = bof;

  TaggedNode() = default;
  TaggedNode(std::unique_ptr<AST::Node> node, Tag tag)
      : node_(std::move(node)), tag_(tag) {}

  static TaggedNode TerminalExpression(const TextSpan &span, IR::Val val);
  TaggedNode(const TextSpan &span, const std::string &token, Tag tag);

  bool valid() const { return node_ != nullptr; }
  static TaggedNode Invalid() { return TaggedNode{}; }
};
}  // namespace frontend

#endif  // ICARUS_FRONTEND_TAGGED_NODE_H
