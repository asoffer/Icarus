#define DEFINE_ERR(name, pos, str)                                             \
  AST::Node *name(NPtrVec &&nodes) {                                           \
    error_log.log(nodes[pos]->loc, str);                                       \
    return new AST::Identifier(nodes[pos]->loc, "invalid_node");               \
  }

template <size_t N> AST::Node *drop_all_but(NPtrVec &&nodes) {
  auto temp = nodes[N];
  assert(temp && "stolen pointer is null");
  nodes[N] = nullptr;
  return temp;
}

namespace ErrMsg {
AST::Node *NeedExprAtEndOfFileOrLine(NPtrVec &&nodes) {
  std::string msg("Expected expression at end of ");
  error_log.log(
      nodes[0]->loc,
      msg + ((nodes[0]->node_type() == Language::newline) ? "file." : "line."));
  return new AST::Identifier(nodes[0]->loc, "invalid_node");
}

namespace Unop {
AST::Node *TextNonExpr(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc,
                "'" + nodes[1]->token() + "' is a reserved keyword.");
  return new AST::Identifier(nodes[0]->loc, "invalid_node");
}
AST::Node *NonExpr(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected unary operator preceding '" +
                                   nodes[1]->token() + "'.");
  return new AST::Identifier(nodes[0]->loc, "invalid_node");
}
} // namespace Unop

namespace Binop {
DEFINE_ERR(InavlidLeft, 0,
           "Left-hand side of binary operator is not an expression.")
DEFINE_ERR(InavlidBoth, 1, "Neither side of binary operator is an expression")
DEFINE_ERR(InavlidRight, 2,
           "Right-hand side of binary operator is not an expression.")

DEFINE_ERR(LeftTextNonExpr, 0,
           "'" + nodes[0]->token() + "' is a reserved keyword.")
DEFINE_ERR(BothTextNonExpr, 2,
           "Both sides of binary operator are reserved keywords.")
DEFINE_ERR(RightTextNonExpr, 2,
           "'" + nodes[2]->token() + "' is a reserved keyword.")

DEFINE_ERR(TODOBetter, 1, "Invalid binary operator.")

AST::Node *InvalidLeftAndEnd(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc,
                "Left-hand side of binary operator is not an expression.");

  std::string msg("Expected expression at end of ");
  error_log.log(
      nodes[0]->loc,
      msg + ((nodes[0]->node_type() == Language::newline) ? "file." : "line."));
  return new AST::Identifier(nodes[0]->loc, "invalid_node");
}
} // namespace Binop
namespace Access {
DEFINE_ERR(InvalidLeft, 0, "Left-hand side of `.` must be an expression")
DEFINE_ERR(InvalidBoth, 1, "Neither side of `.` operator is an expression")
DEFINE_ERR(InvalidRight, 2, "Right-hand side of `.` must be an identifier")

} // namespace Access
} // namespace ErrMsg
#undef DEFINE_INVALID

namespace Language {
#define OP_BL indirection, dots, negation
#define OP_L not_operator, dereference, reserved_free, reserved_print
#define OP_B binop, fn_arrow, reserved_in

#define LEFT_UNOP OP_L, OP_BL
#define O_LEFT_UNOP Opt({LEFT_UNOP})

#define BINOP OP_B, OP_BL
#define O_BINOP Opt({BINOP})

#define O_BCOP                                                                 \
  Opt({BINOP, chainop, dot, tick, declop, fn_arrow, rocket_operator,           \
       assign_operator, reserved_in})

#define NON_ID_EXPR expression
#define O_NON_ID_EXPR Opt({NON_ID_EXPR})

#define EXPR NON_ID_EXPR, identifier
#define O_EXPR Opt({EXPR})

#define TEXT_NON_EXPR                                                          \
  reserved_break, reserved_if, reserved_else, reserved_for, reserved_while,    \
      reserved_continue, reserved_import, reserved_repeat, reserved_restart,   \
      reserved_return, reserved_print, reserved_free, reserved_in
#define O_TEXT_NON_EXPR Opt({TEXT_NON_EXPR})

#define NON_EXPR                                                               \
  TEXT_NON_EXPR, hashtag, right_paren, right_brace, right_bracket,            \
      declaration, semicolon, left_brace, /** Not sure from here on **/ binop, \
      tick, dot, declop, fn_arrow, rocket_operator, assign_operator, chainop,  \
      dots
#define O_NON_EXPR Opt({NON_EXPR})

#define NEWLINE_OR_EOF newline, eof
#define O_NEWLINE_OR_EOF Opt({NEWLINE_OR_EOF})

// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
static const std::vector<Rule> Rules = {
    Rule(0x01, expression, {O_LEFT_UNOP, O_EXPR}, AST::Unop::build),
    Rule(0x00, expression, {O_LEFT_UNOP, O_NEWLINE_OR_EOF},
         ErrMsg::NeedExprAtEndOfFileOrLine),
    Rule(0x01, expression, {O_LEFT_UNOP, O_TEXT_NON_EXPR},
         ErrMsg::Unop::TextNonExpr),
    Rule(0x02, expression, {O_LEFT_UNOP, O_NON_EXPR}, ErrMsg::Unop::NonExpr),

    Rule(0x00, expression, {O_EXPR, O_BINOP, O_EXPR}, AST::Binop::build),
    Rule(0x00, expression, {O_EXPR, Opt({chainop}), O_EXPR},
         AST::ChainOp::build),

    Rule(0x00, expression, {O_EXPR, O_BCOP, O_TEXT_NON_EXPR},
         ErrMsg::Binop::RightTextNonExpr),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, O_BCOP, O_EXPR},
         ErrMsg::Binop::LeftTextNonExpr),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, O_BCOP, O_TEXT_NON_EXPR},
         ErrMsg::Binop::BothTextNonExpr),
    Rule(0x01, expression, {O_TEXT_NON_EXPR, O_BCOP, O_NON_EXPR},
         ErrMsg::Binop::TODOBetter),
    Rule(0x02, expression, {O_NON_EXPR, O_BCOP, O_TEXT_NON_EXPR},
         ErrMsg::Binop::TODOBetter),
    Rule(0x02, expression, {O_EXPR, O_BCOP, O_NON_EXPR},
         ErrMsg::Binop::TODOBetter),
    Rule(0x02, expression, {O_NON_EXPR, O_BCOP, O_EXPR},
         ErrMsg::Binop::TODOBetter),
    Rule(0x02, expression, {O_NON_EXPR, O_BCOP, O_NON_EXPR},
         ErrMsg::Binop::TODOBetter),

    /*
        Rule(0xf0, expression, {O_NON_EXPR, Opt({OP_B}), O_EXPR},
             ErrMsg::Binop::InvalidLeft),
        Rule(0xf0, expression, {O_EXPR, Opt({OP_B}), O_NON_EXPR},
             ErrMsg::Binop::InvalidRight),
        Rule(0xf0, expression, {O_NON_EXPR, Opt({OP_B}), O_NON_EXPR},
             ErrMsg::Binop::InvalidBoth),

            Rule(0xf0, expression, {O_EXPR, Opt({BINOP, dot}),
       O_NEWLINE_OR_EOF},
                 ErrMsg::NeedExprAtEndOfFileOrLine),
            Rule(0xf0, expression, {O_NON_EXPR, Opt({BINOP, dot}),
           O_NEWLINE_OR_EOF},
                 ErrMsg::Binop::InvalidLeftAndEnd),

            Rule(0xf0, expression, {O_EXPR, Opt({dot}), Opt({identifier})},
                 AST::Access::build),
            Rule(0xf0, expression, {O_EXPR, Opt({dot}), O_NON_ID_EXPR},
                 ErrMsg::Access::InvalidRight),
            Rule(0xf0, expression, {O_EXPR, Opt({dot}), O_NON_EXPR},
                 ErrMsg::Access::InvalidRight),

            Rule(0xf0, expression, {O_NON_EXPR, Opt({dot}), Opt({identifier})},
                 ErrMsg::Access::InvalidLeft),
            Rule(0xf0, expression, {O_NON_EXPR, Opt({dot}), O_NON_ID_EXPR},
                 ErrMsg::Access::InvalidBoth),
            Rule(0xf0, expression, {O_NON_EXPR, Opt({dot}), O_NON_EXPR},
                 ErrMsg::Access::InvalidBoth),
        */
};

extern size_t precedence(Language::Operator op);
} // namespace Language

// This function determines if a shift should be done, even when a valid
// reduce is possible. Recall that the stack can never be empty, so calls to
// stack_.back() are always safe.

AST::Node *Parser::get(size_t n) { return stack_[stack_.size() - n]; }
Language::NodeType Parser::get_type(size_t n) { return get(n)->node_type(); }

bool Parser::should_shift() {
  // If the size is just 1, no rule will match so don't bother checking.
  if (stack_.size() < 2) { return true; }

  // We'll need these node types a lot, so lets make it easy to use
  const auto ahead_type = lookahead_->node_type();

  if ((get_type(2) & Language::OP_) && (ahead_type & Language::OP_)) {
    auto left_prec  = precedence(((AST::TokenNode *)get(2))->op);
    auto right_prec = precedence(((AST::TokenNode *)lookahead_)->op);

    if (left_prec < right_prec) { return true; }
    if (left_prec > right_prec) { return false; }
    return (left_prec & assoc_mask) == right_assoc;
  }


  return false;
}
