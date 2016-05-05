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
DEFINE_ERR(RightNonId, 2, "Right-hand side of '" + nodes[1]->token() +
                              "' must be an identifier.")
DEFINE_ERR(RightTextNonId, 2,
           "Right-hand side of '" + nodes[1]->token() + "' is a reserved word.")
DEFINE_ERR(RightNonIdLeftReserved, 1,
           "Right-hand side of '" + nodes[1]->token() +
               "' must be an "
               "identifier. Left-hand side is a reserved word.")
DEFINE_ERR(RightTextNonIdLeftReserved, 1,
           "Both sides of '" + nodes[1]->token() + "' are reserved.")
DEFINE_ERR(RightNonIdLeftNonExpr, 1,
           "Right-hand side of '" + nodes[1]->token() +
               "' must be an "
               "identifier. Left-hand side must be an "
               "expression.")
DEFINE_ERR(RightTextNonIdLeftNonExpr, 1,
           "Right-hand side of '" + nodes[1]->token() +
               "' is a "
               "reserved word. Left-hand side must "
               "be an expression.")

} // namespace Access

namespace Declaration {
DEFINE_ERR(LeftNonId, 0, "Left-hand side of '" + nodes[1]->token() +
                             "' must be an identifier.")
DEFINE_ERR(LeftTextNonId, 0,
           "Left-hand side of '" + nodes[1]->token() + "' is a reserved word.")
DEFINE_ERR(LeftNonIdRightReserved, 1,
           "Left-hand side of '" + nodes[1]->token() +
               "' must be an identifier. Right-hand side is a reserved word.")
DEFINE_ERR(LeftTextNonIdRightReserved, 1,
           "Both sides of '" + nodes[1]->token() + "' are reserved.")
DEFINE_ERR(
    LeftNonIdRightNonExpr, 1,
    "Left-hand side of '" + nodes[1]->token() +
        "' must be an identifier. Right-hand side must be an expression.")
DEFINE_ERR(LeftTextNonIdRightNonExpr, 1,
           "Left-hand side of '" + nodes[1]->token() +
               "' is a reserved word. Right-hand side must be an expression.")
} // namespace Declaration

} // namespace ErrMsg
#undef DEFINE_INVALID

namespace Language {
#define OP_BL indirection, dots, negation
#define OP_L not_operator, dereference, reserved_free, reserved_print
#define OP_B binop, reserved_in, assign_operator

#define LEFT_UNOP OP_L, OP_BL
#define O_LEFT_UNOP Opt({LEFT_UNOP})

#define BINOP OP_B, OP_BL
#define O_BINOP Opt({BINOP})

#define O_BCOP                                                                 \
  Opt({BINOP, chainop, fn_arrow, rocket_operator, assign_operator, reserved_in})

#define NON_ID_EXPR expression, fn_expression, fn_literal
#define O_NON_ID_EXPR Opt({NON_ID_EXPR})

#define EXPR NON_ID_EXPR, identifier
#define O_EXPR Opt({EXPR})

#define TEXT_NON_EXPR                                                          \
  reserved_break, reserved_if, reserved_else, reserved_for, reserved_while,    \
      reserved_continue, reserved_import, reserved_repeat, reserved_restart,   \
      reserved_return, reserved_print, reserved_free, reserved_in,             \
      reserved_enum, reserved_struct, reserved_return
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

    Rule(0x00, fn_expression, {O_EXPR, Opt({fn_arrow}), O_EXPR},
         AST::Binop::build),
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

    Rule(0x00, expression, {O_EXPR, Opt({dot}), Opt({identifier})},
         AST::Access::build),
    Rule(0x00, expression, {O_EXPR, Opt({tick}), Opt({identifier})},
         AST::Declaration::BuildGenerate),

    // TODO move these out of Access namespace. They apply to ` as well.
    Rule(0x00, expression, {O_EXPR, Opt({dot, tick}), O_NON_ID_EXPR},
         ErrMsg::Access::RightNonId),
    Rule(0x00, expression, {O_EXPR, Opt({dot, tick}), O_TEXT_NON_EXPR},
         ErrMsg::Access::RightTextNonId),
    Rule(0x01, expression, {O_EXPR, Opt({dot, tick}), O_NON_EXPR},
         ErrMsg::Access::RightNonId),
    Rule(0x00, expression,
         {O_TEXT_NON_EXPR, Opt({dot, tick}), Opt({identifier})},
         ErrMsg::Binop::LeftTextNonExpr),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, Opt({dot, tick}), O_NON_ID_EXPR},
         ErrMsg::Access::RightNonIdLeftReserved),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, Opt({dot, tick}), O_TEXT_NON_EXPR},
         ErrMsg::Access::RightTextNonIdLeftReserved),
    Rule(0x01, expression, {O_TEXT_NON_EXPR, Opt({dot, tick}), O_NON_EXPR},
         ErrMsg::Access::RightNonId),
    Rule(0x01, expression, {O_NON_EXPR, Opt({dot, tick}), O_NON_ID_EXPR},
         ErrMsg::Access::RightNonId),
    Rule(0x01, expression, {O_NON_EXPR, Opt({dot, tick}), O_TEXT_NON_EXPR},
         ErrMsg::Access::RightTextNonId),
    Rule(0x02, expression, {O_NON_EXPR, Opt({dot, tick}), O_NON_EXPR},
         ErrMsg::Access::RightNonId),
    Rule(0x01, expression, {O_NON_EXPR, Opt({dot, tick}), Opt({identifier})},
         ErrMsg::Binop::TODOBetter),

    Rule(0x00, expression, {Opt({identifier}), Opt({declop}), O_EXPR},
         AST::Declaration::BuildBasic),

    Rule(0x00, expression, {O_NON_ID_EXPR, Opt({declop}), O_EXPR},
         ErrMsg::Declaration::LeftNonId),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, Opt({declop}), O_EXPR},
         ErrMsg::Declaration::LeftTextNonId),
    Rule(0x01, expression, {O_NON_EXPR, Opt({declop}), O_EXPR},
         ErrMsg::Declaration::LeftNonId),
    Rule(0x00, expression, {Opt({identifier}), Opt({declop}), O_TEXT_NON_EXPR},
         ErrMsg::Binop::RightTextNonExpr),
    Rule(0x00, expression, {O_NON_ID_EXPR, Opt({declop}), O_TEXT_NON_EXPR},
         ErrMsg::Declaration::LeftNonIdRightReserved),
    Rule(0x00, expression, {O_TEXT_NON_EXPR, Opt({declop}), O_TEXT_NON_EXPR},
         ErrMsg::Declaration::LeftTextNonIdRightReserved),
    Rule(0x01, expression, {O_NON_EXPR, Opt({declop}), O_TEXT_NON_EXPR},
         ErrMsg::Declaration::LeftNonId),
    Rule(0x01, expression, {O_NON_ID_EXPR, Opt({declop}), O_NON_EXPR},
         ErrMsg::Declaration::LeftNonId),
    Rule(0x01, expression, {O_TEXT_NON_EXPR, Opt({declop}), O_NON_EXPR},
         ErrMsg::Declaration::LeftTextNonId),
    Rule(0x02, expression, {O_NON_EXPR, Opt({declop}), O_NON_EXPR},
         ErrMsg::Declaration::LeftNonId),
    Rule(0x01, expression, {Opt({identifier}), Opt({declop}), O_NON_EXPR},
         ErrMsg::Binop::TODOBetter),

    // Haven't even considered errors below here

    Rule(0x01, expression, {Opt({left_paren}), O_EXPR, Opt({right_paren})},
         AST::Expression::parenthesize),
    Rule(0x00, expression,
         {O_EXPR, Opt({left_paren}), O_EXPR, Opt({right_paren})},
         AST::Binop::build_paren_operator),
    Rule(0x01, statements,
         {Opt({statements}), Opt({EXPR, reserved_return}), O_NEWLINE_OR_EOF},
         AST::Statements::build_more),
    Rule(0x02, statements, {Opt({EXPR, reserved_return}), O_NEWLINE_OR_EOF},
         AST::Statements::build_one),

    Rule(0x10, keep_current, {Opt({statements, left_brace}), O_NEWLINE_OR_EOF},

         drop_all_but<0>),
    Rule(0x10, keep_current, {Opt({newline}), Opt({statements, left_brace})},
         drop_all_but<1>),

    Rule(0x00, fn_literal, {Opt({fn_expression}), Opt({left_brace}),
                            Opt({statements}), Opt({right_brace})},
         AST::FunctionLiteral::build),
    Rule(0x00, fn_literal,
         {Opt({fn_expression}), Opt({left_brace}), Opt({right_brace})},
         AST::FunctionLiteral::build),

    Rule(0x00, expression, {Opt({reserved_enum}), Opt({left_brace}),
                            Opt({statements}), Opt({right_brace})},
         AST::EnumLiteral::build),
    Rule(0x00, expression,
         {Opt({reserved_enum}), Opt({left_brace}), Opt({right_brace})},
         AST::EnumLiteral::build),

    Rule(0x00, expression, {Opt({reserved_struct}), Opt({left_brace}),
                            Opt({statements}), Opt({right_brace})},
         AST::StructLiteral::build),
    Rule(0x00, expression,
         {Opt({reserved_struct}), Opt({left_brace}), Opt({right_brace})},
         AST::StructLiteral::build),

    Rule(0x00, program,
         {Opt({Language::bof}), Opt({statements}), Opt({Language::eof})},
         drop_all_but<1>),
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

  if (ahead_type == Language::left_brace &&
      (get_type(1) == Language::fn_expression ||
       get_type(1) == Language::reserved_struct ||
       get_type(1) == Language::reserved_enum)) {
    return true;
  }
  if (ahead_type == Language::right_paren) { return false; }

  if ((get_type(2) & Language::OP_) && (ahead_type & Language::OP_)) {
    auto left_prec  = precedence(((AST::TokenNode *)get(2))->op);
    auto right_prec = precedence(((AST::TokenNode *)lookahead_)->op);

    if (left_prec < right_prec) { return true; }
    if (left_prec > right_prec) { return false; }
    return (left_prec & assoc_mask) == right_assoc;
  }

  return false;
}
