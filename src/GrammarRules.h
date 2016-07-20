extern AST::Node *BuildBinaryOperator(NPtrVec &&nodes);
extern AST::Node *BuildKWExprBlock(NPtrVec &&nodes);
extern AST::Node *BuildKWExprBlockOneLiner(NPtrVec &&nodes);
extern AST::Node *BuildKWExprBlockNoLiner(NPtrVec &&nodes);
extern AST::Node *BuildKWBlock(NPtrVec &&nodes);
extern AST::Node *BuildKWBlockOneLiner(NPtrVec &&nodes);
extern AST::Node *BuildKWBlockNoLiner(NPtrVec &&nodes);
extern AST::Node *Parenthesize(NPtrVec &&nodes);
extern AST::Node *BuildEmptyParen(NPtrVec &&nodes);

#define RESERVED_MSG(index)                                                    \
  {/* Wrap in anonymous scope to ensure that identifier 'tok' isn't leaked */  \
    assert(nodes[index]->is_token_node());                                     \
    auto tok = ((AST::TokenNode *)nodes[index])->token;                        \
    Error::Log::Log(nodes[index]->loc, "'" + std::string(tok) +                \
                                           "' is a reserved "                  \
                                           "keyword.");                        \
  }

#define NOT_BINOP_MSG(index)                                                   \
  {/* Wrap in anonymous scope to ensure that identifier 'tok' isn't leaked */  \
    assert(nodes[index]->is_token_node());                                     \
    auto tok = ((AST::TokenNode *)nodes[index])->token;                        \
    Error::Log::Log(nodes[index]->loc, "Operator '" + std::string(tok) +       \
                                           "' is not a binary operator.");     \
  }

template <size_t N> AST::Node *drop_all_but(NPtrVec &&nodes) {
  auto temp = nodes[N];
  assert(temp && "stolen pointer is null");
  nodes[N] = nullptr;
  return temp;
}

template <typename T> static T *steal_node(AST::Node *&n) {
  auto temp = (T *)n;
  assert(temp && "stolen pointer is null");
  n = nullptr;
  return temp;
}

AST::Node *CombineColonEq(NPtrVec &&nodes) {
  auto tk_node   = (AST::TokenNode *)nodes[0];
  tk_node->token = ":=";
  tk_node->op    = Language::Operator::ColonEq;

  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

namespace ErrMsg {
AST::Node *EmptyFile(NPtrVec &&nodes) {
  Error::Log::Log(nodes[0]->loc, "File is empty.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

template <size_t PrevIndex> AST::Node *MaybeMissingComma(NPtrVec &&nodes) {
  Error::Log::MissingComma(nodes[PrevIndex]->loc);
  auto tk_node = new AST::TokenNode(nodes[PrevIndex]->loc, ",");
  return BuildBinaryOperator({steal_node<AST::Node>(nodes[PrevIndex]), tk_node,
                              steal_node<AST::Node>(nodes[PrevIndex + 1])});
}

template <size_t RTN, size_t RES> AST::Node *Reserved(NPtrVec &&nodes) {
  RESERVED_MSG(RES)
  return new AST::Identifier(nodes[RTN]->loc, "invalid_node");
}

template <size_t RTN, size_t RES1, size_t RES2>
AST::Node *BothReserved(NPtrVec &&nodes) {
  RESERVED_MSG(RES1)
  RESERVED_MSG(RES2)
  return new AST::Identifier(nodes[RTN]->loc, "invalid_node");
}

AST::Node *NonBinop(NPtrVec &&nodes) {
  NOT_BINOP_MSG(1)
  return new AST::Identifier(nodes[1]->loc, "invalid_node");
}

template <size_t RTN, size_t RES> AST::Node *NonBinopReserved(NPtrVec &&nodes) {
  NOT_BINOP_MSG(1)
  RESERVED_MSG(RES)
  return new AST::Identifier(nodes[RTN]->loc, "invalid_node");
}

AST::Node *NonBinopBothReserved(NPtrVec &&nodes) {
  RESERVED_MSG(0)
  NOT_BINOP_MSG(1)
  RESERVED_MSG(2)
  return new AST::Identifier(nodes[1]->loc, "invalid_node");
}

} // namespace ErrMsg
#undef RESERVED_MSG
#undef NOT_BINOP_MSG
namespace Language {
static constexpr unsigned int OP_B  = op_b | comma | dots | colon | eq;
static constexpr unsigned int OP_LT = op_lt | kw_else;
static constexpr unsigned int EXPR  = expr | fn_expr | kw_else;
// Used in error productions only!
static constexpr unsigned int RESERVED =
    kw_expr_block | kw_else | kw_block | kw_struct | op_lt;

// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
static const std::vector<Rule> Rules = {
    // Binary operators
    Rule(0x00, fn_expr, {EXPR, fn_arrow, EXPR}, BuildBinaryOperator),
    Rule(0x00, expr, {EXPR, (op_bl | OP_B), EXPR}, BuildBinaryOperator),

    Rule(0x00, op_b, {colon, eq}, CombineColonEq),

    // Right unary operator
    Rule(0x01, expr, {EXPR, dots}, AST::Unop::BuildDots),
    Rule(0x02, expr, {RESERVED, dots}, ErrMsg::Reserved<1, 0>),

    // Using fn_arrow with a reserved keyword
    Rule(0x00, fn_expr, {EXPR, fn_arrow, RESERVED}, ErrMsg::Reserved<1, 2>),
    Rule(0x00, fn_expr, {RESERVED, fn_arrow, EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(0x00, fn_expr, {RESERVED, fn_arrow, RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),

    // Using OP_B or OP_BL with a reserved keyword
    Rule(0x00, expr, {EXPR, (OP_B | op_bl | dots), RESERVED},
         ErrMsg::Reserved<1, 2>),
    Rule(0x01, expr, {RESERVED, (OP_B | op_bl), EXPR}, ErrMsg::Reserved<1, 0>),
    Rule(0x00, expr, {RESERVED, (OP_B | op_bl), RESERVED},
         ErrMsg::BothReserved<1, 0, 2>),

    // Unary operators
    // TODO why does this rule have lower prec than the rule marked XXX
    Rule(0x01, expr, {(op_l | op_bl | OP_LT), EXPR}, AST::Unop::BuildLeft),

    // Using OP_L with a reserved keyword
    Rule(0x02, expr, {(op_l | op_bl | OP_LT), RESERVED},
         ErrMsg::Reserved<0, 1>),

    // Using OP_L like an OP_B (maybe with reserved keywords)
    // XXX
    Rule(0x03, expr, {EXPR, op_l, EXPR}, ErrMsg::NonBinop),
    Rule(0x00, expr, {EXPR, op_l, RESERVED}, ErrMsg::NonBinopReserved<1, 2>),
    Rule(0x02, expr, {RESERVED, op_l, EXPR}, ErrMsg::NonBinopReserved<1, 0>),
    Rule(0x00, expr, {RESERVED, op_l, RESERVED}, ErrMsg::NonBinopBothReserved),

    // Call and Index operators
    Rule(0x00, expr, {EXPR, l_paren, EXPR, r_paren},
         AST::Binop::BuildCallOperator),
    Rule(0x00, expr, {EXPR, l_paren, r_paren}, BuildEmptyParen),
    Rule(0x00, expr, {EXPR, l_bracket, EXPR, r_bracket},
         AST::Binop::BuildIndexOperator),

    // Call and index operator with reserved words. We can't put reserved words
    // in the first slot because that might conflict with a real use case. For
    // example, "if(a)".
    Rule(0x00, expr, {EXPR, l_paren, RESERVED, r_paren},
         ErrMsg::Reserved<0, 2>),
    Rule(0x00, expr, {EXPR, l_bracket, RESERVED, r_bracket},
         ErrMsg::Reserved<0, 2>),

    // Parenthesization and bracketing (array literals)
    Rule(0x01, expr, {l_paren, EXPR, r_paren}, Parenthesize),
    Rule(0x01, expr, {l_bracket, EXPR, r_bracket}, AST::ArrayLiteral::build),
    Rule(0x00, expr, {l_bracket, r_bracket}, AST::ArrayLiteral::BuildEmpty),

    // TODO allow hashtags on any expression
    Rule(0x00, expr, {EXPR, hashtag}, AST::Expression::AddHashtag),

    // Maybe missing comma
    Rule(0x00, expr, {l_paren, EXPR, EXPR, r_paren},
         ErrMsg::MaybeMissingComma<1>),
    Rule(0x00, expr, {l_bracket, EXPR, EXPR, r_bracket},
         ErrMsg::MaybeMissingComma<1>),
    Rule(0x00, expr, {EXPR, l_bracket, EXPR, EXPR, r_bracket},
         ErrMsg::MaybeMissingComma<2>),

    // Parentheses and brackets around reserved words
    Rule(0x01, expr, {l_paren, RESERVED, r_paren}, ErrMsg::Reserved<1, 1>),
    Rule(0x01, expr, {l_bracket, RESERVED, r_bracket}, ErrMsg::Reserved<1, 1>),

    // Array type
    Rule(0x00, expr, {l_bracket, EXPR, semicolon, EXPR, r_bracket},
         AST::ArrayType::build),

    // Array type with reserved words
    Rule(0x00, expr, {l_bracket, EXPR, semicolon, RESERVED, r_bracket},
         ErrMsg::Reserved<0, 3>),
    Rule(0x00, expr, {l_bracket, RESERVED, semicolon, EXPR, r_bracket},
         ErrMsg::Reserved<0, 1>),
    Rule(0x00, expr, {l_bracket, RESERVED, semicolon, RESERVED, r_bracket},
         ErrMsg::BothReserved<0, 1, 3>),

    Rule(0x00, prog, {bof, eof}, ErrMsg::EmptyFile),
    Rule(0x00, prog, {bof, stmts, eof}, drop_all_but<1>),

    Rule(0x03, stmts, {op_lt}, AST::Jump::build),
    Rule(0x02, stmts,
         {(expr | fn_expr | kw_else | if_stmt | one_stmt), newline},
         AST::Statements::build_one),
    Rule(0x01, stmts,
         {stmts, (expr | fn_expr | stmts | if_stmt | one_stmt), newline},
         AST::Statements::build_more),

    Rule(0x02, comma, {comma, newline}, drop_all_but<0>),
    Rule(0x02, l_paren, {l_paren, newline}, drop_all_but<0>),
    Rule(0x02, l_bracket, {l_bracket, newline}, drop_all_but<0>),
    Rule(0x02, l_brace, {l_brace, newline}, drop_all_but<0>),
    Rule(0x02, stmts, {stmts, newline}, drop_all_but<0>),

    Rule(0x00, r_paren, {newline, r_paren}, drop_all_but<1>),
    Rule(0x00, r_bracket, {newline, r_bracket}, drop_all_but<1>),
    Rule(0x00, r_brace, {newline, r_brace}, drop_all_but<1>),
    Rule(0x00, l_brace, {newline, l_brace}, drop_all_but<1>),
    Rule(0x00, stmts, {newline, stmts}, drop_all_but<1>),

    Rule(0x00, r_paren, {r_paren, newline}, drop_all_but<0>),
    Rule(0x00, r_bracket, {r_bracket, newline}, drop_all_but<0>),
    Rule(0x00, r_brace, {r_brace, newline}, drop_all_but<0>),

    Rule(0x00, expr, {fn_expr, l_brace, stmts, r_brace},
         AST::FunctionLiteral::build),
    Rule(0x00, expr, {fn_expr, l_brace, (expr | fn_expr), r_brace},
         AST::FunctionLiteral::BuildOneLiner),
    Rule(0x00, expr, {fn_expr, l_brace, r_brace},
         AST::FunctionLiteral::BuildNoLiner),

    // TODO need single statement to be another type to make merging actually
    // work correctly.
    Rule(0x02, one_stmt, {kw_expr_block, EXPR, l_brace, stmts, r_brace},
         BuildKWExprBlock),
    Rule(0x02, one_stmt,
         {kw_expr_block, EXPR, l_brace, (expr | fn_expr), r_brace},
         BuildKWExprBlockOneLiner),
    Rule(0x02, one_stmt, {kw_expr_block, EXPR, l_brace, r_brace},
         BuildKWExprBlockNoLiner),

    Rule(0x02, if_stmt, {kw_if, EXPR, l_brace, stmts, r_brace},
         BuildKWExprBlock),
    Rule(0x02, if_stmt, {kw_if, EXPR, l_brace, (expr | fn_expr), r_brace},
         BuildKWExprBlockOneLiner),
    Rule(0x02, if_stmt, {kw_if, EXPR, l_brace, r_brace},
         BuildKWExprBlockNoLiner),

    Rule(0x02, expr, {kw_struct, EXPR, l_brace, stmts, r_brace},
         BuildKWExprBlock),
    Rule(0x02, expr, {kw_struct, EXPR, l_brace, r_brace},
         BuildKWExprBlockOneLiner),
    Rule(0x02, expr, {kw_struct, EXPR, l_brace, r_brace},
         BuildKWExprBlockNoLiner),

    Rule(0x01, if_stmt, {if_stmt, kw_else, if_stmt},
         AST::Conditional::build_else_if), // TODO stmts-> if_stmt
    Rule(0x01, if_stmt, {if_stmt, kw_else, l_brace, stmts, r_brace},
         AST::Conditional::build_else),
    Rule(0x01, if_stmt, {if_stmt, kw_else, l_brace, (expr | fn_expr), r_brace},
         AST::Conditional::BuildElseOneLiner),
    Rule(0x01, if_stmt, {if_stmt, kw_else, l_brace, r_brace},
         AST::Conditional::BuildElseNoLiner),

    // TODO missing first statement is an error-production

    Rule(0x01, expr, {(kw_block | kw_struct), l_brace, stmts, r_brace},
         BuildKWBlock),
    Rule(0x01, expr,
         {(kw_block | kw_struct), l_brace, (expr | fn_expr), r_brace},
         BuildKWBlockOneLiner),
    Rule(0x01, expr, {(kw_block | kw_struct), l_brace, r_brace},
         BuildKWBlockNoLiner),
};
} // namespace Language
