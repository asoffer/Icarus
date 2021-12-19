OPERATOR_MACRO(Hashtag, "", hashtag, 0, non_assoc)
OPERATOR_MACRO(Import, "import", op_l, 1, non_assoc)
OPERATOR_MACRO(Return, "return", sop_lt, 1, non_assoc)
// TODO need to support rhs-only too (e.g., `#.label <<`)
OPERATOR_MACRO(Yield, "<<", yield, 1, non_assoc)
OPERATOR_MACRO(Comma, ",", comma, 2, chain_assoc)
OPERATOR_MACRO(VariadicPack, "..", op_l, 3, right_assoc)
OPERATOR_MACRO(Tilde, "~", op_bl, 4, non_assoc)
OPERATOR_MACRO(Assign, "=", eq, 4, non_assoc)
OPERATOR_MACRO(ColonEq, ":=", colon_eq, 4, non_assoc)
OPERATOR_MACRO(DoubleColonEq, "::=", colon_eq, 4, non_assoc)
OPERATOR_MACRO(Colon, ":", colon, 5, non_assoc)
OPERATOR_MACRO(DoubleColon, "::", colon, 5, non_assoc)
OPERATOR_MACRO(SymbolOrEq, "|=", op_b, 6, non_assoc)
OPERATOR_MACRO(SymbolXorEq, "^=", op_b, 7, non_assoc)
OPERATOR_MACRO(SymbolAndEq, "&=", op_b, 8, non_assoc)
OPERATOR_MACRO(AddEq, "+=", op_b, 9, non_assoc)
OPERATOR_MACRO(SubEq, "-=", op_b, 9, non_assoc)
OPERATOR_MACRO(MulEq, "*=", op_b, 10, non_assoc)
OPERATOR_MACRO(DivEq, "/=", op_b, 10, non_assoc)
OPERATOR_MACRO(ModEq, "%=", op_b, 10, non_assoc)
OPERATOR_MACRO(Rocket, "=>", rocket, 11, right_assoc)
OPERATOR_MACRO(Arrow, "->", fn_arrow, 11, right_assoc)
OPERATOR_MACRO(Or, "or", op_b, 12, chain_assoc)
OPERATOR_MACRO(Xor, "xor", op_b, 13, left_assoc)
OPERATOR_MACRO(And, "and", op_bl, 14, left_assoc)
OPERATOR_MACRO(Lt, "<", op_b, 15, chain_assoc)
OPERATOR_MACRO(Le, "<=", op_b, 15, chain_assoc)
OPERATOR_MACRO(Eq, "==", op_b, 15, chain_assoc)
OPERATOR_MACRO(Ne, "!=", op_b, 15, chain_assoc)
OPERATOR_MACRO(Ge, ">=", op_b, 15, chain_assoc)
OPERATOR_MACRO(Gt, ">", op_b, 15, chain_assoc)
OPERATOR_MACRO(Add, "+", op_b, 16, left_assoc)
OPERATOR_MACRO(Sub, "-", op_bl, 16, left_assoc)
OPERATOR_MACRO(Mul, "*", op_bl, 17, left_assoc)
OPERATOR_MACRO(Div, "/", op_b, 17, left_assoc)
OPERATOR_MACRO(Mod, "%", op_b, 17, left_assoc)
OPERATOR_MACRO(As, "as", op_b, 18, left_assoc)
OPERATOR_MACRO(SymbolOr, "|", op_b, 19, chain_assoc)
OPERATOR_MACRO(SymbolXor, "^", op_b, 20, left_assoc)
OPERATOR_MACRO(SymbolAnd, "&", op_bl, 21, left_assoc)
OPERATOR_MACRO(ArgType, "$", op_lt, 22, non_assoc)
OPERATOR_MACRO(Copy, "copy", op_l, 22, non_assoc)
OPERATOR_MACRO(Init, "init", op_l, 22, non_assoc)
OPERATOR_MACRO(Move, "move", op_l, 22, non_assoc)
OPERATOR_MACRO(Destroy, "destroy", op_l, 22, non_assoc)
OPERATOR_MACRO(Not, "not", op_l, 23, right_assoc)
OPERATOR_MACRO(TypeOf, ":?", op_r, 23, right_assoc)
OPERATOR_MACRO(BufPtr, "[*]", op_l, 23, right_assoc)
OPERATOR_MACRO(Backtick, "`", op_l, 23, right_assoc)
OPERATOR_MACRO(At, "@", op_l, 23, right_assoc)
OPERATOR_MACRO(Index, "[]", expr, 24, chain_assoc)
OPERATOR_MACRO(Call, "'", tick, 25, left_assoc)
OPERATOR_MACRO(NotAnOperator, "_", eof, 100, chain_assoc)
