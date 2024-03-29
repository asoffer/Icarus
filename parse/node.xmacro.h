// Defines an X-macro enabling iteration over all kinds of ParseNodes.
//
// Parse nodes are categorized hierarchically with `IC_XMACRO_PARSE_NODE` at the
// top (most general) of the hierarchy. The hierarchy is structured as follows,
// where each category is also categorized as the node above it.
//
// IC_XMACRO_PARSE_NODE
//  |- IC_XMACRO_PARSE_NODE_STATEMENT
//  |   |- IC_XMACRO_PARSE_NODE_DECLARATION
//  |- IC_XMACRO_PARSE_NODE_EXPRESSION
//      |- IC_XMACRO_PARSE_NODE_PREFIX_UNARY
//      |- IC_XMACRO_PARSE_NODE_CONSTANT
//
// Each of these macros is separately definable. Any left undefined will expand
// to nothing. This means that one could extract all constants simply by
// defining `IC_XMACRO_PARSE_NODE_CONSTANT`. Or one could define
// `IC_XMACRO_PARSE_NODE` to obtain all parse nodes. One could also define both
// to get all parse nodes but have specialized treatment for constant parse
// nodes.
//
#if not defined(IC_XMACRO_PARSE_NODE)
#define IC_XMACRO_PARSE_NODE(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE)

#if not defined(IC_XMACRO_PARSE_NODE_STATEMENT)
#define IC_XMACRO_PARSE_NODE_STATEMENT(name) IC_XMACRO_PARSE_NODE(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_STATEMENT)

#if not defined(IC_XMACRO_PARSE_NODE_DECLARATION)
#define IC_XMACRO_PARSE_NODE_DECLARATION(name) IC_XMACRO_PARSE_NODE(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_DECLARATION)

#if not defined(IC_XMACRO_PARSE_NODE_EXPRESSION)
#define IC_XMACRO_PARSE_NODE_EXPRESSION(name) IC_XMACRO_PARSE_NODE(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_EXPRESSION)

#if not defined(IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START)
#define IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(name) IC_XMACRO_PARSE_NODE(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START)

#if not defined(IC_XMACRO_PARSE_NODE_PREFIX_UNARY)
#define IC_XMACRO_PARSE_NODE_PREFIX_UNARY(name, token, precedence)             \
  IC_XMACRO_PARSE_NODE_EXPRESSION(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_PREFIX_UNARY)

#if not defined(IC_XMACRO_PARSE_NODE_CONSTANT)
#define IC_XMACRO_PARSE_NODE_CONSTANT(name, type)                              \
  IC_XMACRO_PARSE_NODE_EXPRESSION(name)
#endif  // not defined(IC_XMACRO_PARSE_NODE_CONSTANT)

IC_XMACRO_PARSE_NODE(Module)
IC_XMACRO_PARSE_NODE(ModuleStart)
IC_XMACRO_PARSE_NODE(StatementSequence)
IC_XMACRO_PARSE_NODE(DeclarationStart)
IC_XMACRO_PARSE_NODE(DeclaredIdentifier)
IC_XMACRO_PARSE_NODE(StatementStart)
IC_XMACRO_PARSE_NODE(ScopeStart)
IC_XMACRO_PARSE_NODE(AssignedValueStart)
IC_XMACRO_PARSE_NODE(Assignment)
IC_XMACRO_PARSE_NODE(IfStatementTrueBranchStart)
IC_XMACRO_PARSE_NODE(IfStatementFalseBranchStart)
IC_XMACRO_PARSE_NODE(WhileLoopBodyStart)
IC_XMACRO_PARSE_NODE(WhileLoopStart)
IC_XMACRO_PARSE_NODE(EnumLiteralStart)
IC_XMACRO_PARSE_NODE(ScopeBodyStart)
IC_XMACRO_PARSE_NODE(ScopeBlockStart)
IC_XMACRO_PARSE_NODE(ScopeLiteralStart)
IC_XMACRO_PARSE_NODE(ScopeBlock)
IC_XMACRO_PARSE_NODE(InfixOperator)
IC_XMACRO_PARSE_NODE(InvocationArgumentStart)
IC_XMACRO_PARSE_NODE(PrefixInvocationArgumentEnd)
IC_XMACRO_PARSE_NODE(NamedArgument)
IC_XMACRO_PARSE_NODE(NamedArgumentStart)
IC_XMACRO_PARSE_NODE(IndexArgumentStart)
IC_XMACRO_PARSE_NODE(FunctionTypeParameters)
IC_XMACRO_PARSE_NODE(FunctionLiteralSignature)
IC_XMACRO_PARSE_NODE(FunctionLiteralStart)
IC_XMACRO_PARSE_NODE(InterfaceLiteralStart)
IC_XMACRO_PARSE_NODE(ExtensionStart)
IC_XMACRO_PARSE_NODE(NoReturns)
IC_XMACRO_PARSE_NODE(Return)
IC_XMACRO_PARSE_NODE(Extension)
IC_XMACRO_PARSE_NODE(ExtendWith)
IC_XMACRO_PARSE_NODE_DECLARATION(Declaration)
IC_XMACRO_PARSE_NODE_STATEMENT(Statement)
IC_XMACRO_PARSE_NODE_STATEMENT(IfStatement)
IC_XMACRO_PARSE_NODE_STATEMENT(WhileLoop)
IC_XMACRO_PARSE_NODE_EXPRESSION(Binding)
IC_XMACRO_PARSE_NODE_EXPRESSION(ScopeLiteral)
IC_XMACRO_PARSE_NODE_EXPRESSION(InterfaceLiteral)
IC_XMACRO_PARSE_NODE_EXPRESSION(Identifier)
IC_XMACRO_PARSE_NODE_EXPRESSION(ExpressionPrecedenceGroup)
IC_XMACRO_PARSE_NODE_EXPRESSION(MemberExpression)
IC_XMACRO_PARSE_NODE_EXPRESSION(EnumLiteral)
IC_XMACRO_PARSE_NODE_EXPRESSION(EmptyParenthesis)

IC_XMACRO_PARSE_NODE_EXPRESSION(Scope)

IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Import, Import, Loosest)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Pointer, Star, TightUnary)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Slice, Backslash, TightUnary)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(BufferPointer, BracketedStar, TightUnary)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Address, Ampersand, TightUnary)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Deref, At, TightUnary)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Pattern, Tilde, Loosest)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY(Minus, Minus, TightUnary)

IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(ImportStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(PointerStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(SliceStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(BufferPointerStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(AddressStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(DerefStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(PatternStart)
IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START(MinusStart)

IC_XMACRO_PARSE_NODE_EXPRESSION(CallExpression)
IC_XMACRO_PARSE_NODE_EXPRESSION(IndexExpression)
IC_XMACRO_PARSE_NODE_EXPRESSION(FunctionLiteral)
IC_XMACRO_PARSE_NODE_CONSTANT(BooleanLiteral, type::Bool)
IC_XMACRO_PARSE_NODE_CONSTANT(NullTypeLiteral, type::NullType)
IC_XMACRO_PARSE_NODE_CONSTANT(IntegerLiteral, type::Integer)
IC_XMACRO_PARSE_NODE_CONSTANT(StringLiteral, type::Slice(type::Char))
IC_XMACRO_PARSE_NODE_CONSTANT(CharacterLiteral, type::Char)
IC_XMACRO_PARSE_NODE_CONSTANT(TypeLiteral, type::Type_)
IC_XMACRO_PARSE_NODE_CONSTANT(BuiltinLiteral, type::Module)

#undef IC_XMACRO_PARSE_NODE
#undef IC_XMACRO_PARSE_NODE_DECLARATION
#undef IC_XMACRO_PARSE_NODE_STATEMENT
#undef IC_XMACRO_PARSE_NODE_EXPRESSION
#undef IC_XMACRO_PARSE_NODE_PREFIX_UNARY
#undef IC_XMACRO_PARSE_NODE_PREFIX_UNARY_START
#undef IC_XMACRO_PARSE_NODE_CONSTANT
