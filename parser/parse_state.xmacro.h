#if not defined(IC_XMACRO_PARSER_STATE)
#error `IC_XMACRO_PARSER_STATE` must be defined.
#endif

#if not defined(IC_XMACRO_PARSER_STATE_SEQUENCE)
#define IC_XMACRO_PARSER_STATE_SEQUENCE(kind, separator)                       \
  IC_XMACRO_PARSER_STATE(kind##Sequence)                                       \
  IC_XMACRO_PARSER_STATE(Resolve##kind##Sequence)                              \
  IC_XMACRO_PARSER_STATE(Subsequent##kind##Sequence)
#endif

IC_XMACRO_PARSER_STATE(Newlines)

IC_XMACRO_PARSER_STATE(Module)

IC_XMACRO_PARSER_STATE(Statement)
IC_XMACRO_PARSER_STATE_SEQUENCE(Statement, Newlines)

IC_XMACRO_PARSER_STATE(Declaration)
IC_XMACRO_PARSER_STATE(DeclaredSymbol)
IC_XMACRO_PARSER_STATE(ResolveInferredTypeDeclaration)
IC_XMACRO_PARSER_STATE(ResolveUninferredTypeDeclaration)
IC_XMACRO_PARSER_STATE(ResolveDefaultedDeclaration)

IC_XMACRO_PARSER_STATE(Expression)
IC_XMACRO_PARSER_STATE(MaybeCallTermSuffix)
IC_XMACRO_PARSER_STATE(MaybeMemberTermSuffix)
IC_XMACRO_PARSER_STATE(ResolveMemberTerm)
IC_XMACRO_PARSER_STATE(AtomicTerm)

IC_XMACRO_PARSER_STATE(ResolveParameters)
IC_XMACRO_PARSER_STATE(ResolveUnaryExpression)

IC_XMACRO_PARSER_STATE(MaybeInfix)
IC_XMACRO_PARSER_STATE(ResolveExpressionGroup)
IC_XMACRO_PARSER_STATE(ExpressionClosing)

#undef IC_XMACRO_PARSER_STATE
#undef IC_XMACRO_PARSER_STATE_SEQUENCE
