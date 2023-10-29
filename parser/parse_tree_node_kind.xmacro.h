#if not defined(IC_XMACRO_PARSE_TREE_NODE_KIND)
#define IC_XMACRO_PARSE_TREE_NODE_KIND(name)
#endif

#if not defined(IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND)
#define IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(name, type)                    \
  IC_XMACRO_PARSE_TREE_NODE_KIND(name)
#endif

#if not defined(IC_XMACRO_PARSE_TREE_NODE_SCOPE_START_KIND)
#define IC_XMACRO_PARSE_TREE_NODE_SCOPE_START_KIND(name)                       \
  IC_XMACRO_PARSE_TREE_NODE_KIND(name)
#endif

IC_XMACRO_PARSE_TREE_NODE_KIND(StatementSequence)
IC_XMACRO_PARSE_TREE_NODE_KIND(Statement)
IC_XMACRO_PARSE_TREE_NODE_KIND(Declaration)
IC_XMACRO_PARSE_TREE_NODE_KIND(Let)
IC_XMACRO_PARSE_TREE_NODE_KIND(Var)
IC_XMACRO_PARSE_TREE_NODE_KIND(DeclaredIdentifier)
IC_XMACRO_PARSE_TREE_NODE_KIND(ColonColonEqual)
IC_XMACRO_PARSE_TREE_NODE_KIND(ColonEqual)
IC_XMACRO_PARSE_TREE_NODE_KIND(ColonColon)
IC_XMACRO_PARSE_TREE_NODE_KIND(Colon)
IC_XMACRO_PARSE_TREE_NODE_SCOPE_START_KIND(ScopeStart)
IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(BooleanLiteral, type::Bool)
IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(IntegerLiteral, type::Integer)
IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(StringLiteral, type::Slice(type::Char))
IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(TypeLiteral, type::Type_)
IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(BuiltinLiteral, type::Module)
IC_XMACRO_PARSE_TREE_NODE_KIND(Identifier)
IC_XMACRO_PARSE_TREE_NODE_KIND(InfixOperator)
IC_XMACRO_PARSE_TREE_NODE_KIND(ExpressionPrecedenceGroup)
IC_XMACRO_PARSE_TREE_NODE_KIND(ExpressionGroup)
IC_XMACRO_PARSE_TREE_NODE_KIND(MemberExpression)
IC_XMACRO_PARSE_TREE_NODE_KIND(Import)
IC_XMACRO_PARSE_TREE_NODE_KIND(Pointer)
IC_XMACRO_PARSE_TREE_NODE_KIND(BufferPointer)
IC_XMACRO_PARSE_TREE_NODE_KIND(CallExpression)
IC_XMACRO_PARSE_TREE_NODE_KIND(InvocationArgumentStart)
IC_XMACRO_PARSE_TREE_NODE_KIND(FunctionTypeParameters)

#undef IC_XMACRO_PARSE_TREE_NODE_KIND
#undef IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND
#undef IC_XMACRO_PARSE_TREE_NODE_SCOPE_START_KIND
