#if not defined(IC_XMACRO_PARSE_TREE_NODE_KIND)
#error `IC_XMACRO_PARSE_TREE_NODE_KIND` must be defined.
#endif

IC_XMACRO_PARSE_TREE_NODE_KIND(StatementSequence)
IC_XMACRO_PARSE_TREE_NODE_KIND(Declaration)
IC_XMACRO_PARSE_TREE_NODE_KIND(BooleanLiteral)
IC_XMACRO_PARSE_TREE_NODE_KIND(IntegerLiteral)
IC_XMACRO_PARSE_TREE_NODE_KIND(TypeLiteral)

#undef IC_XMACRO_PARSE_TREE_NODE_KIND
