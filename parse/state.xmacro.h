#if not defined(IC_XMACRO_PARSER_STATE)
#error `IC_XMACRO_PARSER_STATE` must be defined.
#endif

IC_XMACRO_PARSER_STATE(Newlines)

IC_XMACRO_PARSER_STATE(Module)
IC_XMACRO_PARSER_STATE(ResolveModule)

IC_XMACRO_PARSER_STATE(Statement)
IC_XMACRO_PARSER_STATE(ResolveStatement)
IC_XMACRO_PARSER_STATE(StatementSequence)
IC_XMACRO_PARSER_STATE(BracedStatementSequence)
IC_XMACRO_PARSER_STATE(ResolveStatementSequence)
IC_XMACRO_PARSER_STATE(SubsequentStatementSequence)

IC_XMACRO_PARSER_STATE(IdentifierSequence)
IC_XMACRO_PARSER_STATE(BracedIdentifierSequence)
IC_XMACRO_PARSER_STATE(ResolveIdentifierSequence)
IC_XMACRO_PARSER_STATE(SubsequentIdentifierSequence)

IC_XMACRO_PARSER_STATE(InvocationArgumentSequence)
IC_XMACRO_PARSER_STATE(ResolveInvocationArgumentSequence)

IC_XMACRO_PARSER_STATE(IndexArgumentSequence)
IC_XMACRO_PARSER_STATE(ResolveIndexArgumentSequence)

IC_XMACRO_PARSER_STATE(Declaration)
IC_XMACRO_PARSER_STATE(ColonToEndOfDeclaration)
IC_XMACRO_PARSER_STATE(DeclaredSymbol)
IC_XMACRO_PARSER_STATE(ResolveUninferredTypeDeclaration)
IC_XMACRO_PARSER_STATE(ResolveDeclaration)

IC_XMACRO_PARSER_STATE(ParenthesizedExpression)
IC_XMACRO_PARSER_STATE(ResolveMemberTerm)

IC_XMACRO_PARSER_STATE(CommaSeparatedExpressionSequence)
IC_XMACRO_PARSER_STATE(CommaSeparatedDeclarationSequence)
IC_XMACRO_PARSER_STATE(ResolveFunctionTypeParameters)

IC_XMACRO_PARSER_STATE(ResolveAssignment)
IC_XMACRO_PARSER_STATE(TryAssignment)

IC_XMACRO_PARSER_STATE(FunctionLiteralReturnTypeStart)
IC_XMACRO_PARSER_STATE(FunctionLiteralBody)
IC_XMACRO_PARSER_STATE(ResolveFunctionLiteral)

IC_XMACRO_PARSER_STATE(ResolveScope)
IC_XMACRO_PARSER_STATE(ResolveScopeBlock)
IC_XMACRO_PARSER_STATE(ResolveScopeLiteral)

IC_XMACRO_PARSER_STATE(IfStatementTrueBranchStart)
IC_XMACRO_PARSER_STATE(ResolveIfStatement)
IC_XMACRO_PARSER_STATE(IfStatementTryElse)

IC_XMACRO_PARSER_STATE(WhileLoopBody)
IC_XMACRO_PARSER_STATE(ResolveWhileLoop)

IC_XMACRO_PARSER_STATE(ResolveEnumLiteral)

IC_XMACRO_PARSER_STATE(ResolveInterfaceLiteral)

IC_XMACRO_PARSER_STATE(ExtensionWithToEnd)
IC_XMACRO_PARSER_STATE(ResolveExtension)

IC_XMACRO_PARSER_STATE(ResolveReturn)

IC_XMACRO_PARSER_STATE(Atom)
IC_XMACRO_PARSER_STATE(SuffixOfCall)
IC_XMACRO_PARSER_STATE(NamedArgument)
IC_XMACRO_PARSER_STATE(TryTermSuffix)
IC_XMACRO_PARSER_STATE(TryPrefix)
IC_XMACRO_PARSER_STATE(TryInfix)
IC_XMACRO_PARSER_STATE(ResolveInfix)
IC_XMACRO_PARSER_STATE(Expression)

#define IC_XMACRO_PARSE_NODE_PREFIX_UNARY(node, unused_token,                  \
                                          unused_precedence)                   \
  IC_XMACRO_PARSER_STATE(Resolve##node)
#include "parse/node.xmacro.h"

IC_XMACRO_PARSER_STATE(ClosingParenthesis)
IC_XMACRO_PARSER_STATE(ClosingBrace)

#undef IC_XMACRO_PARSER_STATE
