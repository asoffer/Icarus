#if not defined(IC_XMACRO_ATOM)
#error `IC_XMACRO_ATOM` must be defined.
#endif

// Defines an X-macro enabling iteration over parse atoms in the language.
// Two arguments are provided to each macro invocation.
//
// Argument 1: The token representing the enumerator in the `Token::Kind`
// Argument 2: The token representing the enumerator in the
//             `ParseTree::Node::Kind`

IC_XMACRO_ATOM(Builtin, BuiltinLiteral)
IC_XMACRO_ATOM(True, BooleanLiteral)
IC_XMACRO_ATOM(False, BooleanLiteral)
IC_XMACRO_ATOM(StringLiteral, StringLiteral)
IC_XMACRO_ATOM(IntegerLiteral, IntegerLiteral)
IC_XMACRO_ATOM(Identifier, Identifier)

#define IC_XMACRO_PRIMITIVE_TYPE(kind, unused_symbol, unused_spelling)         \
  IC_XMACRO_ATOM(kind, TypeLiteral)
#include "common/language/primitive_types.xmacro.h"

#undef IC_XMACRO_ATOM
