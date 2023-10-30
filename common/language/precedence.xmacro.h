#if not defined(IC_XMACRO_PRECEDENCE_GROUP)
#define IC_XMACRO_PRECEDENCE_GROUP(group)
#endif // not defined(IC_XMACRO_PRECEDENCE_GROUP)

#if not defined(IC_XMACRO_PRECEDENCE_ORDER)
#define IC_XMACRO_PRECEDENCE_ORDER(lower, higher)
#endif // not defined(IC_XMACRO_PRECEDENCE_ORDER)

// Defines an X-macro enabling iteration over precedence groups in the language.

// All precedence groups in the language.
IC_XMACRO_PRECEDENCE_GROUP(Function)
IC_XMACRO_PRECEDENCE_GROUP(Comparison)
IC_XMACRO_PRECEDENCE_GROUP(PlusMinus)
IC_XMACRO_PRECEDENCE_GROUP(Modulus)
IC_XMACRO_PRECEDENCE_GROUP(MultiplyDivide)
IC_XMACRO_PRECEDENCE_GROUP(TightUnary)

// Relationships generating the precedence ordering for precedence groups.
// Arguments are provided to the macro with the lower precedence group as the
// first argument and the higher precedence group as the second argument. Thus,
// for example, `Comparison` has weaker precedence than `PlusMinus`, indicating
// that the expression `a + b < c` is to be interpreted as `(a + b) < c` rather
// than `a + (b < c)`.
IC_XMACRO_PRECEDENCE_ORDER(Comparison, PlusMinus)
IC_XMACRO_PRECEDENCE_ORDER(PlusMinus, MultiplyDivide)
IC_XMACRO_PRECEDENCE_ORDER(Function, TightUnary)
IC_XMACRO_PRECEDENCE_ORDER(MultiplyDivide, TightUnary)

#undef IC_XMACRO_PRECEDENCE_GROUP
#undef IC_XMACRO_PRECEDENCE_ORDER
