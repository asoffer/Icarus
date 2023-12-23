#if not defined(IC_XMACRO_TYPE_KIND)
#error `IC_XMACRO_TYPE_KIND` must be defined.
#endif  // not defined(IC_XMACRO_TYPE_KIND)

// Defines an X-macro enabling iteration over all kinds of types in the
// language.

IC_XMACRO_TYPE_KIND(Primitive)
IC_XMACRO_TYPE_KIND(Parameters)
IC_XMACRO_TYPE_KIND(Function)
IC_XMACRO_TYPE_KIND(Slice)
IC_XMACRO_TYPE_KIND(Pointer)
IC_XMACRO_TYPE_KIND(BufferPointer)
IC_XMACRO_TYPE_KIND(Opaque)
IC_XMACRO_TYPE_KIND(DependentFunction)

#undef IC_XMACRO_TYPE_KIND
