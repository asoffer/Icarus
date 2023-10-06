#if not defined(IC_XMACRO_TYPE_KIND)
#error `IC_XMACRO_TYPE_KIND` must be defined.
#endif  // not defined(IC_XMACRO_TYPE_KIND)

IC_XMACRO_TYPE_KIND(Primitive)
IC_XMACRO_TYPE_KIND(Parameters)
IC_XMACRO_TYPE_KIND(Function)
IC_XMACRO_TYPE_KIND(Slice)
IC_XMACRO_TYPE_KIND(Pointer)
IC_XMACRO_TYPE_KIND(BufferPointer)
IC_XMACRO_TYPE_KIND(Pattern)
IC_XMACRO_TYPE_KIND(GenericFunction)

#undef IC_XMACRO_TYPE_KIND
