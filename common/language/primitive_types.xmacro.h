// Defines an X-macro enabling iteration over all primitive types in the
// language. Three arguments are provided to each macro invocation of
// `IC_XMACRO_PRIMITIVE_TYPE`.
//
// Argument 1: The token representing the enumerator in
//             `type::PrimitiveType::Kind`.
// Argument 2: The global symbol in the `type` namespace used to reference this
//             type. Note that these two match identically with the exception of
//             `Type` vs. `Type_`, so as to avoid collision with the struct
//             `type::Type`.
// Argument 3: The spelling of the given type in the language.
//
// Additionally, the X-macros `IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY` and
// `IC_XMACRO_PRIMITIVE_TYPE_END_CATEGORY` define X-macros identifying the
// boundaries of type categories.

#if not defined(IC_XMACRO_PRIMITIVE_TYPE)
#define IC_XMACRO_PRIMITIVE_TYPE(token, cpp_spelling, ic_spelling)
#endif 

#if not defined(IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE)
#define IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(category, start, end)
#endif 

#if not defined(IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING)
#define IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(t, cpp)
#endif

IC_XMACRO_PRIMITIVE_TYPE(Bottom, Bottom, "bottom")
IC_XMACRO_PRIMITIVE_TYPE(Error, Error, "error")
IC_XMACRO_PRIMITIVE_TYPE(Bool, Bool, "bool")
IC_XMACRO_PRIMITIVE_TYPE(NullType, NullType, "nulltype")
IC_XMACRO_PRIMITIVE_TYPE(Char, Char, "char")
IC_XMACRO_PRIMITIVE_TYPE(Byte, Byte, "byte")
IC_XMACRO_PRIMITIVE_TYPE(Integer, Integer, "integer")
IC_XMACRO_PRIMITIVE_TYPE(I8, I8, "i8")
IC_XMACRO_PRIMITIVE_TYPE(I16, I16, "i16")
IC_XMACRO_PRIMITIVE_TYPE(I32, I32, "i32")
IC_XMACRO_PRIMITIVE_TYPE(I64, I64, "i64")
IC_XMACRO_PRIMITIVE_TYPE(U8, U8, "u8")
IC_XMACRO_PRIMITIVE_TYPE(U16, U16, "u16")
IC_XMACRO_PRIMITIVE_TYPE(U32, U32, "u32")
IC_XMACRO_PRIMITIVE_TYPE(U64, U64, "u64")
IC_XMACRO_PRIMITIVE_TYPE(F32, F32, "f32")
IC_XMACRO_PRIMITIVE_TYPE(F64, F64, "f64")
IC_XMACRO_PRIMITIVE_TYPE(Type, Type_, "type")
IC_XMACRO_PRIMITIVE_TYPE(Module, Module, "module")
// TODO: Unspellable primitive types should be designated separately so we don't
// have to work around the lexer's configuration.
IC_XMACRO_PRIMITIVE_TYPE(Interface_, Interface, "unspellable-interface")
IC_XMACRO_PRIMITIVE_TYPE(Scope_, Scope, "unspellable-scope")

IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(Integral, Integer, U64)
IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(SignedIntegral, Integer, I64)
IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(UnsignedIntegral, U8, U64)
IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(Numeric, Integer, F64)

IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Bool, bool)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Char, char)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Byte, std::byte)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Integer, ::ic::Integer)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(I8,  int8_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(I16, int16_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(I32, int32_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(I64, int64_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(U8,  uint8_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(U16, uint16_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(U32, uint32_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(U64, uint64_t)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(F32, float)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(F64, double)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Type,::ic::type::Type)
IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(Module, ::ic::ModuleId)

#undef IC_XMACRO_PRIMITIVE_TYPE
#undef IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE
#undef IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING
