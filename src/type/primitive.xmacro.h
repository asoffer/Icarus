// TODO some of these are given a name but shouldn't have one! Anything with a
// hyphen.
PRIMITIVE_MACRO(Bool,       Bool,       bool)
PRIMITIVE_MACRO(Char,       Char,       char)
PRIMITIVE_MACRO(Code,       Code,       code)
PRIMITIVE_MACRO(Int,        Int,        int)
PRIMITIVE_MACRO(Real,       Real,       real)
PRIMITIVE_MACRO(Type_,      Type,       type)
PRIMITIVE_MACRO(Void,       Void,       void)
PRIMITIVE_MACRO(NullPtr,    NullPtr,    null-pointer)
PRIMITIVE_MACRO(EmptyArray, EmptyArray, empty-array)
PRIMITIVE_MACRO(Err,        Err,        -err)
PRIMITIVE_MACRO(String,     String,     string)
PRIMITIVE_MACRO(Generic,    Generic,    -generic)
// TODO this is a bad name and the ambiguity with the keyword needs to be fixed
PRIMITIVE_MACRO(Block,      Block,      bloc)
PRIMITIVE_MACRO(Module,     Module,     module)
