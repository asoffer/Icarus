#ifndef ICARUS_TYPE_ALL_H
#define ICARUS_TYPE_ALL_H

namespace type {
struct Array;
struct Enum;
struct Function;
struct Pointer;
struct Range;
struct Scope;
struct Slice;
struct Struct;
struct Tuple;
struct Type;
struct Variant;
} // namespace type


extern type::Type *Err, *Unknown, *Bool, *Char, *Int, *Real, *Code, *Type_,
    *Void, *NullPtr, *String, *EmptyArray, *Generic;

#include "../ast/ast.h"
#include "../base/types.h"

#include <iosfwd>
#include <string>
#include <unordered_map>
#include <vector>

struct Context;

namespace IR {
struct Func;
} // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
} // namespace AST

// TODO names for argumentss of built-in functions (repr, assign, destroy, etc)
// should be better? Should be necessarily unnamed? Figure it out and be
// consistent.

#include "primitive.h"
#include "array.h"
#include "enum.h"
#include "function.h"
#include "pointer.h"
#include "range.h"
#include "scope.h"
#include "slice.h"
#include "struct.h"
#include "tuple.h"
#include "variant.h"

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

IR::Val PtrCallFix(IR::Val v);

#endif // ICARUS_TYPE_ALL_H
