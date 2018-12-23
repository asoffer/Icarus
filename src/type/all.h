#ifndef ICARUS_TYPE_ALL_H
#define ICARUS_TYPE_ALL_H

namespace type {
struct Array;
struct Enum;
struct Flags;
struct Function;
struct GenericStruct;
struct IncompleteStruct;
struct Pointer;
struct Struct;
struct Type;
struct Variant;
}  // namespace type

#include "base/types.h"

#include <iosfwd>
#include <string>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"

struct Context;

namespace ir {
struct Func;
}  // namespace ir

namespace ast {
struct Declaration;
struct Expression;
struct Identifier;
}  // namespace ast

// TODO names for argumentss of built-in functions (repr, assign, destroy, etc)
// should be better? Should be necessarily unnamed? Figure it out and be
// consistent.

#include "array.h"
#include "enum.h"
#include "flags.h"
#include "function.h"
#include "opaque.h"
#include "generic_struct.h"
#include "incomplete_struct.h"
#include "pointer.h"
#include "primitive.h"
#include "struct.h"
#include "tuple.h"
#include "variant.h"

#undef TYPE_FNS
#undef BASIC_METHODS
#undef ENDING

#endif  // ICARUS_TYPE_ALL_H
