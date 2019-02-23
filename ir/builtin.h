#ifndef ICARUS_IR_BUILTIN_H
#define ICARUS_IR_BUILTIN_H

#include <string>

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct AnyFunc;

enum class Builtin : char {
#define IR_BUILTIN_MACRO(enumerator, ...) enumerator,
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
};

type::Type const* BuiltinType(Builtin);
std::string stringify(Builtin);

AnyFunc DebugIrFn();
AnyFunc BytesFn();
AnyFunc AlignmentFn();

}  // namespace ir

#endif // ICARUS_IR_BUILTIN_H
