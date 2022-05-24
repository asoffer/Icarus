#ifndef ICARUS_COMPILER_BUILTIN_MODULE_H
#define ICARUS_COMPILER_BUILTIN_MODULE_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope_context.h"
#include "ir/value/slice.h"
#include "module/builtin.h"
#include "type/type.h"

namespace compiler {

// Returns a BuiltinModule consisting of all nodes built in as language
// intrinsics.
std::unique_ptr<module::BuiltinModule> MakeBuiltinModule();

}  // namespace compiler

#endif  // ICARUS_COMPILER_BUILTIN_MODULE_H
