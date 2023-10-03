#ifndef ICARUS_IR_BUILTIN_MODULE_H
#define ICARUS_IR_BUILTIN_MODULE_H

#include "ir/module.h"
#include "ir/global_function_registry.h"
#include "lexer/token_buffer.h"

namespace ic {

Module BuiltinModule(TokenBuffer& token_buffer,
                     GlobalFunctionRegistry& registry);

}  // namespace ic

#endif  // ICARUS_IR_BUILTIN_MODULE_H
