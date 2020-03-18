#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include "ir/blocks/group.h"
#include "type/function.h"

namespace ir {

using CompiledFn = BlockGroup<type::Function>;

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_FN_H
