#ifndef ICARUS_IR_EXECUTE_H
#define ICARUS_IR_EXECUTE_H

#include "ir/emit.h"
#include "ir/module.pb.h"

namespace ic {

ModuleProto Serialize(EmitContext& context);

}  // namespace ic

#endif  // ICARUS_IR_EXECUTE_H
