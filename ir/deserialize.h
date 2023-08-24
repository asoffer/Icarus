#ifndef ICARUS_IR_EXECUTE_H
#define ICARUS_IR_EXECUTE_H

#include "ir/module.h"
#include "ir/module.pb.h"

namespace ic {

bool Deserialize(ModuleProto const& proto, Module& module);

}  // namespace ic

#endif  // ICARUS_IR_EXECUTE_H
