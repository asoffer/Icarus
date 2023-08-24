#ifndef ICARUS_IR_SERIALIZE_H
#define ICARUS_IR_SERIALIZE_H

#include "ir/module.h"
#include "ir/module.pb.h"

namespace ic {

ModuleProto Serialize(Module& module);

}  // namespace ic

#endif  // ICARUS_IR_SERIALIZE_H
