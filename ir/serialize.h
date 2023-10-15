#ifndef ICARUS_IR_SERIALIZE_H
#define ICARUS_IR_SERIALIZE_H

#include "ir/global_function_registry.h"
#include "ir/module.h"
#include "ir/module.pb.h"

namespace ic {

struct Serializer {
  void Serialize(Module& module, ModuleProto& proto);
  void SerializeFunction(IrFunction const& function, FunctionProto& proto);
};

}  // namespace ic

#endif  // ICARUS_IR_SERIALIZE_H
