#ifndef ICARUS_IR_SERIALIZE_H
#define ICARUS_IR_SERIALIZE_H

#include "ir/global_function_registry.h"
#include "ir/module.h"
#include "ir/module.pb.h"

namespace ic {

struct Serializer {
  explicit Serializer(GlobalFunctionRegistry const& registry)
      : registry_(registry) {}
  void Serialize(Module& module, ModuleProto& proto);
  void SerializeFunction(IrFunction const& function, FunctionProto& proto);

 private:
  GlobalFunctionRegistry const& registry_;
};

}  // namespace ic

#endif  // ICARUS_IR_SERIALIZE_H
