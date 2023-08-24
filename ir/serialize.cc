#include "ir/serialize.h"

#include "jasmin/serialization.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {

ModuleProto Serialize(Module& module) {
  ModuleProto proto;
  auto& initializer = *proto.mutable_initializer();
  initializer.set_parameters(0);
  initializer.set_returns(0);
  jasmin::Serialize(module.initializer, *initializer.mutable_content());
  return proto;
}

}  // namespace ic
