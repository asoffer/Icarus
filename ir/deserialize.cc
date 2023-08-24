#include "ir/deserialize.h"

#include "jasmin/serialization.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {

bool Deserialize(ModuleProto const& proto, Module& module) {
  NTH_ASSERT(proto.initializer().parameters() == 0);
  NTH_ASSERT(proto.initializer().returns() == 0);
  return jasmin::Deserialize(proto.initializer().content(), module.initializer);
}

}  // namespace ic
