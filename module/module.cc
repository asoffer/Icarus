#include "module/module.h"

#include "jasmin/serialization.h"
#include "module/module.pb.h"

namespace module {

bool Module::Serialize(std::ostream &output) const {
  internal_proto::Module proto;
  auto& initializer = *proto.mutable_initializer();
  initializer.set_parameters(initializer_.parameter_count());
  initializer.set_returns(initializer_.return_count());
  jasmin::Serialize(initializer_, *initializer.mutable_content());
  return initializer.SerializeToOstream(&output);
}

}  // namespace module
