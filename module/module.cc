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
  return proto.SerializeToOstream(&output);
}

std::optional<Module> Module::Deserialize(std::istream &input) {
  std::optional<Module> m;
  internal_proto::Module proto;
  if (not proto.ParseFromIstream(&input)) { return m; }
  m.emplace();
  jasmin::Deserialize(proto.initializer().content(), m->initializer_);
  return m;
}

}  // namespace module
