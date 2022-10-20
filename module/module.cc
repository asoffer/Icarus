#include "module/module.h"

#include "module/module.pb.h"

namespace module {

bool Module::Serialize(std::ostream &output) const {
  internal_proto::Module proto;
  auto& initializer = *proto.mutable_initializer();
  initializer.set_parameters(initializer_.parameter_count());
  initializer.set_returns(initializer_.return_count());
  initializer.set_returns(initializer_.return_count());
  auto& content = *initializer.mutable_content();
  std::span raw_instructions = initializer_.raw_instructions();
  content.resize(raw_instructions.size() * jasmin::ValueSize);
  char* ptr = content.data();
  for (jasmin::Value value : raw_instructions) {
    jasmin::Value::Store(value, ptr, jasmin::ValueSize);
    ptr += jasmin::ValueSize;
  }

  return initializer.SerializeToOstream(&output);
}

}  // namespace module
