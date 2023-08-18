#include "vm/serialization.h"

#include "vm/implementation.h"

namespace vm {

void Serialize(Function const& f, serialization::proto::Function& proto,
               SerializationState& state) {
  proto.set_parameters(f.parameter_count());
  proto.set_returns(f.return_count());
  jasmin::Serialize(internal::Impl(f.raw()), *proto.mutable_content(), state);
}

bool Deserialize(serialization::proto::Function const& proto, Function& f,
                 SerializationState& state) {
  Function new_f(proto.parameters(), proto.returns());
  bool success =
      jasmin::Deserialize(proto.content(), internal::Impl(new_f.raw()), state);
  if (success) { f = std::move(new_f); }
  return success;
}

void Serialize(FunctionTable const& from,
               serialization::proto::FunctionTable& to,
               SerializationState& state) {
  to.mutable_functions()->Reserve(from.functions_.size());
  for (auto const& function : from.functions_) {
    Serialize(function, *to.add_functions(), state);
  }
}

bool Deserialize(serialization::proto::FunctionTable const& from,
                 FunctionTable& to, module::UniqueId module_id,
                 SerializationState& state) {
  for (auto const& function : from.functions()) {
    to.emplace(function.parameters(), function.returns(), module_id);
  }

  for (size_t i = 0; i < to.functions_.size(); ++i) {
    if (not Deserialize(from.functions(i), to.functions_[i], state)) {
      return false;
    }
  }
  return true;
}

}  // namespace vm
