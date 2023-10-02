#include "ir/serialize.h"

#include "ir/module.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

void SerializeContent(jasmin::Value op_code_value, InstructionProto& instruction,
                      std::span<jasmin::Value const>& immediate_values) {
  auto op_code_metadata = InstructionSet::OpCodeMetadata(op_code_value);
  auto op_code =
      static_cast<InstructionProto::OpCode>(op_code_metadata.op_code_value);
  size_t immediate_count = op_code_metadata.immediate_value_count;
  instruction.set_op_code(op_code);
  switch (op_code) {
    case InstructionProto::PUSH_FUNCTION:
      instruction.mutable_content()->Add(0);
      break;
    default: {
      for (size_t i = 0; i < immediate_count; ++i) {
        instruction.mutable_content()->Add(immediate_values[i].raw_value());
      }
    };
  }
  immediate_values = immediate_values.subspan(immediate_count);
}

}  // namespace

ModuleProto Serialize(Module& module) {
  ModuleProto proto;
  auto& initializer = *proto.mutable_initializer();
  initializer.set_parameters(0);
  initializer.set_returns(0);
  auto& instructions = *initializer.mutable_instructions();

  std::span<jasmin::Value const> raw_instructions =
      module.initializer().raw_instructions();
  while (not raw_instructions.empty()) {
    jasmin::Value op_code = raw_instructions.front();
    raw_instructions      = raw_instructions.subspan(1);
    SerializeContent(op_code, *instructions.Add(), raw_instructions);
  }
  return proto;
}

}  // namespace ic
