#include "ir/serialize.h"

#include "common/resources.h"
#include "ir/module.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/serialize.h"

namespace ic {
namespace {

void SerializeContent(jasmin::Value op_code_value,
                      InstructionProto& instruction,
                      std::span<jasmin::Value const>& immediate_values) {
  auto op_code_metadata = InstructionSet::OpCodeMetadata(op_code_value);
  auto op_code =
      static_cast<InstructionProto::OpCode>(op_code_metadata.op_code_value);
  size_t immediate_count = op_code_metadata.immediate_value_count;
  instruction.set_op_code(op_code);
  switch (op_code) {
    case InstructionProto::PUSH_STRING_LITERAL:
      instruction.mutable_content()->Add(resources.StringLiteralIndex(
          std::string(immediate_values[0].as<char const*>(),
                      immediate_values[1].as<size_t>())));
      break;
    case InstructionProto::PUSH_FUNCTION:
      instruction.mutable_content()->Add(
          global_function_registry
              .id(immediate_values[0].as<IrFunction const*>())
              .value());
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

void Serializer::SerializeFunction(IrFunction const& function,
                                   FunctionProto& proto) {
  auto& instructions = *proto.mutable_instructions();
  std::span<jasmin::Value const> raw_instructions = function.raw_instructions();
  while (not raw_instructions.empty()) {
    jasmin::Value op_code = raw_instructions.front();
    raw_instructions      = raw_instructions.subspan(1);
    SerializeContent(op_code, *instructions.Add(), raw_instructions);
  }
}

void Serializer::Serialize(Module& module, ModuleProto& proto) {
  for (auto const& m : resources.module_map) { *proto.add_modules() = m; }
  SerializeTypeSystem(*proto.mutable_type_system());

  for (auto const& f : module.functions()) {
    auto& proto_fn = *proto.add_functions();
    SerializeFunction(f, proto_fn);
  }
  for (auto const& s : resources.strings) { *proto.add_string_literals() = s; }
  for (auto const& [name, type] : resources.foreign_functions) {
    auto& f = *proto.add_foreign_functions();
    f.set_name(name);
    type::SerializeFunctionType(type, *f.mutable_type());
  }

  for (auto const& [id, entry] : module.entries()) {
    (*proto.mutable_identifiers())[id.value()] = std::string_view(id);
    auto& symbol = (*proto.mutable_exported_symbols())[id.value()];
    type::Serialize(entry.qualified_type.type(), *symbol.mutable_type());
    for (jasmin::Value v : entry.value) {
      uint64_t raw_value;
      if (entry.qualified_type.type().kind() == type::Type::Kind::Function) {
        raw_value =
            global_function_registry.id(v.as<IrFunction const*>()).value();
      } else {
        raw_value = v.raw_value();
      }
      symbol.add_content(raw_value);
    }
  }
}

}  // namespace ic
