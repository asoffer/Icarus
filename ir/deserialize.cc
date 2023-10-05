#include "ir/deserialize.h"

#include "common/resources.h"
#include "ir/builtin_module.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {

bool Deserializer::DeserializeFunction(ModuleProto const& m,
                                       FunctionProto const& proto,
                                       IrFunction& f) {
  for (auto const& instruction : proto.instructions()) {
    uint64_t op_code = static_cast<uint64_t>(instruction.op_code());

    jasmin::Value op_code_value = jasmin::Value::Uninitialized();
    op_code_value.set_raw_value(reinterpret_cast<uint64_t>(
        InstructionSet::InstructionFunction(op_code)));
    f.raw_append(op_code_value);
    switch (op_code) {
      case InstructionProto::PUSH_STRING_LITERAL: {
        auto const& string_literals = m.string_literals();
        if (instruction.content().size() != 1) { return false; }
        if (instruction.content()[0] >= string_literals.size()) {
          return false;
        }
        std::string_view s =
            resources.StringLiteral(resources.StringLiteralIndex(
                string_literals[instruction.content()[0]]));
        f.raw_append(jasmin::Value(s.data()));
        f.raw_append(jasmin::Value(s.size()));
      } break;
      case InstructionProto::PUSH_FUNCTION: {
        if (instruction.content().size() != 1) { return false; }
        uint64_t index                         = instruction.content()[0];
        [[maybe_unused]] uint32_t module_index = index >> 32;
        uint32_t function_index                = index & uint32_t{0xffffffff};
        NTH_REQUIRE(builtin_module_ != nullptr);
        auto entry = builtin_module_->Lookup(function_index);
        if (entry.qualified_type.type() == type::Error) { return false; }
        if (entry.value.size() != 1) { return false; }
        f.raw_append(entry.value[0]);
      } break;
      default: {
        auto op_code_metadata = InstructionSet::OpCodeMetadata(op_code_value);
        if (op_code_metadata.immediate_value_count !=
            instruction.content().size()) {
          return false;
        }
        for (uint64_t value : instruction.content()) {
          jasmin::Value v = jasmin::Value::Uninitialized();
          v.set_raw_value(value);
          f.raw_append(v);
        }
      } break;
    }
  }
  return true;
}

bool Deserializer::Deserialize(ModuleProto const& proto, Module& module) {
  if (proto.initializer().parameters() != 0) { return false; }
  if (proto.initializer().returns() != 0) { return false; }

  // Insert all functions, so that when we populate their bodies we have a
  // stable `IrFunction` to refer to.
  for (auto const& function : proto.functions()) {
    module.add_function(function.parameters(), function.returns());
  }

  if (not DeserializeFunction(proto, proto.initializer(), module.initializer())) {
    return false;
  }

  size_t i = 0;
  for (auto const& function : proto.functions()) {
    if (not DeserializeFunction(proto, function, module.functions()[i++])) {
      return false;
    }
  }
  return true;
}

bool Deserializer::DeserializeDependentModules(
    std::span<ModuleProto const> protos, DependentModules& dm) {
  NTH_REQUIRE(dm.count() == 0);
  dm.modules_.reserve(protos.size() + 1);
  dm.modules_.emplace_back(BuiltinModule(registry_));
  builtin_module_ = &dm.modules_[0];
  for (auto const& proto : protos) {
    if (not Deserialize(proto, dm.modules_.emplace_back(registry_))) {
      return false;
    }
  }
  return true;
}

}  // namespace ic
