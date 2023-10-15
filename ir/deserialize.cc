#include "ir/deserialize.h"

#include "common/resources.h"
#include "ir/builtin_module.h"
#include "ir/foreign_function.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/deserialize.h"

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
        uint64_t index = instruction.content()[0];
        FunctionId function_id(ModuleId(index >> 32),
                               LocalFunctionId(index & uint32_t{0xffffffff}));
        if (function_id.module() == ModuleId::Builtin()) {
          NTH_REQUIRE((v.debug), builtin_module_ != nullptr);
          // TODO: Are local functions the same as module symbols?
          auto entry =
              builtin_module_->Lookup(function_id.local_function().value());
          if (entry.qualified_type.type() == type::Error) { return false; }
          if (entry.value.size() != 1) { return false; }
          f.raw_append(entry.value[0]);
        } else if (function_id.module() == ModuleId::Foreign()) {
          f.raw_append(
              &ForeignFunctions()[function_id.local_function().value()].second);
        } else {
          NTH_UNIMPLEMENTED();
        }
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

  if (not DeserializeFunction(proto, proto.initializer(),
                              module.initializer())) {
    return false;
  }

  for (std::string const& s : proto.string_literals()) {
    resources.StringLiteralIndex(s);
  }
  for (auto const& f : proto.foreign_functions()) {
    std::vector<type::ParametersType::Parameter> parameters;
    std::vector<type::Type> return_types;
    parameters.reserve(f.type().parameters().size());
    for (type::ParameterTypeProto const& p : f.type().parameters()) {
      parameters.push_back(
          {.name = p.name(), .type = type::Deserialize(p.type())});
    }
    return_types.reserve(f.type().returns().size());
    for (type::TypeProto const& t : f.type().returns()) {
      return_types.push_back(type::Deserialize(t));
    }
    auto fn_type = type::Function(type::Parameters(std::move(parameters)),
                                  std::move(return_types));
    resources.ForeignFunctionIndex(proto.string_literals()[f.name()], fn_type);
    // TODO: This breaks down when coalescing needs to happen.
    IrFunction const* fn =
        ForeignFunction(proto.string_literals()[f.name()], fn_type);
    // TODO: Handle errors.
    NTH_REQUIRE(fn != nullptr);
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
  dm.modules_.emplace_back(BuiltinModule());
  builtin_module_ = &dm.modules_[0];
  for (auto const& proto : protos) {
    if (not Deserialize(proto, dm.modules_.emplace_back())) { return false; }
  }
  return true;
}

}  // namespace ic
