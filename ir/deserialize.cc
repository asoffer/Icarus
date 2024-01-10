#include "ir/deserialize.h"

#include "common/resources.h"
#include "ir/builtin_module.h"
#include "ir/foreign_function.h"
#include "jasmin/core/metadata.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/deserialize.h"

namespace ic {

std::pair<ModuleId, Module const*> Deserializer::ResolveModule(
    ModuleId id) const {
  if (id == ModuleId::Foreign()) { return std::make_pair(id, nullptr); }
  if (id == ModuleId::Current()) {
    return std::make_pair(current_id_, current_);
  }
  NTH_REQUIRE((v.harden), dependencies_ != nullptr);
  NTH_REQUIRE((v.harden), dependencies_->size() > id.value());
  auto resolved_id = resources.module_map[(*dependencies_)[id.value()]];
  return std::make_pair(resolved_id, &dependent_modules_[resolved_id]);
}

bool Deserializer::DeserializeFunction(ModuleProto const& module_proto,
                                       FunctionProto const& proto,
                                       IrFunction& f) {
  for (auto const& instruction : proto.instructions()) {
    uint64_t op_code = static_cast<uint64_t>(instruction.op_code());
    f.raw_append(jasmin::Metadata<InstructionSet>().metadata(op_code).function);
    switch (op_code) {
      case InstructionProto::PUSH_STRING_LITERAL: {
        auto const& string_literals = module_proto.string_literals();
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
        auto [id, m]   = ResolveModule(ModuleId(index >> 32));
        FunctionId function_id(id,
                               LocalFunctionId(index & uint32_t{0xffffffff}));
        if (function_id.module() == ModuleId::Builtin()) {
          NTH_REQUIRE((v.debug), builtin_module_ != nullptr);
          // TODO: Are local functions the same as module symbols?
          auto entry = builtin_module_->Lookup(
              Identifier(BuiltinNames()[function_id.local_function().value()]));
          if (entry.qualified_type.type() == type::Error) { return false; }
          if (entry.value.size() != 1) { return false; }
          f.raw_append(entry.value[0]);
        } else if (function_id.module() == ModuleId::Foreign()) {
          auto const& ff =
              module_proto
                  .foreign_functions()[function_id.local_function().value()];
          LocalFunctionId local_fn_id(
              ForeignFunctionIndex(module_proto.string_literals()[ff.name()],
                                   type::DeserializeFunctionType(
                                       ff.type(), module_proto.type_system())));

          f.raw_append(&LookupForeignFunction(local_fn_id).second);
        } else {
          f.raw_append(jasmin::Value(
              &m->functions()[function_id.local_function().value()]));
        }
      } break;
      case InstructionProto::PUSH_POINTER: {
        if (instruction.content().size() != 1) { return false; }
        uint64_t index        = instruction.content()[0];
        auto const& ptr       = module_proto.foreign_pointers()[index];
        std::string_view name = module_proto.string_literals()[ptr.name()];
        auto t                = type::Ptr(
                           type::Deserialize(ptr.pointee(), module_proto.type_system()));
        f.raw_append(LookupForeignPointer(ForeignPointerIndex(name, t)).second);
      } break;
      default: {
        auto op_code_metadata =
            jasmin::Metadata<InstructionSet>().metadata(op_code);
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

bool Deserializer::Deserialize(ModuleProto const& proto, ModuleId id,
                               Module& module) {
  current_      = &module;
  current_id_   = id;
  dependencies_ = &proto.modules();

  type::DeserializeTypeSystem(proto.type_system());

  // Insert all functions, so that when we populate their bodies we have a
  // stable `IrFunction` to refer to.
  for (auto const& function : proto.functions()) {
    module.add_function(current_id_, function.parameters(), function.returns());
  }

  for (std::string const& s : proto.string_literals()) {
    resources.StringLiteralIndex(s);
  }
  for (auto const& f : proto.foreign_functions()) {
    InsertForeignFunction(
        proto.string_literals()[f.name()],
        type::DeserializeFunctionType(f.type(), proto.type_system()), true);
  }

  for (auto const& ptr : proto.foreign_pointers()) {
    InsertForeignPointer(
        proto.string_literals()[ptr.name()],
        type::Ptr(type::Deserialize(ptr.pointee(), proto.type_system())));
  }

  auto const& identifiers = proto.identifiers();
  for (auto const& [id, exported_symbol] : proto.exported_symbols()) {
    auto id_iter = identifiers.find(id);
    if (id_iter == identifiers.end()) { return false; }
    absl::InlinedVector<jasmin::Value, 2> value;

    type::Type t =
        type::Deserialize(exported_symbol.type(), proto.type_system());
    if (t.kind() == type::Type::Kind::Function) {
      if (exported_symbol.content().size() != 1) { return false; }
      uint64_t n    = exported_symbol.content()[0];
      auto [mid, m] = ResolveModule(ModuleId(n >> 32));
      LocalFunctionId local_fn_id(n & uint64_t{0xffffffff});
      if (mid == ModuleId::Foreign()) {
        auto const& f = proto.foreign_functions()[local_fn_id.value()];
        local_fn_id   = LocalFunctionId(ForeignFunctionIndex(
              proto.string_literals()[f.name()],
              type::DeserializeFunctionType(f.type(), proto.type_system())));
      }

      value.push_back(
          &global_function_registry.function(FunctionId(mid, local_fn_id)));
    } else if (t.kind() == type::Type::Kind::Pointer) {
      if (exported_symbol.content().size() != 1) { return false; }
      uint64_t n    = exported_symbol.content()[0];
      value.push_back(global_pointer_registry.pointer(n));
    } else {
      for (uint64_t n : exported_symbol.content()) {
        jasmin::Value v = jasmin::Value::Uninitialized();
        v.set_raw_value(n);
        value.push_back(v);
      }
    }

    module.Insert(
        Identifier(id_iter->second),
        Module::Entry{.qualified_type = type::QualifiedType::Constant(t),
                      .value          = std::move(value)});
  }

  size_t i = 0;
  if (proto.functions().empty()) { return false; }
  auto const & init = proto.functions(0);
  if (proto.functions().empty()) { return false; }
  if (init.parameters() != 0) { return false; }
  if (init.returns() != 0) { return false; }
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
  uint32_t i = 0;
  for (auto const& proto : protos) {
    if (not Deserialize(proto, ModuleId(++i), dm.modules_.emplace_back())) {
      return false;
    }
  }
  return true;
}

}  // namespace ic
