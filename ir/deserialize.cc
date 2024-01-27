#include "ir/deserialize.h"

#include "common/resources.h"
#include "jasmin/serialize/string_reader.h"
#include "jasmin/serialize/deserialize.h"
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

bool Deserializer::Deserialize(ModuleProto const& proto, ModuleId id,
                               Module& module) {
  current_      = &module;
  current_id_   = id;
  dependencies_ = &proto.modules();

  type::DeserializeTypeSystem(proto.type_system());

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

  jasmin::StringReader reader(proto.program());
  return jasmin::Deserialize(reader, global_program);
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
