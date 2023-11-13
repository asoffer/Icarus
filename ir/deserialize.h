#ifndef ICARUS_IR_DESERIALIZE_H
#define ICARUS_IR_DESERIALIZE_H

#include "google/protobuf/repeated_field.h"
#include "ir/dependent_modules.h"
#include "ir/module.h"
#include "ir/module.pb.h"
#include "nth/base/attributes.h"
#include "nth/debug/debug.h"

namespace ic {

struct Deserializer {
  Deserializer(DependentModules& dm NTH_ATTRIBUTE(lifetimebound))
      : dependent_modules_(dm) {}
  bool Deserialize(ModuleProto const& proto, ModuleId id, Module& module);
  bool DeserializeFunction(ModuleProto const& m, FunctionProto const& proto,
                           IrFunction& f);

  // Populates `dm` from the given collection of serialized modules. Serialized
  // modules must constitute all transitive dependencies of the
  // currently-being-compiled module and must be listed in a topologically
  // sorted order so that if `b` depends on `a`, then `a` appears before `b`.
  bool DeserializeDependentModules(std::span<ModuleProto const> protos,
                                   DependentModules& dm);

  Module const& current() const& {
    NTH_REQUIRE(current_ != nullptr);
    return *current_;
  }

 private:
  std::pair<ModuleId, Module const*> ResolveModule(ModuleId id) const;

  Module const* builtin_module_;
  Module const* current_;
  ModuleId current_id_ = ModuleId::Invalid();
  google::protobuf::RepeatedPtrField<std::string> const* dependencies_;
  DependentModules& dependent_modules_;
};

}  // namespace ic

#endif  // ICARUS_IR_DESERIALIZE_H
