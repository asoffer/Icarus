#include "type/opaque.h"

#include "absl/strings/str_format.h"
#include "base/debug.h"
#include "base/global.h"
#include "ir/interpreter/interpreter.h"
#include "type/system.h"

namespace type {

static base::Global<
    absl::flat_hash_map<std::pair<ir::ModuleId, uintptr_t>, Opaque const *>>
    used_;

// TODO: Using the address as the numeric id is problematic because it makes
// values uncachable.
Opaque const *Opaq(ir::ModuleId mod, uintptr_t numeric_id) {
  auto handle           = used_.lock();
  auto [iter, inserted] = handle->try_emplace(std::make_pair(mod, numeric_id));
  if (inserted) { iter->second = new Opaque(mod); }
  return iter->second;
}

Opaque::Opaque(ir::ModuleId mod)
    : LegacyType(IndexOf<Opaque>(),
                 LegacyType::Flags{.is_default_initializable = 0,
                                   .is_copyable              = 0,
                                   .is_movable               = 0,
                                   .has_destructor           = 0}),
      mod_(mod) {}

void Opaque::WriteTo(std::string *result) const {
  absl::StrAppendFormat(result, "opaque.%x", numeric_id());
}

core::Bytes Opaque::bytes(core::Arch const &a) const {
  UNREACHABLE("Must not request the size of an opaque type");
}

core::Alignment Opaque::alignment(core::Arch const &a) const {
  UNREACHABLE("Must not request the alignment of an opaque type");
}

Type OpaqueTypeInstruction::Resolve() const {
  auto *o = Allocate<Opaque>(mod.value());
  used_.lock()->emplace(
      std::make_pair(mod.value(), reinterpret_cast<uintptr_t>(o)), o);
  GlobalTypeSystem.insert(Type(o));
  return o;
}

bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                          OpaqueTypeInstruction const &inst) {
  ir::ModuleId mod = interpreter.frame().resolve(inst.mod);
  auto *o          = Allocate<Opaque>(mod);
  used_.lock()->emplace(std::make_pair(mod, reinterpret_cast<uintptr_t>(o)), o);
  GlobalTypeSystem.insert(Type(o));
  interpreter.frame().set(inst.result, Type(o));
  return true;
}

}  // namespace type
