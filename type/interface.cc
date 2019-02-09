#include "type/interface.h"

#include <mutex>
#include <utility>

#include <unordered_map>
#include "base/guarded.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/val.h"
#include "misc/architecture.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

void Interface::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

bool Interface::matches(Type const *t) const {
  // TODO implement
  return true;
}

void Interface::WriteTo(std::string *result) const {
  result->append("intf.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

ir::Val Interface::PrepareArgument(Type const *t, ir::Val const &val,
                                   Context *ctx) const {
  UNREACHABLE();
}

void Interface::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                               ir::RegisterOr<ir::Addr> to,
                               Context *ctx) const {
  UNREACHABLE();
}

void Interface::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                               ir::RegisterOr<ir::Addr> to,
                               Context *ctx) const {
  UNREACHABLE();
}

void Interface::EmitRepr(ir::Val const &id_val, Context *ctx) const {
  UNREACHABLE();
}
}  // namespace type
