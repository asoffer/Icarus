#include "type/function.h"

#include "base/container/map.h"
#include "base/guarded.h"
#include "ir/val.h"

namespace type {

void GenericFunction::EmitAssign(const Type *from_type, ir::Val const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {}
void GenericFunction::EmitInit(ir::Register reg, Context *ctx) const {}
void GenericFunction::EmitDestroy(ir::Register reg, Context *ctx) const {}
ir::Val GenericFunction::PrepareArgument(const Type *t, const ir::Val &val,
                                  Context *ctx) const {
  NOT_YET();
}
void GenericFunction::EmitRepr(ir::Val const &id_val, Context *ctx) const {}

void GenericFunction::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

void Function::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

static base::guarded<base::map<base::vector<Type const *>,
                               base::map<base::vector<Type const *>, Function>>>
    funcs_;
Function const *Func(base::vector<Type const *> in,
                     base::vector<Type const *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

}  // namespace type
