#include "type/function.h"

#include "base/container/map.h"
#include "base/guarded.h"
#include "ir/cmd.h"
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

void Function::EmitAssign(Type const *from_type, ir::Val const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::AnyFunc>(), to);
}

void Function::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE();
}

void Function::EmitRepr(ir::Val const &, Context *ctx) const { UNREACHABLE(); }

void Function::WriteTo(std::string *result) const {
  if (input.empty()) {
    result->append("()");
  } else if (input.size() == 1 && !input[0]->is<Function>()) {
    input.at(0)->WriteTo(result);
  } else {
    result->append("(");
    input.at(0)->WriteTo(result);
    for (size_t i = 1; i < input.size(); ++i) {
      result->append(", ");
      input.at(i)->WriteTo(result);
    }
    result->append(")");
  }

  result->append(" -> ");

  if (output.empty()) {
    result->append("()");
  } else if (output.size() == 1) {
    output.at(0)->WriteTo(result);
  } else {
    result->append("(");
    output.at(0)->WriteTo(result);
    for (size_t i = 1; i < output.size(); ++i) {
      result->append(", ");
      output.at(i)->WriteTo(result);
    }
    result->append(")");
  }
}

}  // namespace type
