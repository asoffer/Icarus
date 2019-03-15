#include "type/function.h"

#include "base/guarded.h"
#include "ir/cmd.h"
#include "ir/val.h"

namespace type {
Type const *Generic = new GenericFunction;

void GenericFunction::EmitCopyAssign(const Type *from_type,
                                     ir::Results const &from,
                                     ir::RegisterOr<ir::Addr> to,
                                     Context *ctx) const {}
void GenericFunction::EmitMoveAssign(const Type *from_type,
                                     ir::Results const &from,
                                     ir::RegisterOr<ir::Addr> to,
                                     Context *ctx) const {}
void GenericFunction::EmitInit(ir::Register reg, Context *ctx) const {}
void GenericFunction::EmitDestroy(ir::Register reg, Context *ctx) const {}
ir::Results GenericFunction::PrepareArgument(const Type *t,
                                             const ir::Results &val,
                                             Context *ctx) const {
  NOT_YET();
}
void GenericFunction::EmitRepr(ir::Results const &id_val, Context *ctx) const {}

void GenericFunction::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  NOT_YET();
}

layout::Bytes GenericFunction::bytes(layout::Arch const &) const {
  return layout::Host().ptr_bytes;
}

layout::Alignment GenericFunction::alignment(layout::Arch const &) const {
  return layout::Host().ptr_alignment;
}

Cmp GenericFunction::Comparator() const { return Cmp::None; }

void Function::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  NOT_YET();
}

static base::guarded<std::map<std::vector<Type const *>,
                               std::map<std::vector<Type const *>, Function>>>
    funcs_;
Function const *Func(std::vector<Type const *> in,
                     std::vector<Type const *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

void Function::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::AnyFunc>(0), to);
}

void Function::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::AnyFunc>(0), to);
}

void Function::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE();
}

void Function::EmitRepr(ir::Results const &, Context *ctx) const { UNREACHABLE(); }

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

ir::Results Function::PrepareArgument(Type const *from, ir::Results const &val,
                                      Context *ctx) const {
  if (this == from) {
    return val;
  } else {
    NOT_YET(this, from);
  }
}

layout::Bytes Function::bytes(layout::Arch const &a) const {
  return a.fn_ptr_bytes;
}

layout::Alignment Function::alignment(layout::Arch const &a) const {
  return a.fn_ptr_alignment;
}

Cmp Function::Comparator() const { return Cmp::None; }

}  // namespace type
