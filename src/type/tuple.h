#ifndef ICARUS_TYPE_TUPLE_H
#define ICARUS_TYPE_TUPLE_H

#include "type.h"

struct Architecture;

namespace type {
struct Tuple : public Type {
  Tuple() = delete;
  ~Tuple() {}
  Tuple(base::vector<Type const *> entries) : entries_(std::move(entries)) {}
  virtual char *WriteTo(char *buf) const;
  virtual size_t string_size() const;

  void EmitAssign(Type const *from_type, ir::Val const &from,
                  ir::RegisterOr<ir::Addr> to, Context *ctx) const;
  virtual void EmitInit(ir::Register reg, Context *ctx) const;
  virtual void EmitDestroy(ir::Register reg, Context *ctx) const {
    UNREACHABLE();
  }
  virtual ir::Val PrepareArgument(Type const *t, ir::Val const &val,
                                  Context *ctx) const;

  virtual void EmitRepr(ir::Val const &id_val, Context *ctx) const;

  virtual Cmp Comparator() const { UNREACHABLE(); }

  size_t offset(size_t n, Architecture const &arch) const;

  Type const *finalize();

#ifdef ICARUS_USE_LLVM
  virtual llvm::Type *llvm(llvm::LLVMContext &) const { UNREACHABLE(); }
#endif  // ICARUS_USE_LLVM

  base::vector<Type const *> entries_;

  mutable ir::Func *init_func_ = nullptr, *assign_func_ = nullptr,
                   *destroy_func_ = nullptr, *repr_func_ = nullptr;
};

Type const *Tup(base::vector<Type const *> entries);

}  // namespace type

#endif  // ICARUS_TYPE_TUPLE_H
