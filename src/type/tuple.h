#ifndef ICARUS_TYPE_TUPLE_H
#define ICARUS_TYPE_TUPLE_H

#include "type.h"

namespace type {
struct Tuple : public Type {
  Tuple() = delete;
  ~Tuple() {}
  Tuple(std::vector<const Type *> entries) : entries_(std::move(entries)) {}
  virtual char *WriteTo(char *buf) const;
  virtual size_t string_size() const;
  virtual void EmitAssign(const Type *from_type, IR::Val from, IR::Val to,
                          Context *ctx) const { UNREACHABLE(); }
  virtual void EmitInit(IR::Val id_val, Context *ctx) const { UNREACHABLE(); }
  virtual void EmitDestroy(IR::Val id_val, Context *ctx) const { UNREACHABLE(); }
  virtual IR::Val EmitInitialValue(Context *ctx) const { UNREACHABLE(); }
  virtual IR::Val PrepareArgument(const Type *t, const IR::Val &val,
                                  Context *ctx) const { UNREACHABLE(); }
  virtual void EmitRepr(IR::Val id_val, Context *ctx) const { UNREACHABLE(); }
  virtual Cmp Comparator() const { UNREACHABLE(); }

#ifdef ICARUS_USE_LLVM
  virtual llvm::Type *llvm(llvm::LLVMContext &) const { UNREACHABLE(); }
#endif // ICARUS_USE_LLVM

  std::vector<const Type *> entries_;
};

const Type *Tup(std::vector<const Type *> entries);
}  // namespace type

#endif  // ICARUS_TYPE_TUPLE_H
