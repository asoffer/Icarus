#ifndef ICARUS_TYPE_TUPLE_H
#define ICARUS_TYPE_TUPLE_H

#include "base/lazy.h"
#include "ir/any_func.h"
#include "type/type.h"

namespace type {
struct Tuple : public Type {
  Tuple() = delete;
  ~Tuple() {}
  Tuple(std::vector<Type const *> entries) : entries_(std::move(entries)) {}
  void WriteTo(std::string *result) const override;

  void EmitCopyAssign(Type const *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;
  void EmitMoveAssign(Type const *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;
   void EmitInit(ir::Reg reg, Context *ctx) const override;
   void EmitDestroy(ir::Reg reg, Context *ctx) const override;
   ir::Results PrepareArgument(Type const *t, ir::Results const &val,
                                      Context *ctx) const override;

  void EmitRepr(ir::Results const &id_val, Context *ctx) const override;

  virtual Cmp Comparator() const { UNREACHABLE(); }

  virtual void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const;

  bool ReinterpretAs(Type const *t) const override;

  core::Bytes offset(size_t n, core::Arch const &arch) const;
  size_t size() const { return entries_.size(); }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool IsCopyable() const override;
  bool IsMovable() const override;

  Type const *finalize();
  bool needs_destroy() const override;

#ifdef ICARUS_USE_LLVM
  virtual llvm::Type *llvm(llvm::LLVMContext &) const { UNREACHABLE(); }
#endif  // ICARUS_USE_LLVM

  std::vector<Type const *> entries_;

  base::lazy<ir::CompiledFn *> destroy_func_;
  base::lazy<ir::CompiledFn *> init_func_;
  base::lazy<ir::CompiledFn *> copy_assign_func_;
  base::lazy<ir::CompiledFn *> move_assign_func_;
};

Type const *Tup(std::vector<Type const *> entries);

}  // namespace type

#endif  // ICARUS_TYPE_TUPLE_H
