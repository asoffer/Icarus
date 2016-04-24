#include "Type.h"
#include "Scope.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module *global_module;

namespace cstdlib {
extern llvm::Constant *free();
} // namespace cstdlib

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
extern llvm::Module *global_module;

llvm::Function *Array::destroy() {
  if (destroy_fn_ != nullptr) { return destroy_fn_; }

  auto save_block = builder.GetInsertBlock();

  destroy_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(Void, {TypePtr(Ptr(this))}, false),
      llvm::Function::ExternalLinkage, "destr." + Mangle(this), global_module);

  auto entry_block = make_block("entry", destroy_fn_);
  builder.SetInsertPoint(entry_block);
  auto array = destroy_fn_->args().begin();
  array->setName("array");

  auto data_ptr = builder.CreateLoad(
      builder.CreateGEP(array, {data::const_uint(0), data::const_uint(1)}),
      "ptr_to_free");

  auto len_ptr = builder.CreateGEP(
      array, {data::const_uint(0), data::const_uint(0)}, "len_ptr");
  auto len_val = builder.CreateLoad(len_ptr, "len_val");
  auto end_ptr = builder.CreateGEP(data_ptr, len_val, "end_ptr");

  auto cond_block = make_block("cond", destroy_fn_);
  auto loop_block = make_block("loop", destroy_fn_);
  auto land_block = make_block("land", destroy_fn_);

  builder.CreateBr(cond_block);
  builder.SetInsertPoint(cond_block);

  auto phi = builder.CreatePHI(*Ptr(data_type), 2, "phi");
  phi->addIncoming(data_ptr, entry_block);

  builder.CreateCondBr(builder.CreateICmpULT(phi, end_ptr), loop_block,
                       land_block);

  builder.SetInsertPoint(loop_block);
  data_type.get->CallDestroy(nullptr, phi);
  auto next_ptr = builder.CreateGEP(phi, data::const_uint(1));

  builder.CreateBr(cond_block);
  phi->addIncoming(next_ptr, loop_block);

  builder.SetInsertPoint(land_block);
  builder.CreateCall(cstdlib::free(), builder.CreateBitCast(data_ptr, RawPtr));

  builder.CreateRetVoid();
  builder.SetInsertPoint(save_block);
  return destroy_fn_;
}

llvm::Function *Structure::destroy() {
  if (destroy_fn_) { return destroy_fn_; }

  auto save_block = builder.GetInsertBlock();
  destroy_fn_     = llvm::Function::Create(*Func(Ptr(this), Void),
                                       llvm::Function::ExternalLinkage,
                                       "destr." + Mangle(this), global_module);

  auto block = make_block("entry", destroy_fn_);
  builder.SetInsertPoint(block);

  for (const auto &kv : field_num_to_llvm_num) {
    auto the_field_type = field_type AT(kv.first);
    auto val_to_destr   = destroy_fn_->args().begin();

    if (the_field_type.get->requires_uninit()) {
      auto arg = builder.CreateGEP(
          val_to_destr, {data::const_uint(0), data::const_uint(kv.second)});
      the_field_type.get->CallDestroy(nullptr, arg);
    }
  }

  builder.CreateRetVoid();
  builder.SetInsertPoint(save_block);
  return destroy_fn_;
}
