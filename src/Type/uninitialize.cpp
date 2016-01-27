#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* free();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_neg(llvm::IRBuilder<>& bldr, size_t n);
}  // namespace data

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);
extern llvm::Module* global_module;

// This method uninitializes stack space for each particular type.

llvm::Function* Array::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;

  uninit_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(*Void, { *Ptr(this) }, false),
      llvm::Function::ExternalLinkage, "uninit." + to_string(), global_module);

  FnScope* fn_scope = Scope::build_fn<FnScope>();

  fn_scope->set_parent_function(uninit_fn_);
  fn_scope->set_type(get_function(Ptr(this), Void));

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto alloc = uninit_fn_->args().begin();
  auto data_ptr = bldr.CreateLoad(alloc);
  auto ptr_to_free = bldr.CreateGEP(bldr.CreateBitCast(data_ptr, *RawPtr),
      { data::const_neg(bldr, Uint->bytes()) }, "ptr_to_free");

  if (data_type()->uninitialize() != nullptr) {
    auto len_ptr = bldr.CreateBitCast(ptr_to_free, *Ptr(Uint), "len_ptr");
    auto len_val = bldr.CreateLoad(len_ptr);
    auto end_ptr = bldr.CreateGEP(data_ptr, { len_val });

    auto loop_block = make_block("loop", uninit_fn_);

    bldr.CreateBr(loop_block);
    bldr.SetInsertPoint(loop_block);

    llvm::PHINode* phi = bldr.CreatePHI(*Ptr(data_type()), 2, "phi");
    phi->addIncoming(data_ptr, fn_scope->entry_block());

    bldr.CreateCall(data_type()->uninitialize(), { phi });

    auto next_ptr = bldr.CreateGEP(phi, { data::const_uint(1) });

    bldr.CreateCondBr(bldr.CreateICmpULT(next_ptr, end_ptr),
        loop_block, fn_scope->exit_block());
    phi->addIncoming(next_ptr, loop_block);
  }

  bldr.CreateCall(cstdlib::free(), { ptr_to_free });

  fn_scope->exit();
  return uninit_fn_;
}

llvm::Function* TypeSystem::Primitive::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}

llvm::Function* Function::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}


llvm::Function* Pointer::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}

llvm::Function* Tuple::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}

llvm::Function* UserDefined::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}

llvm::Function* Enum::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;
  // TODO
  return nullptr;
}


