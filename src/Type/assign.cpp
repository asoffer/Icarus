#include "Type.h"
#include "Scope.h"

#include <iostream>

extern llvm::Module* global_module;

namespace data {
  extern llvm::Value* const_int(int n, bool is_signed = false);
  extern llvm::Value* const_uint(size_t n);
}  // namespace data


llvm::Function* get_llvm_assign(Type* type) {
  return llvm::Function::Create(
      llvm::FunctionType::get(Type::get_void()->llvm(),
        { type->llvm(), Type::get_pointer(type)->llvm() }, false),
      llvm::Function::ExternalLinkage, "assign." + type->to_string(),
      global_module);
}

llvm::Function* Primitive::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
    
  FnScope* fn_scope = Scope::build<FnScope>();
  fn_scope->set_parent_function(assign_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  fn_scope->exit();

  return assign_fn_;
}

llvm::Function* Function::assign() {
  // Functions (for now) are const. This should not be allowed.
#ifdef DEBUG
  std::cerr << "FATAL: Function assignment is illegal!" << std::endl;
#endif

  return nullptr;
}

llvm::Function* Array::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);

  // Create unitilization function

  FnScope* fn_scope = Scope::build<FnScope>();
  fn_scope->set_parent_function(assign_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;

  bldr.CreateCall(uninitialize(), { var });

  auto raw_ptr_type = get_pointer(get_char())->llvm();
  auto raw_len_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, raw_ptr_type),
      { data::const_int(-4, true) }, "ptr_to_len");
  auto len_ptr = bldr.CreateBitCast(raw_len_ptr,
      get_pointer(get_int())->llvm());

  // TODO is this const_int(0) superfluous?
  auto len = bldr.CreateLoad(bldr.CreateGEP(len_ptr, { data::const_uint(0) }));


  auto data_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, data_type()->llvm()), { data::const_uint(0) });
  auto copy_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(var, data_type()->llvm()), { data::const_uint(0) });

  auto loop_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "loop", assign_fn_);
  auto done_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "loop_done", assign_fn_);

  bldr.CreateBr(loop_block);

  auto prev_block = bldr.GetInsertBlock();

  bldr.SetInsertPoint(loop_block);
  llvm::PHINode* phi = bldr.CreatePHI(Type::get_uint()->llvm(), 2, "loop_phi");
  phi->addIncoming(data::const_uint(0), prev_block);

  auto data_ptr_item = bldr.CreateGEP(data_ptr, { phi });
  auto copy_elem = bldr.CreateLoad(
    bldr.CreateGEP(copy_ptr, { phi }));

  // TODO If the next thing is an array, I want to just point to it.
  bldr.CreateCall(data_type()->assign(), { copy_elem, data_ptr_item });
  auto next_iter = bldr.CreateAdd(phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_iter, len), loop_block, done_block);
  phi->addIncoming(next_iter, bldr.GetInsertBlock());





  fn_scope->exit();
  return assign_fn_;

  return nullptr;
}

llvm::Function* Pointer::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;
  return nullptr;
}

llvm::Function* Tuple::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;
  return nullptr;
}

llvm::Function* UserDefined::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;
  return nullptr;
}
