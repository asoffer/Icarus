#include "Type.h"
#include "Scope.h"

#include <iostream>

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(llvm::IRBuilder<>& bldr, int n, bool is_signed = false);
  extern llvm::Value* const_uint(size_t n);

  extern llvm::Value* global_string(const std::string& s);
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
  fn_scope->set_type(get_function(get_tuple({ this, get_pointer(this) }), get_void()));

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
  fn_scope->set_type(get_function(get_tuple({ this, get_pointer(this) }), get_void()));

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;


  bldr.CreateCall(uninitialize(), { var });

  auto raw_ptr_type = get_pointer(get_char())->llvm();
  auto raw_len_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, raw_ptr_type),
      { data::const_int(bldr, -4, true) }, "ptr_to_len");
  auto len_val = bldr.CreateLoad(
    bldr.CreateBitCast(raw_len_ptr, get_pointer(get_uint())->llvm()));

  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Type::get_uint()->bytes());
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len_val, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Store the right length in the start of the array.
  bldr.CreateStore(len_val,
      bldr.CreateBitCast(malloc_call, get_pointer(get_uint())->llvm()));

  auto raw_data_ptr = bldr.CreateGEP(malloc_call, { int_size });
  auto data_ptr = bldr.CreateBitCast(raw_data_ptr, get_pointer(data_type())->llvm());
  auto copy_ptr = bldr.CreateGEP(val, { data::const_uint(0) });

  bldr.CreateStore(data_ptr, var);

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

  bldr.CreateCondBr(bldr.CreateICmpULT(next_iter, len_val), loop_block, done_block);
  phi->addIncoming(next_iter, bldr.GetInsertBlock());

  bldr.SetInsertPoint(done_block);
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

  assign_fn_ = get_llvm_assign(this);
    
  FnScope* fn_scope = Scope::build<FnScope>();
  fn_scope->set_parent_function(assign_fn_);
  fn_scope->set_type(get_function(get_tuple({ this, get_pointer(this) }), get_void()));

//   llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
//   auto iter = assign_fn_->args().begin();
//   auto val = iter;
//   auto var = ++iter;

  // assign all fields
  // TODO It's problematic to pass an object of user-defined type by value
//  auto fields_size = fields_.size();
//  for (size_t field_num = 0; field_num < fields_size; ++field_num) {
//    auto field_type = fields_[field_num].second;
//
//    auto field_val = bldr.CreateGEP(field_type->llvm(), val,
//        { data::const_uint(field_num) });
//    field_val->dump();
//    auto field_var = bldr.CreateGEP(field_type->llvm(), var,
//        { data::const_uint(0), data::const_uint(field_num) });
//  
//    field_var->dump();
//    bldr.CreateCall(field_type->assign(), { field_val, field_var });
//  }
//
  fn_scope->exit();

  return assign_fn_;
}
