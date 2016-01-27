#include "Type.h"
#include "Scope.h"

#include <iostream>

extern llvm::Module* global_module;
extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);
namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_neg(llvm::IRBuilder<>& bldr, size_t n);
  extern llvm::Value* const_uint(size_t n);

  extern llvm::Value* global_string(const std::string& s);
}  // namespace data


llvm::Function* get_llvm_assign(Type* type) {
  return llvm::Function::Create(
      Type::get_function(Type::get_tuple({ type, Ptr(type) }), Void)->llvm(),
      llvm::Function::ExternalLinkage, "assign." + type->to_string(),
      global_module);
}

llvm::Function* TypeSystem::Primitive::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
  auto block = make_block("entry", assign_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  bldr.CreateRetVoid();

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

  FnScope* fn_scope = Scope::build_fn<FnScope>();
  fn_scope->set_parent_function(assign_fn_);
  fn_scope->set_type(get_function(get_tuple({ this, Ptr(this) }), Void));

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;


  bldr.CreateCall(uninitialize(), { var });

  auto raw_len_ptr = bldr.CreateGEP(bldr.CreateBitCast(val, *RawPtr),
      { data::const_neg(bldr, Uint->bytes()) }, "ptr_to_len");
  auto len_val = bldr.CreateLoad(
    bldr.CreateBitCast(raw_len_ptr, *Ptr(Uint)));

  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Uint->bytes());
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len_val, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Store the right length in the start of the array.
  bldr.CreateStore(len_val,
      bldr.CreateBitCast(malloc_call, *Ptr(Uint)));

  auto raw_data_ptr = bldr.CreateGEP(malloc_call, { int_size });
  auto copy_to_ptr = bldr.CreateBitCast(raw_data_ptr, *Ptr(data_type()));
  auto copy_from_ptr = bldr.CreateGEP(val, { data::const_uint(0) });
  auto end_ptr = bldr.CreateGEP(copy_to_ptr, { len_val });

  bldr.CreateStore(copy_to_ptr, var);

  auto loop_block = make_block("loop", assign_fn_);
  auto done_block = make_block("loop_done", assign_fn_);

  bldr.CreateBr(loop_block);

  auto prev_block = bldr.GetInsertBlock();

  bldr.SetInsertPoint(loop_block);
  llvm::PHINode* from_phi = bldr.CreatePHI(*Ptr(data_type()), 2, "phi");
  from_phi->addIncoming(copy_from_ptr, prev_block);

  llvm::PHINode* to_phi = bldr.CreatePHI(*Ptr(data_type()), 2, "phi");
  to_phi->addIncoming(copy_to_ptr, prev_block);

  auto copy_from_elem = bldr.CreateLoad(bldr.CreateGEP(from_phi, { data::const_uint(0) }));
  bldr.CreateCall(data_type()->assign(), { copy_from_elem, to_phi });

  auto next_from_ptr = bldr.CreateGEP(from_phi, data::const_uint(1));
  auto next_to_ptr = bldr.CreateGEP(to_phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_to_ptr, end_ptr), loop_block, done_block);
  to_phi->addIncoming(next_to_ptr, bldr.GetInsertBlock());
  from_phi->addIncoming(next_from_ptr, bldr.GetInsertBlock());

  bldr.SetInsertPoint(done_block);
  fn_scope->exit();
  return assign_fn_;

  return nullptr;
}

llvm::Function* Pointer::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
  auto block = make_block("entry", assign_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  bldr.CreateRetVoid();

  return assign_fn_;
}

llvm::Function* Tuple::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;
  return nullptr;
}

llvm::Function* UserDefined::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
    
  FnScope* fn_scope = Scope::build_fn<FnScope>();
  fn_scope->set_parent_function(assign_fn_);
  fn_scope->set_type(get_function(get_tuple({ this, Ptr(this) }), Void));

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
   auto iter = assign_fn_->args().begin();
   auto val = iter;
   auto var = ++iter;

  // assign all fields
   auto fields_size = fields_.size();
   for (size_t field_num = 0; field_num < fields_size; ++field_num) {
     auto field_type = fields_[field_num].second;

     auto field_val = bldr.CreateGEP(llvm(), val,
         { data::const_uint(0), data::const_uint(field_num) });
     if (!field_type->is_user_defined()) {
       field_val = bldr.CreateLoad(*field_type, field_val);
     }
     auto field_var = bldr.CreateGEP(llvm(), var,
         { data::const_uint(0), data::const_uint(field_num) });

     bldr.CreateCall(field_type->assign(), { field_val, field_var });
   }

  fn_scope->exit();

  return assign_fn_;
}

llvm::Function* Enum::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
  auto block = make_block("entry", assign_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  bldr.CreateRetVoid();

  return assign_fn_;
}


void TypeSystem::Primitive::set_assign(llvm::Function* fn) {}
void Pointer::set_assign(llvm::Function* fn) {}
void Tuple::set_assign(llvm::Function* fn) {}
void Function::set_assign(llvm::Function* fn) {}
void Array::set_assign(llvm::Function* fn) {}
void Enum::set_assign(llvm::Function* fn) {}
void UserDefined::set_assign(llvm::Function* fn) {
  assign_fn_ = fn;
  assign_fn_->setName("assign." + to_string());
}
