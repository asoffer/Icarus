#include "Type.h"
#include "Scope.h"

#include <iostream>

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module* global_module;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
namespace cstdlib {
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace data {
extern llvm::Value *const_neg(llvm::IRBuilder<> &bldr, size_t n);
extern llvm::Value *const_uint(size_t n);
} // namespace data

llvm::Function *get_llvm_assign(Type *type) {
  return llvm::Function::Create(*Func({type, Ptr(type)}, Void),
                                llvm::Function::ExternalLinkage,
                                "assign." + type->to_string(), global_module);
}

llvm::Function* Primitive::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  auto block = make_block("entry", assign_fn_);
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  bldr.CreateRetVoid();

  return assign_fn_;
}

llvm::Function* Function::assign() {
  assert(false && "Function assignment is illegal!");
}

llvm::Function* Array::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = llvm::Function::Create(
      *Func({Ptr(this), Ptr(this)}, Void), llvm::Function::ExternalLinkage,
      "assign." + Mangle(this), global_module);

  // Create uninitialization function
  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  auto block = make_block("entry", assign_fn_);
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val  = iter;
  auto var = ++iter;
  val->setName("val");
  var->setName("var");

  call_uninit(bldr, var);
  // Allocate space and save the pointer
  auto new_len = bldr.CreateLoad(
      bldr.CreateGEP(val, {data::const_uint(0), data::const_uint(0)}),
      "new_len");
  auto data_ptr_ptr = bldr.CreateGEP(
      var, {data::const_uint(0), data::const_uint(1)}, "data_ptr_ptr");
  auto load_ptr_ptr = bldr.CreateGEP(
      val, {data::const_uint(0), data::const_uint(1)}, "load_ptr_ptr");
  auto malloc_call = bldr.CreateBitCast(
      bldr.CreateCall(
          cstdlib::malloc(),
          bldr.CreateMul(new_len, data::const_uint(data_type->bytes()))),
      *Ptr(data_type), "malloc_call");

  bldr.CreateStore(
      new_len, bldr.CreateGEP(var, {data::const_uint(0), data::const_uint(0)}));

  bldr.CreateStore(malloc_call, bldr.CreateGEP(var, {data::const_uint(0),
                                                     data::const_uint(1)}));

  auto copy_to_ptr   = bldr.CreateLoad(data_ptr_ptr);
  auto copy_from_ptr = bldr.CreateLoad(load_ptr_ptr);
  auto end_ptr       = bldr.CreateGEP(copy_to_ptr, new_len);

  auto loop_block = make_block("loop", assign_fn_);
  auto land_block = make_block("land", assign_fn_);

  auto prev_block = bldr.GetInsertBlock();
  bldr.CreateBr(loop_block);
  bldr.SetInsertPoint(loop_block);
  auto from_phi = bldr.CreatePHI(*Ptr(data_type), 2, "from_phi");
  auto to_phi = bldr.CreatePHI(*Ptr(data_type), 2, "to_phi");
  from_phi->addIncoming(copy_from_ptr, prev_block);
  to_phi->addIncoming(copy_to_ptr, prev_block);

  auto copy_from_elem =
      data_type->is_big()
          ? static_cast<llvm::Value *>(from_phi)
          : static_cast<llvm::Value *>(bldr.CreateLoad(from_phi));

  bldr.CreateCall(data_type->assign(), {copy_from_elem, to_phi});

  auto next_from_ptr = bldr.CreateGEP(from_phi, data::const_uint(1));
  auto next_to_ptr   = bldr.CreateGEP(to_phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_to_ptr, end_ptr), loop_block,
                    land_block);
  to_phi->addIncoming(next_to_ptr, bldr.GetInsertBlock());
  from_phi->addIncoming(next_from_ptr, bldr.GetInsertBlock());

  auto exit_block = make_block("exit", assign_fn_);
  bldr.CreateBr(exit_block);
  bldr.SetInsertPoint(exit_block);
  bldr.CreateRetVoid();

  return assign_fn_;
}

llvm::Function *Pointer::assign() {
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

llvm::Function *Structure::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  auto block = make_block("entry", assign_fn_);
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val  = iter;
  auto var  = ++iter;

  // assign all fields
  for (const auto& iter : field_num_to_llvm_num) {
    auto the_field_type = field_type AT(iter.first);
    auto field_val = bldr.CreateGEP(
        val, {data::const_uint(0), data::const_uint(iter.second)});
    if (!the_field_type->is_big()) {
      field_val = bldr.CreateLoad(*the_field_type, field_val);
    }
    auto field_var = bldr.CreateGEP(
        var, {data::const_uint(0), data::const_uint(iter.second)});
    bldr.CreateCall(the_field_type->assign(), {field_val, field_var});
  }

  auto exit_block = make_block("exit", assign_fn_);
  bldr.CreateBr(exit_block);
  bldr.SetInsertPoint(exit_block);
  bldr.CreateRetVoid();

  return assign_fn_;
}

llvm::Function *Enumeration::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  assign_fn_ = get_llvm_assign(this);
  auto block = make_block("entry", assign_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val  = iter;
  auto var = ++iter;
  bldr.CreateStore(val, var);
  bldr.CreateRetVoid();

  return assign_fn_;
}

llvm::Function* DependentType::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;
  return nullptr;
}

// TODO these should probably be the default behavior in Type and overriden
// elsewhere
llvm::Function* TypeVariable::assign() {
  assert(false && "Cannot assign to a type variable");
}

llvm::Function* ForwardDeclaration::assign() {
  assert(false && "Cannot assign to a forward declaration");
}
