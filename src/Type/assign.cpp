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

  auto save_block = builder.GetInsertBlock();

  assign_fn_ = llvm::Function::Create(
      *Func({Ptr(this), Ptr(this)}, Void), llvm::Function::ExternalLinkage,
      "assign." + Mangle(this), global_module);

  // Create uninitialization function
  auto block = make_block("entry", assign_fn_);
  builder.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto val  = iter;
  auto var = ++iter;
  val->setName("val");
  var->setName("var");

  call_uninit(var);
  // Allocate space and save the pointer
  auto new_len = builder.CreateLoad(
      builder.CreateGEP(val, {data::const_uint(0), data::const_uint(0)}),
      "new_len");
  auto data_ptr_ptr = builder.CreateGEP(
      var, {data::const_uint(0), data::const_uint(1)}, "data_ptr_ptr");
  auto load_ptr_ptr = builder.CreateGEP(
      val, {data::const_uint(0), data::const_uint(1)}, "load_ptr_ptr");
  auto malloc_call = builder.CreateBitCast(
      builder.CreateCall(
          cstdlib::malloc(),
          builder.CreateMul(new_len, data::const_uint(data_type.get->bytes()))),
      *Ptr(data_type), "malloc_call");

  builder.CreateStore(
      new_len, builder.CreateGEP(var, {data::const_uint(0), data::const_uint(0)}));

  builder.CreateStore(malloc_call, builder.CreateGEP(var, {data::const_uint(0),
                                                     data::const_uint(1)}));

  auto copy_to_ptr   = builder.CreateLoad(data_ptr_ptr);
  auto copy_from_ptr = builder.CreateLoad(load_ptr_ptr);
  auto end_ptr       = builder.CreateGEP(copy_to_ptr, new_len);

  auto loop_block = make_block("loop", assign_fn_);
  auto exit_block = make_block("exit", assign_fn_);

  auto prev_block = builder.GetInsertBlock();
  builder.CreateBr(loop_block);
  builder.SetInsertPoint(loop_block);
  auto from_phi = builder.CreatePHI(*Ptr(data_type), 2, "from_phi");
  auto to_phi = builder.CreatePHI(*Ptr(data_type), 2, "to_phi");
  from_phi->addIncoming(copy_from_ptr, prev_block);
  to_phi->addIncoming(copy_to_ptr, prev_block);

  auto copy_from_elem =
      data_type.get->is_big()
          ? static_cast<llvm::Value *>(from_phi)
          : static_cast<llvm::Value *>(builder.CreateLoad(from_phi));

  builder.CreateCall(data_type.get->assign(), {copy_from_elem, to_phi});

  auto next_from_ptr = builder.CreateGEP(from_phi, data::const_uint(1));
  auto next_to_ptr   = builder.CreateGEP(to_phi, data::const_uint(1));

  builder.CreateCondBr(builder.CreateICmpULT(next_to_ptr, end_ptr), loop_block,
                       exit_block);
  to_phi->addIncoming(next_to_ptr, builder.GetInsertBlock());
  from_phi->addIncoming(next_from_ptr, builder.GetInsertBlock());

  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  builder.SetInsertPoint(save_block);

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
    if (!the_field_type.get->is_big()) {
      field_val = bldr.CreateLoad(the_field_type, field_val);
    }
    auto field_var = bldr.CreateGEP(
        var, {data::const_uint(0), data::const_uint(iter.second)});
    bldr.CreateCall(the_field_type.get->assign(), {field_val, field_var});
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
