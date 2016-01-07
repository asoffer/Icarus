#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* free();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_neg(llvm::IRBuilder<>& bldr, size_t n);
}  // namespace data


extern llvm::Module* global_module;

// This method uninitializes stack space for each particular type.

llvm::Function* Array::uninitialize() {
  if (uninit_fn_ != nullptr) return uninit_fn_;

  uninit_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(get_void()->llvm(),
        { get_pointer(this)->llvm() }, false),
      llvm::Function::ExternalLinkage, "uninit." + to_string(), global_module);

  FnScope* fn_scope = Scope::build<FnScope>();

  fn_scope->set_parent_function(uninit_fn_);
  fn_scope->set_type(get_function(get_pointer(this), get_void()));

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto alloc = uninit_fn_->args().begin();
  auto basic_ptr_type = get_pointer(get_char())->llvm();

  auto data_ptr = bldr.CreateLoad(alloc);
  auto ptr_to_free = bldr.CreateGEP(
      bldr.CreateBitCast(data_ptr, basic_ptr_type),
      { data::const_neg(bldr, get_uint()->bytes()) }, "ptr_to_free");

  if (data_type()->uninitialize() != nullptr) {
    auto len_ptr = bldr.CreateBitCast(ptr_to_free,
        get_pointer(get_uint())->llvm(), "len_ptr");
    auto len_val = bldr.CreateLoad(len_ptr);

    auto loop_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "loop", uninit_fn_);

    bldr.CreateBr(loop_block);
    bldr.SetInsertPoint(loop_block);

    llvm::PHINode* phi = bldr.CreatePHI(get_uint()->llvm(), 2, "phi");
    phi->addIncoming(data::const_uint(0), fn_scope->entry_block());

    auto curr_ptr = bldr.CreateGEP(data_ptr, { phi });
    bldr.CreateCall(data_type()->uninitialize(), { curr_ptr });

    auto next_data = bldr.CreateAdd(phi, data::const_uint(1));

    bldr.CreateCondBr(bldr.CreateICmpULT(next_data, len_val),
        loop_block, fn_scope->exit_block());
    phi->addIncoming(next_data, loop_block);
  }

  bldr.CreateCall(cstdlib::free(), { ptr_to_free });

  fn_scope->exit();
  return uninit_fn_;
}

llvm::Function* Primitive::uninitialize() {
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
