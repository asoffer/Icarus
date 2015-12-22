#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* free();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(int n, bool is_signed = false);
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
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto alloc = uninit_fn_->args().begin();
  auto basic_ptr_type = get_pointer(get_char())->llvm();

  // TODO uninitialize internals

  auto ptr_to_free = bldr.CreateGEP(
      bldr.CreateBitCast(bldr.CreateLoad(alloc), basic_ptr_type),
      { data::const_int(-4, true) }, "ptr_to_free");

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
