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
extern llvm::Value *const_char(char c);
extern llvm::Value *const_uint(size_t n);
extern llvm::Value *const_neg(llvm::IRBuilder<> &bldr, size_t n);
} // namespace data

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
extern llvm::Module *global_module;

void Primitive::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {}

void Array::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  if (uninit_fn_ == nullptr) {
    uninit_fn_ = llvm::Function::Create(
        llvm::FunctionType::get(*Void, {*Ptr(this)}, false),
        llvm::Function::ExternalLinkage, "uninit." + to_string(),
        global_module);

    FnScope *fn_scope = new FnScope(uninit_fn_);
    fn_scope->set_type(Func(Ptr(this), Void));

    llvm::IRBuilder<> &fnbldr = fn_scope->builder();
    fn_scope->enter();

    auto array = uninit_fn_->args().begin();
    uninit_fn_->args().begin()->setName("array");
    auto data_ptr = fnbldr.CreateLoad(
        fnbldr.CreateGEP(array, {data::const_uint(0), data::const_uint(1)}),
        "ptr_to_free");

    if (data_type->requires_uninit()) {
      auto len_ptr = fnbldr.CreateGEP(
          array, {data::const_uint(0), data::const_uint(0)}, "len_ptr");
      auto len_val = fnbldr.CreateLoad(len_ptr, "len_val");

      auto end_ptr  = fnbldr.CreateGEP(data_ptr, len_val, "end_ptr");

      auto loop_block = make_block("loop", uninit_fn_);
      fnbldr.CreateBr(loop_block);
      fnbldr.SetInsertPoint(loop_block);

      auto phi = fnbldr.CreatePHI(*Ptr(data_type), 2, "phi");
      phi->addIncoming(data_ptr, fn_scope->entry_block());

      data_type->call_uninit(fnbldr, {phi});
      auto next_ptr   = fnbldr.CreateGEP(phi, data::const_uint(1));
      auto land_block = make_block("land", uninit_fn_);
      fnbldr.CreateCondBr(fnbldr.CreateICmpULT(next_ptr, end_ptr), loop_block,
                          land_block);
      phi->addIncoming(next_ptr, loop_block);

      fnbldr.SetInsertPoint(land_block);
    }

    fnbldr.CreateCall(cstdlib::free(),
                      fnbldr.CreateBitCast(data_ptr, *RawPtr));
    fn_scope->exit();
  }

  bldr.CreateCall(uninit_fn_, {var});
}

void Tuple::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  // TODO
}

void Pointer::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {}
void Function::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {}
void Enumeration::call_uninit(llvm::IRBuilder<> &bldr, llvm::Value *var) {}

void Structure::call_uninit(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  if (!requires_uninit()) return;

  if (uninit_fn_ == nullptr) {
    uninit_fn_ = llvm::Function::Create(*Func(Ptr(this), Void),
                                        llvm::Function::ExternalLinkage,
                                        "uninit." + to_string(), global_module);

    auto block = make_block("entry", uninit_fn_);

    llvm::IRBuilder<> fnbldr(llvm::getGlobalContext());
    fnbldr.SetInsertPoint(block);

    for (const auto &kv : field_num_to_llvm_num) {
      auto the_field_type = field_type AT(kv.first);
      if (the_field_type->requires_uninit()) {
        auto arg = fnbldr.CreateGEP(
            uninit_fn_->args().begin(),
            {data::const_uint(0), data::const_uint(kv.second)});
        the_field_type->call_uninit(fnbldr, {arg});
      }
    }

    fnbldr.CreateRetVoid();
  }

  bldr.CreateCall(uninit_fn_, { var });
}

void DependentType::call_uninit(llvm::IRBuilder<>&, llvm::Value*) {
  assert(false && "Cannot uninitialize a dependent type");
}

void TypeVariable::call_uninit(llvm::IRBuilder<>&, llvm::Value*) {
  assert(false && "Cannot uninitialize a type variable");
}

void ForwardDeclaration::call_uninit(llvm::IRBuilder<> &, llvm::Value *) {
  assert(false && "Cannot uninitialize a forward declaration");
}

