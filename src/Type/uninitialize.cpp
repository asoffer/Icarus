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
} // namespace data

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
extern llvm::Module *global_module;

void Primitive::call_uninit(llvm::Value *var) {}

void Array::call_uninit(llvm::Value *var) {
  if (uninit_fn_ == nullptr) {
    auto prev_block = builder.GetInsertBlock();

    uninit_fn_ = llvm::Function::Create(
        llvm::FunctionType::get(Void, {TypePtr(Ptr(this))}, false),
        llvm::Function::ExternalLinkage, "uninit." + Mangle(this),
        global_module);

    auto entry_block = make_block("entry", uninit_fn_);
    builder.SetInsertPoint(entry_block);

    auto array = uninit_fn_->args().begin();
    array->setName("array");

    auto data_ptr = builder.CreateLoad(
        builder.CreateGEP(array, {data::const_uint(0), data::const_uint(1)}),
        "ptr_to_free");

    if (data_type.get->requires_uninit()) {
      auto len_ptr = builder.CreateGEP(
          array, {data::const_uint(0), data::const_uint(0)}, "len_ptr");
      auto len_val = builder.CreateLoad(len_ptr, "len_val");

      auto end_ptr  = builder.CreateGEP(data_ptr, len_val, "end_ptr");

      auto loop_block = make_block("loop", uninit_fn_);
      builder.CreateBr(loop_block);
      builder.SetInsertPoint(loop_block);

      auto phi = builder.CreatePHI(*Ptr(data_type), 2, "phi");
      phi->addIncoming(data_ptr, entry_block);

      data_type.get->call_uninit({phi});
      auto next_ptr   = builder.CreateGEP(phi, data::const_uint(1));
      auto land_block = make_block("land", uninit_fn_);
      builder.CreateCondBr(builder.CreateICmpULT(next_ptr, end_ptr), loop_block,
                           land_block);
      phi->addIncoming(next_ptr, loop_block);

      builder.SetInsertPoint(land_block);
    }

    builder.CreateCall(cstdlib::free(),
                       builder.CreateBitCast(data_ptr, RawPtr));
    builder.CreateRetVoid();
    builder.SetInsertPoint(prev_block);
  }

  builder.CreateCall(uninit_fn_, {var});
}

void Tuple::call_uninit(llvm::Value *var) {
  // TODO
}

void Pointer::call_uninit(llvm::Value *var) {}
void Function::call_uninit(llvm::Value *var) {}
void Enumeration::call_uninit(llvm::Value *var) {}

void Structure::call_uninit(llvm::Value* var) {
  if (!requires_uninit()) return;

  if (uninit_fn_ == nullptr) {
    auto prev_block = builder.GetInsertBlock();
    uninit_fn_ = llvm::Function::Create(
        *Func(Ptr(this), Void), llvm::Function::ExternalLinkage,
        "uninit." + Mangle(this), global_module);

    auto block = make_block("entry", uninit_fn_);
    builder.SetInsertPoint(block);

    for (const auto &kv : field_num_to_llvm_num) {
      auto the_field_type = field_type AT(kv.first);
      if (the_field_type.get->requires_uninit()) {
        auto arg = builder.CreateGEP(
            uninit_fn_->args().begin(),
            {data::const_uint(0), data::const_uint(kv.second)});
        the_field_type.get->call_uninit({arg});
      }
    }

    builder.CreateRetVoid();

    builder.SetInsertPoint(prev_block);
  }

  builder.CreateCall(uninit_fn_, { var });
}

void DependentType::call_uninit(llvm::Value*) {
  assert(false && "Cannot uninitialize a dependent type");
}

void TypeVariable::call_uninit(llvm::Value*) {
  assert(false && "Cannot uninitialize a type variable");
}

void ForwardDeclaration::call_uninit(llvm::Value *) {
  assert(false && "Cannot uninitialize a forward declaration");
}

