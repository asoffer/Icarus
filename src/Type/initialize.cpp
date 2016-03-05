#include "Type.h"
#include "Scope.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

namespace cstdlib {
extern llvm::Constant *calloc();
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace data {
extern llvm::Value *null_pointer(Type *t);
extern llvm::Value *const_int(int n);
extern llvm::Value *const_uint(size_t n);
extern llvm::Value *const_char(char c);
extern llvm::Value *const_real(double d);
extern llvm::Value *const_true();
extern llvm::Value *const_false();
} // namespace data

extern llvm::Module *global_module;

void Primitive::call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  switch (type_) {
  case TypeEnum::Error: assert(false && "Constructor called for type Error");
  case TypeEnum::Unknown:
    assert(false && "Constructor called for unknown type");
  case TypeEnum::Type: assert(false && "Constructor called for type");
  case TypeEnum::Void: assert(false && "Constructor called for void type");
  case TypeEnum::NullPtr: assert(false && "Constructor called for raw nullptr");
  case TypeEnum::Bool: bldr.CreateStore(data::const_false(), var); return;
  case TypeEnum::Char: bldr.CreateStore(data::const_char('\0'), var); return;
  case TypeEnum::Int: bldr.CreateStore(data::const_int(0), var); return;
  case TypeEnum::Real: bldr.CreateStore(data::const_real(0), var); return;
  case TypeEnum::Uint: bldr.CreateStore(data::const_uint(0), var); return;
  }
}

// TODO Change this is array size is known at compile time
void Array::call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  bldr.CreateStore(
      data::const_uint(0),
      bldr.CreateGEP(var, {data::const_uint(0), data::const_uint(0)}));
  bldr.CreateStore(
      data::null_pointer(data_type),
      bldr.CreateGEP(var, {data::const_uint(0), data::const_uint(1)}));
}

void Tuple::call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  // TODO
}

void Pointer::call_init(llvm::IRBuilder<> &bldr, llvm::Value *var) {
  bldr.CreateStore(data::null_pointer(pointee), var);
}

void Function::call_init(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  assert(false && "Cannot initialize a function. They are constant");
}

void Enumeration::call_init(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  bldr.CreateStore(data::const_uint(0), { var });
}

void DependentType::call_init(llvm::IRBuilder<>&, llvm::Value*) {
  assert(false && "Cannot initialize a dependent type");
}

void TypeVariable::call_init(llvm::IRBuilder<>&, llvm::Value*) {
  assert(false && "Cannot initialize a type variable");
}

void ForwardDeclaration::call_init(llvm::IRBuilder<> &, llvm::Value *) {
  assert(false && "Cannot initialize a forward declaration");
}

void Structure::call_init(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  if (init_fn_ == nullptr) {
    auto x = Func(Ptr(this), Void);
    init_fn_ = llvm::Function::Create(*x,
        llvm::Function::ExternalLinkage, "init." + to_string(), global_module);

    FnScope* fn_scope = new FnScope(init_fn_);
    fn_scope->set_type(Func(Ptr(this), Void));

    llvm::IRBuilder<>& bldr = fn_scope->builder();
    fn_scope->enter();

    // initialize all fields
    for (const auto& kv : field_num_to_llvm_num) {
      auto the_field_type = field_type AT(kv.first);
      auto arg = bldr.CreateGEP(
          init_fn_->args().begin(),
          {data::const_uint(0), data::const_uint(kv.second)});
      auto init_expr = init_values AT(kv.first);
      if (init_expr) {
        auto init_val = init_expr->generate_code(fn_scope);
        bldr.CreateCall(the_field_type->assign(), {init_val, arg});
      } else {
        the_field_type->call_init(bldr, {arg});
      }
    }

    fn_scope->exit();
  }

  bldr.CreateCall(init_fn_, { var });
}

// TODO no need to return anything here.
llvm::Value *Array::initialize_literal(llvm::IRBuilder<> &bldr,
                                       llvm::Value *alloc, size_t len) {
  return initialize_literal(bldr, alloc, data::const_uint(len));
}

llvm::Value *Array::initialize_literal(llvm::IRBuilder<> &bldr,
                                       llvm::Value *alloc, llvm::Value *len) {
  auto use_calloc         = data_type->is_primitive();
  llvm::Value *alloc_call = nullptr;

  if (use_calloc) {
    alloc_call = bldr.CreateBitCast(
        bldr.CreateCall(cstdlib::calloc(),
                        {len, data::const_uint(data_type->bytes())}),
        *Ptr(data_type));
  } else {
    auto bytes_to_alloc =
        bldr.CreateMul(len, data::const_uint(data_type->bytes()));

    alloc_call = bldr.CreateBitCast(
        bldr.CreateCall(cstdlib::malloc(), {bytes_to_alloc}), *Ptr(data_type));
  }

  // TODO allocate this in the right place
  bldr.CreateStore(
      len, bldr.CreateGEP(alloc, {data::const_uint(0), data::const_uint(0)}));

  bldr.CreateStore(alloc_call, bldr.CreateGEP(alloc, {data::const_uint(0),
                                                       data::const_uint(1)}));
  return alloc;
}
