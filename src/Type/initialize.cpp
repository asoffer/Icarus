#include "Type.h"
#include "Scope.h"

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

namespace cstdlib {
extern llvm::Constant *calloc();
extern llvm::Constant *malloc();
extern llvm::Constant *printf();
} // namespace cstdlib

namespace data {
extern llvm::Value *global_string(const std::string &s);
extern llvm::Value *null_pointer(TypePtr t);
extern llvm::Value *const_int(int n);
extern llvm::Value *const_uint(size_t n);
extern llvm::Value *const_char(char c);
extern llvm::Value *const_real(double d);
extern llvm::Value *const_true();
extern llvm::Value *const_false();
} // namespace data

extern llvm::Module *global_module;

void Primitive::call_init(llvm::Value *var) {
  switch (type_) {
  case TypeEnum::Error: assert(false && "Constructor called for type Error");
  case TypeEnum::Unknown:
    assert(false && "Constructor called for unknown type");
  case TypeEnum::Type: assert(false && "Constructor called for type");
  case TypeEnum::Void: assert(false && "Constructor called for void type");
  case TypeEnum::NullPtr: assert(false && "Constructor called for raw nullptr");
  case TypeEnum::Bool: builder.CreateStore(data::const_false(), var); return;
  case TypeEnum::Char: builder.CreateStore(data::const_char('\0'), var); return;
  case TypeEnum::Int: builder.CreateStore(data::const_int(0), var); return;
  case TypeEnum::Real: builder.CreateStore(data::const_real(0), var); return;
  case TypeEnum::Uint: builder.CreateStore(data::const_uint(0), var); return;
  }
}

// TODO Change this is array size is known at compile time
void Array::call_init(llvm::Value *var) {
  if (init_fn_ == nullptr) {
    auto save_block = builder.GetInsertBlock();

    init_fn_ = llvm::Function::Create(*Func(Ptr(this), Void),
                                      llvm::Function::ExternalLinkage,
                                      "init." + Mangle(this), global_module);

    auto block = make_block("entry", init_fn_);
    builder.SetInsertPoint(block);
    auto arg = init_fn_->args().begin();

    builder.CreateStore(
        data::const_uint(0),
        builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(0)}));
    auto ptr = builder.CreateBitCast(
        builder.CreateCall(cstdlib::malloc(), {data::const_uint(0)}),
        *Ptr(data_type));

    builder.CreateCall(cstdlib::printf(),
                       {data::global_string("malloced 0x%x in init.%s\n"),
                        ptr, data::global_string(to_string())});

    builder.CreateStore(ptr, builder.CreateGEP(arg, {data::const_uint(0),
                                                     data::const_uint(1)}));

    builder.CreateRetVoid();
    builder.SetInsertPoint(save_block);
  }
  builder.CreateCall(init_fn_, {var});
}

void Tuple::call_init(llvm::Value *var) {
  // TODO
}

void Pointer::call_init(llvm::Value *var) {
  builder.CreateStore(data::null_pointer(pointee), var);
}

void Function::call_init(llvm::Value *var) {
  assert(false && "Cannot initialize a function. They are constant");
}

void Enumeration::call_init(llvm::Value *var) {
  builder.CreateStore(data::const_uint(0), {var});
}

void DependentType::call_init(llvm::Value *) {
  assert(false && "Cannot initialize a dependent type");
}

void TypeVariable::call_init(llvm::Value *) {
  assert(false && "Cannot initialize a type variable");
}

void ParametricStructure::call_init(llvm::Value *) {
  assert(false && "Cannot initialize a parametric struct");
}

void QuantumType::call_init(llvm::Value *) {
  assert(false && "Cannot initialize a quantum type");
}

void Structure::call_init(llvm::Value *var) {
  if (init_fn_ == nullptr) {
    auto save_block = builder.GetInsertBlock();

    init_fn_ = llvm::Function::Create(*Func(Ptr(this), Void),
                                      llvm::Function::ExternalLinkage,
                                      "init." + Mangle(this), global_module);

    auto block = make_block("entry", init_fn_);
    builder.SetInsertPoint(block);

    // initialize all fields
    for (const auto &kv : field_num_to_llvm_num) {
      auto the_field_type = field_type AT(kv.first);
      auto arg =
          builder.CreateGEP(init_fn_->args().begin(),
                            {data::const_uint(0), data::const_uint(kv.second)});
      auto init_expr = init_values AT(kv.first);
      if (init_expr) {
        // Scope::Stack.push(fn_scope);
        auto init_val = init_expr->generate_code();
        // Scope::Stack.pop();
        builder.CreateCall(the_field_type.get->assign(), {init_val, arg});
      } else {
        the_field_type.get->call_init({arg});
      }
    }

    builder.CreateRetVoid();
    builder.SetInsertPoint(save_block);
  }

  builder.CreateCall(init_fn_, {var});
}

// TODO no need to return anything here.
llvm::Value *Array::initialize_literal(llvm::Value *alloc, size_t len) {
  return initialize_literal(alloc, data::const_uint(len));
}

// TODO rename? This isn't really about init-ing literals
llvm::Value *Array::initialize_literal(llvm::Value *alloc, llvm::Value *len) {
  auto use_calloc         = data_type.is_primitive();
  llvm::Value *alloc_call = nullptr;

  if (use_calloc) {
    alloc_call = builder.CreateBitCast(
        builder.CreateCall(cstdlib::calloc(),
                           {len, data::const_uint(data_type.get->bytes())}),
        *Ptr(data_type));
    builder.CreateCall(cstdlib::printf(),
                       {data::global_string("calloced 0x%x in initlit.%s\n"),
                        alloc_call, data::global_string(to_string())});

  } else {
    auto bytes_to_alloc =
        builder.CreateMul(len, data::const_uint(data_type.get->bytes()));

    alloc_call = builder.CreateBitCast(
        builder.CreateCall(cstdlib::malloc(), {bytes_to_alloc}),
        *Ptr(data_type));

    builder.CreateCall(cstdlib::printf(),
                       {data::global_string("malloced 0x%x in initlit.%s\n"),
                        alloc_call, data::global_string(to_string())});
  }

  // TODO allocate this in the right place
  builder.CreateStore(len, builder.CreateGEP(alloc, {data::const_uint(0),
                                                     data::const_uint(0)}));

  builder.CreateStore(
      alloc_call,
      builder.CreateGEP(alloc, {data::const_uint(0), data::const_uint(1)}));
  return alloc;
}
