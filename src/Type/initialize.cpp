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
  bldr.CreateStore(data::const_uint(0),
                   bldr.CreateGEP(var, data::const_uint(0)));
  bldr.CreateStore(data::null_pointer(data_type),
                   bldr.CreateGEP(var, data::const_uint(1)));
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

// This function call initializes the array with the given lengths
// TODO document the difference between this and call_init
//llvm::Function* Array::initialize() {
//  if (init_fn_ != nullptr) return init_fn_;
//
//  std::vector<llvm::Type*> init_types(dimension + 1, *Uint);
//  init_types[0] = *Ptr(this);
//
//  init_fn_ = llvm::Function::Create(
//      llvm::FunctionType::get(*Void, init_types, false),
//      llvm::Function::ExternalLinkage, "init." + to_string(),
//      global_module);
//
//  FnScope* fn_scope = new FnScope(init_fn_);
//  fn_scope->set_type(Func(this, Void));
//
//  llvm::IRBuilder<>& bldr = fn_scope->builder();
//  fn_scope->enter();
//
//  // Name the function arguments to make the LLVM IR easier to read
//  std::vector<llvm::Value *> args;
//  size_t arg_num = 0;
//  auto init_args = init_fn_->args();
//  for (auto &arg : init_args) {
//    arg.setName("arg" + std::to_string(arg_num++));
//    args.push_back(&arg);
//  }
//  auto store_ptr = bldr.CreateLoad(bldr.CreateGEP(args[0], data::const_uint(1));
//  bldr.CreateStore(args[0], bldr.CreateGEP(var, data::const_uint(0)));
//  auto len_val = args.back();
//  args.pop_back();
//
//  auto bytes_needed =
//      bldr.CreateMul(len_val, data::const_uint(data_type->bytes()));
//
//  // TODO more generally, determine whether zeroing out the type
//  // is the correct behavior for initialization
//  auto use_calloc = data_type->is_primitive();
//
//  // (M/C)alloc call
//  auto alloc_call = bldr.CreateCall(
//      use_calloc ? cstdlib::calloc() : cstdlib::malloc(), {bytes_needed});
//
//  bldr.CreateStore(alloc_call, );
//
//
//  // Store the length at the head of the array
//  auto len_ptr = bldr.CreateBitCast(alloc_call, *Ptr(Uint), "len_ptr");
//  bldr.CreateStore(len_val, len_ptr);
//
//  // Pointer to the array data
//  auto raw_data_ptr = bldr.CreateGEP(alloc_call, {uint_size});
//
//  // Pointer to data cast
//  auto data_ptr = bldr.CreateBitCast(raw_data_ptr, *Ptr(data_type), "data_ptr");
//  bldr.CreateStore(data_ptr, store_ptr);
//
//  // Just calling calloc is okay for p
//  if (!use_calloc) {
//    auto end_ptr = bldr.CreateGEP(data_ptr, { len_val });
//
//    // Loop through the array and initialize each input
//    auto loop_block = make_block("loop", init_fn_);
//    bldr.CreateBr(loop_block);
//    bldr.SetInsertPoint(loop_block);
//
//    llvm::PHINode* phi = bldr.CreatePHI(*Ptr(data_type), 2, "phi");
//    phi->addIncoming(data_ptr, fn_scope->entry_block());
//
//    std::vector<llvm::Value*> next_init_args = { phi };
//    if (data_type->is_array()) {
//      auto iters = init_args.begin();
//      ++(++iters); // Start at the second length argument
//
//      while (iters != init_args.end()) {
//        next_init_args.push_back(iters);
//        ++iters;
//      }
//      auto data_array_type = static_cast<Array*>(data_type);
//      bldr.CreateCall(data_array_type->initialize(), next_init_args);
//
//    } else {
//      data_type->call_init(bldr, phi);
//    }
//
//    auto next_ptr = bldr.CreateGEP(phi, { data::const_uint(1) });
//
//    bldr.CreateCondBr(bldr.CreateICmpULT(next_ptr, end_ptr),
//        loop_block, fn_scope->exit_block());
//    phi->addIncoming(next_ptr, loop_block);
//  }
//
//  fn_scope->exit();
//  return init_fn_;
//}

llvm::Value *Array::initialize_literal(llvm::IRBuilder<> &bldr,
                                       size_t len) {

  auto bytes_to_alloc = data::const_uint(len * data_type->bytes());
  auto malloc_call = bldr.CreateBitCast(
      bldr.CreateCall(cstdlib::malloc(), {bytes_to_alloc}), *Ptr(data_type));

  // TODO allocate this in the right place
  auto alloc = allocate(bldr);
  bldr.CreateStore(
      data::const_uint(len),
      bldr.CreateGEP(alloc, {data::const_uint(0), data::const_uint(0)}));
  bldr.CreateStore(malloc_call, bldr.CreateGEP(alloc, {data::const_uint(0),
                                                       data::const_uint(1)}));
  return alloc;
}
