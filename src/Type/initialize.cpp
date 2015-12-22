#include "Type.h"
#include "Scope.h"

namespace cstdlib {
  extern llvm::Constant* malloc();
}  // namespace cstdlib


namespace data {
  extern llvm::Value* const_int(int n, bool is_signed = false);
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* const_true();
  extern llvm::Value* const_false();
}  // namespace data

extern llvm::Module* global_module;

llvm::Function* get_llvm_init(Type* type) {
  return llvm::Function::Create(
      llvm::FunctionType::get(Type::get_void()->llvm(),
        { Type::get_pointer(type)->llvm() }, false),
      llvm::Function::ExternalLinkage, "init." + type->to_string(),
      global_module);
}


llvm::Function* Primitive::initialize() {
  if (init_fn_ != nullptr) return init_fn_;

  init_fn_ = get_llvm_init(this);
    
  FnScope* fn_scope = Scope::build<FnScope>();
  fn_scope->set_parent_function(init_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  auto var = init_fn_->args().begin();

  llvm::Value* init_val;
  switch (prim_type_) {
    case t_bool:
      init_val = data::const_false();
      break;
    case t_char:
      init_val = data::const_char('\0');
      break;
    case t_int:
      init_val = data::const_int(0, true);  // signed
      break;
    case t_uint:
      init_val = data::const_int(0); // unsigned
      break;
    case t_real:
      init_val = data::const_real(0);
      break;
    default: return nullptr;
  }

  bldr.CreateStore(init_val, var);
  fn_scope->exit();

  return init_fn_;
}

llvm::Function* Function::initialize() {
  // Functions (for now) are const. This should not be allowed.
#ifdef DEBUG
  std::cerr << "FATAL: Function initialization is illegal!" << std::endl;
#endif

  return nullptr;
}


// For arrays whose length is determned at runtime
llvm::Function* Array::initialize() {
  if (init_fn_ != nullptr) return init_fn_;

  std::vector<llvm::Type*> init_types(dim_ + 1, get_uint()->llvm());
  init_types[0] = get_pointer(this)->llvm();

  // We can't call get_llvm_init, because arrays also need lengths. A slight
  // modification does what we want.
  init_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(Type::get_void()->llvm(), init_types, false),
      llvm::Function::ExternalLinkage, "init." + to_string(),
      global_module);

  FnScope* fn_scope = Scope::build<FnScope>();
  fn_scope->set_parent_function(init_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();

  // Name the function arguments to make the LLVM IR easier to read
  std::vector<llvm::Value*> args;
  size_t arg_num = 0;
  for (auto& arg : init_fn_->args()) {
    arg.setName("arg" + std::to_string(arg_num));
    args.push_back(&arg);
  }
  auto store_ptr = args[0];
  auto len_val = args[1];

  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Type::get_int()->bytes());
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len_val, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto len_ptr = bldr.CreateBitCast(malloc_call,
      get_pointer(get_int())->llvm(), "len_ptr");

  bldr.CreateStore(len_val, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { int_size }, "raw_data_ptr");

  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  auto data_ptr = bldr.CreateBitCast(raw_data_ptr, ptr_type, "data_ptr");
  bldr.CreateStore(data_ptr, store_ptr);

  // Loop through the array and initialize each input
  auto loop_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "loop", init_fn_);

  bldr.CreateBr(loop_block);
  bldr.SetInsertPoint(loop_block);

  llvm::PHINode* phi = bldr.CreatePHI(get_uint()->llvm(), 2, "phi");
  phi->addIncoming(data::const_uint(0), fn_scope->entry_block());

  auto curr_ptr = bldr.CreateGEP(data_ptr, { phi });

  std::vector<llvm::Value*> next_init_args = { curr_ptr };
  if (data_type()->is_array()) {
    auto iters = init_fn_->args().begin();
    ++(++iters); // Start at the second length argument

    while (iters != init_fn_->args().end()) {
      next_init_args.push_back(iters);
      ++iters;
    }
  }
  bldr.CreateCall(data_type()->initialize(), next_init_args);

  auto next_data = bldr.CreateAdd(phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_data, len_val),
      loop_block, fn_scope->exit_block());
  phi->addIncoming(next_data, loop_block);

  fn_scope->exit();

  return init_fn_;
}

llvm::Function* Pointer::initialize() {
  if (init_fn_ != nullptr) return init_fn_;
  // TODO
  return nullptr;
}

llvm::Function* Tuple::initialize() {
  if (init_fn_ != nullptr) return init_fn_;
  // TODO
  return nullptr;
}

llvm::Function* UserDefined::initialize() {
  if (init_fn_ != nullptr) return init_fn_;
  // TODO
  return nullptr;
}

llvm::Value* Array::initialize_literal(llvm::IRBuilder<>& bldr, llvm::Value* runtime_len) {
  // NOTE: this cast is safe because len_ is -1 or positive. Moreover, if
  // len_ is -1, then the runtime length is what is used
  llvm::Value* len;
  if (runtime_len == nullptr) {
    len = data::const_int(0);

  } else {
    len = runtime_len;
  }

  // Compute the amount of space to allocate
  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Type::get_int()->bytes());
  auto zero = data::const_int(0);
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto raw_len_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { zero }, "array_len_raw");

  auto len_ptr = bldr.CreateBitCast(
      raw_len_ptr, Type::get_pointer(Type::get_int())->llvm(), "len_ptr");
    bldr.CreateStore(len, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { int_size }, "array_idx_raw");
  
  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  auto ret_ptr = bldr.CreateBitCast(raw_data_ptr, ptr_type, "array_ptr");


  // initialize insides here 

  auto prev_block = bldr.GetInsertBlock();
  auto parent_fn = prev_block->getParent();

  auto loop_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "init_loop", parent_fn);
  auto done_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "init_done", parent_fn);

  bldr.CreateBr(loop_block);

  bldr.SetInsertPoint(loop_block);
  llvm::PHINode* phi = bldr.CreatePHI(Type::get_uint()->llvm(), 2, "loop_phi");
  phi->addIncoming(data::const_uint(0), prev_block);

  // TODO FIXME
  // data_type()->initialize(bldr, bldr.CreateGEP(data_type()->llvm(), ret_ptr, { phi }));
  auto next_iter = bldr.CreateAdd(phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_iter, len), loop_block, done_block);
  // Need to get insert block becasue the block can be changed inside the loop.
  phi->addIncoming(next_iter, bldr.GetInsertBlock());

  bldr.SetInsertPoint(done_block);

  return ret_ptr;
}
