#include "Type.h"
#include "Scope.h"

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

namespace cstdlib {
  extern llvm::Constant* calloc();
  extern llvm::Constant* malloc();
}  // namespace cstdlib


namespace data {
  extern llvm::Value* null_pointer(Type* t);
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* const_real(double d);
  extern llvm::Value* const_true();
  extern llvm::Value* const_false();
}  // namespace data

extern llvm::Module* global_module;

llvm::Function* get_llvm_init(Type* type) {
  return llvm::Function::Create(
      Type::get_function(Type::get_pointer(type), Type::get_void())->llvm(),
      llvm::Function::ExternalLinkage, "init." + type->to_string(),
      global_module);
}


llvm::Function* Primitive::initialize() {
  if (init_fn_ != nullptr) return init_fn_;

  init_fn_ = get_llvm_init(this);
  
  auto block = make_block("entry", init_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  llvm::Value* init_val;
  switch (prim_type_) {
    case t_bool: init_val = data::const_false(); break;
    case t_char: init_val = data::const_char('\0'); break;
    case t_int:
    case t_uint: init_val = data::const_uint(0); break;
    case t_real: init_val = data::const_real(0); break;
    default:     return nullptr;
  }

  bldr.CreateCall(assign(), { init_val, init_fn_->args().begin() });
  bldr.CreateRetVoid();

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
  fn_scope->set_type(get_function(this, get_void()));

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
      bldr.CreateMul(len_val, bytes_per_elem), "alloc_bytes");

  auto data_is_primitive = data_type()->is_primitive();

  // (M/C)alloc call
  auto alloc_call = bldr.CreateCall(
      data_is_primitive ? cstdlib::calloc() : cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto len_ptr = bldr.CreateBitCast(alloc_call,
      get_pointer(get_uint())->llvm(), "len_ptr");

  bldr.CreateStore(len_val, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      alloc_call, { int_size }, "raw_data_ptr");

  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  auto data_ptr = bldr.CreateBitCast(raw_data_ptr, ptr_type, "data_ptr");
  bldr.CreateStore(data_ptr, store_ptr);

  // Just calling calloc is okay for p
  if (!data_is_primitive) {
    auto end_ptr = bldr.CreateGEP(data_ptr, { len_val });

    // Loop through the array and initialize each input
    auto loop_block = make_block("loop", init_fn_);

    bldr.CreateBr(loop_block);
    bldr.SetInsertPoint(loop_block);

    llvm::PHINode* phi = bldr.CreatePHI(get_pointer(data_type())->llvm(), 2, "phi");
    phi->addIncoming(data_ptr, fn_scope->entry_block());

    std::vector<llvm::Value*> next_init_args = { phi };
    if (data_type()->is_array()) {
      auto iters = init_fn_->args().begin();
      ++(++iters); // Start at the second length argument

      while (iters != init_fn_->args().end()) {
        next_init_args.push_back(iters);
        ++iters;
      }
    }
    bldr.CreateCall(data_type()->initialize(), next_init_args);

    auto next_ptr = bldr.CreateGEP(phi, { data::const_uint(1) });

    bldr.CreateCondBr(bldr.CreateICmpULT(next_ptr, end_ptr),
        loop_block, fn_scope->exit_block());
    phi->addIncoming(next_ptr, loop_block);
  }

  fn_scope->exit();

  return init_fn_;
}

llvm::Function* Pointer::initialize() {
  if (init_fn_ != nullptr) return init_fn_;

  init_fn_ = get_llvm_init(this);
  
  auto block = make_block("entry", init_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  llvm::Value* init_val = data::null_pointer(pointee_type_);
  bldr.CreateCall(assign(), { init_val, init_fn_->args().begin() });
  bldr.CreateRetVoid();

  return init_fn_;
}

llvm::Function* Tuple::initialize() {
  if (init_fn_ != nullptr) return init_fn_;
  // TODO
  return nullptr;
}

llvm::Function* UserDefined::initialize() {
  if (init_fn_ != nullptr) return init_fn_;

  init_fn_ = get_llvm_init(this);
  
  auto block = make_block("entry", init_fn_);

  llvm::IRBuilder<> bldr(llvm::getGlobalContext());
  bldr.SetInsertPoint(block);

  // initialize all fields
  auto fields_size = fields_.size();
  for (size_t field_num = 0; field_num < fields_size; ++field_num) {
    auto field_type = fields_[field_num].second;
    auto arg = bldr.CreateGEP(llvm(), init_fn_->args().begin(), {
        data::const_uint(0), data::const_uint(field_num) });

    if (field_type->is_array()) {
      bldr.CreateCall(field_type->initialize(), { arg, data::const_uint(0) });
    } else {
      bldr.CreateCall(field_type->initialize(), { arg });
    }

  }

  bldr.CreateRetVoid();

  return init_fn_;
}

llvm::Value* Array::initialize_literal(llvm::IRBuilder<>& bldr, llvm::Value* runtime_len) {
  // TODO determine when this can be freed. Currently just being leaked.

  llvm::Value* len =
    (runtime_len == nullptr)
    ? data::const_uint(0)
    : runtime_len;


  // Compute the amount of space to allocate
  auto bytes_per_elem = data::const_uint(data_type()->bytes());
  auto int_size = data::const_uint(Type::get_uint()->bytes());
  auto bytes_needed = bldr.CreateAdd(int_size, 
      bldr.CreateMul(len, bytes_per_elem), "malloc_bytes");

  // Malloc call
  auto malloc_call = bldr.CreateCall(cstdlib::malloc(), { bytes_needed });

  // Pointer to the length at the head of the array
  auto raw_len_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { data::const_uint(0) }, "array_len_raw");

  auto len_ptr = bldr.CreateBitCast(
      raw_len_ptr, Type::get_pointer(Type::get_int())->llvm(), "len_ptr");
    bldr.CreateStore(len, len_ptr);

  // Pointer to the array data
  auto raw_data_ptr = bldr.CreateGEP(Type::get_char()->llvm(),
      malloc_call, { int_size }, "array_idx_raw");
  
  // Pointer to data cast
  auto ptr_type = Type::get_pointer(data_type())->llvm();
  auto ret_ptr = bldr.CreateBitCast(raw_data_ptr, ptr_type, "array_ptr");

  return ret_ptr;
}
