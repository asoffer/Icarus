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

void Primitive::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  llvm::Value* init_val = nullptr;
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
    default: return;
  }

  bldr.CreateStore(init_val, var);
}

// Nothing to be initialized here
void Function::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {}

void Array::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {
#ifdef DEBUG 
  std::cerr << "FATAL: Cannot initialize an array without length information" << std::endl;
#endif
}

void Array::initialize_array(llvm::IRBuilder<>& bldr, 
    llvm::Value* var, std::vector<llvm::Value*> lengths) {

  std::vector<llvm::Value*>
    init_args = { bldr.CreateGEP(var, { data::const_uint(0) }) };
  init_args.insert(init_args.end(), lengths.begin(), lengths.end());

  bldr.CreateCall(init_fn_, init_args);
}

void Pointer::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  // Initialize to zero
  bldr.CreateStore(data::const_int(0), var);
}

void Tuple::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  // TODO
}

llvm::Value* Array::initialize_literal(llvm::IRBuilder<>& bldr, llvm::Value* runtime_len) const {
  // NOTE: this cast is safe because len_ is -1 or positive. Moreover, if
  // len_ is -1, then the runtime length is what is used
  llvm::Value* len;
  if (len_ != -1) {
    len = data::const_int(len_);

  } else if (runtime_len == nullptr) {
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

  data_type()->initialize(bldr, bldr.CreateGEP(data_type()->llvm(), ret_ptr, { phi }));
  auto next_iter = bldr.CreateAdd(phi, data::const_uint(1));

  bldr.CreateCondBr(bldr.CreateICmpULT(next_iter, len), loop_block, done_block);
  // Need to get insert block becasue the block can be changed inside the loop.
  phi->addIncoming(next_iter, bldr.GetInsertBlock());

  bldr.SetInsertPoint(done_block);

  return ret_ptr;
}

void UserDefined::initialize(llvm::IRBuilder<>& bldr, llvm::Value* var) {
  // TODO
}
