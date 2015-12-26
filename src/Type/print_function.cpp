#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> global_builder;

namespace cstdlib {
  extern llvm::Constant* putchar();
  extern llvm::Constant* printf();

}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_uint(size_t n);
  extern llvm::Value* const_int(int n, bool is_signed = false);
  extern llvm::Value* const_char(char c);
  extern llvm::Value* global_string(const std::string& s);
}  // namespace data


llvm::Function* Primitive::print() {
  if (print_fn_ != nullptr) return print_fn_;

  // char doesn't require building a new function. We can just
  // call putchar outright.
  if (this == Type::get_char()) {
    return print_fn_ = static_cast<llvm::Function*>(cstdlib::putchar());
  }
  
  // Types require compile-time generated strings. Moreover the .llvm()
  // method returns a nullptr for type Types. Instead, we pass in its
  // string representation, but this means we must take 

  auto input_type = replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  // Otherwise, actually write our own
  print_fn_ = llvm::Function::Create(
      Type::get_function(input_type, get_void())->llvm(),
      llvm::Function::ExternalLinkage, "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  FnScope* fn_scope = Scope::build<FnScope>();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  if (this == get_bool()) {
    auto true_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "true_block", print_fn_);
    auto false_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "false_block", print_fn_);
    auto merge_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "merge_block", print_fn_);

    // NOTE: Exactly one argument is provided always
    bldr.CreateCondBr(val, true_block, false_block);

    bldr.SetInsertPoint(true_block);
    bldr.CreateBr(merge_block);

    bldr.SetInsertPoint(false_block);
    bldr.CreateBr(merge_block);

    bldr.SetInsertPoint(merge_block);
    llvm::PHINode* phi_node =
      bldr.CreatePHI(get_pointer(get_char())->llvm(), 2, "merge");
    phi_node->addIncoming(data::global_string("true"),  true_block);
    phi_node->addIncoming(data::global_string("false"), false_block);

    bldr.CreateCall(cstdlib::printf(), { data::global_string("%s"), phi_node });

  } else if (this == get_int()) {
    bldr.CreateCall(cstdlib::printf(), { data::global_string("%d"), val });

  } else if (this == get_uint()) {
    bldr.CreateCall(cstdlib::printf(), { data::global_string("%u"), val });

  } else if (this == get_real()) {
    bldr.CreateCall(cstdlib::printf(), { data::global_string("%f"), val });

  } else if (this == get_type()) {
    // To print a type we must first get it's string representation,
    // which is the job of the code generator. We assume that the value
    // passed in is already this string (as a global string pointer)

    bldr.CreateCall(cstdlib::printf(), { data::global_string("%s"), val });
  }

  fn_scope->exit();

  return print_fn_;
}

// There must be a better way to do this.
llvm::Function* Array::print() {
  if (print_fn_ != nullptr) return print_fn_;

  auto input_type =
    replace(get_type(), get_pointer(get_char()));

  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        get_void()->llvm(), { input_type->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  FnScope* fn_scope = Scope::build<FnScope>();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::putchar(), { data::const_char('[') });

  auto basic_ptr_type = get_pointer(get_char())->llvm();

  auto raw_len_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, basic_ptr_type),
      { data::const_int(-4, true) }, "ptr_to_len");

  auto len_ptr = bldr.CreateBitCast(raw_len_ptr,
      get_pointer(get_int())->llvm());

  auto array_len = bldr.CreateLoad(bldr.CreateGEP(len_ptr, { data::const_int(0) }));

  auto loop_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "print_loop", print_fn_);
  auto loop_head_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "print_loop_head", print_fn_);
  auto done_block = llvm::BasicBlock::Create(
    llvm::getGlobalContext(), "print_done", print_fn_);

  bldr.CreateCondBr(bldr.CreateICmpEQ(array_len, data::const_uint(0)),
      done_block, loop_head_block);

  bldr.SetInsertPoint(loop_head_block);

  // TODO is this const_int(0) superfluous?
  auto elem_ptr = bldr.CreateGEP(val, { data::const_uint(0) });

  bldr.CreateCall(data_type()->repr(), { bldr.CreateLoad(elem_ptr) });
  bldr.CreateCondBr(bldr.CreateICmpEQ(array_len, data::const_uint(1)),
      done_block, loop_block);

  bldr.SetInsertPoint(loop_block);
  llvm::PHINode* phi = bldr.CreatePHI(get_uint()->llvm(), 2, "loop_phi");
  phi->addIncoming(data::const_uint(1), loop_head_block);

  bldr.CreateCall(cstdlib::printf(), { data::global_string(", ") });

  elem_ptr = bldr.CreateGEP(val, { phi });
  bldr.CreateCall(data_type()->repr(), { bldr.CreateLoad(elem_ptr) });

  auto next_iter = bldr.CreateAdd(phi, data::const_uint(1));
  bldr.CreateCondBr(bldr.CreateICmpULT(next_iter, array_len), loop_block, done_block);
  phi->addIncoming(next_iter, loop_block);
  bldr.SetInsertPoint(done_block);

  bldr.CreateCall(cstdlib::putchar(), { data::const_char(']') });
  fn_scope->exit();

  return print_fn_;
}

llvm::Function* Function::print() {
  if (print_fn_ != nullptr) return print_fn_;

  auto fn_print_str = global_builder.CreateGlobalStringPtr(
      "<function " + to_string() + ">");

  auto input_type = replace(get_type(), get_pointer(get_char()));

  // TODO
  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        get_void()->llvm(), { get_pointer(input_type)->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);

  FnScope* fn_scope = Scope::build<FnScope>();

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::printf(), { data::global_string("%s"), fn_print_str });
  fn_scope->exit();

  return print_fn_;
}

// TODO complete these
llvm::Function* Pointer::print() { return nullptr; }
llvm::Function* Tuple::print() { return nullptr; }

llvm::Function* UserDefined::print() { return nullptr; }
