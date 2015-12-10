#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> global_builder;

namespace cstdlib {
  extern llvm::Constant* putchar();
  extern llvm::Constant* printf();

  template<char C> llvm::Value* format() {
    std::string char_str(1, C);
    static llvm::Value* format_ =
      global_builder.CreateGlobalStringPtr("%" + char_str, "percent_" + char_str);

    return format_;
  }
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(size_t n, bool is_signed = false);
  extern llvm::Value* const_char(char c);
}  // namespace data


llvm::Function* Primitive::print_function() {
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
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { input_type->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  Scope::Scope* fn_scope = Scope::Scope::build(ScopeType::func);

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  if (this == Type::get_bool()) {
    auto true_str = bldr.CreateGlobalStringPtr("true");
    auto false_str = bldr.CreateGlobalStringPtr("false");

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
      bldr.CreatePHI(llvm::Type::getInt8PtrTy(llvm::getGlobalContext()), 2, "merge");
    phi_node->addIncoming(true_str, true_block);
    phi_node->addIncoming(false_str, false_block);

    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), phi_node });

  } else if (this == Type::get_int() || this == Type::get_uint()) {
    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'d'>(), val });

  } else if (this == Type::get_real()) {
    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'f'>(), val });

  } else if (this == Type::get_type()) {
    // To print a type we must first get it's string representation,
    // which is the job of the code generator. We assume that the value
    // passed in is already this string (as a global string pointer)

    bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), val });
  }

  fn_scope->exit();

  return print_fn_;
}

// There must be a better way to do this.
llvm::Function* Array::print_function() {
  if (print_fn_ != nullptr) return print_fn_;

  auto input_type =
    replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { input_type->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);
  llvm::Value* val = print_fn_->args().begin();

  Scope::Scope* fn_scope = Scope::Scope::build(ScopeType::func);

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::putchar(), { data::const_char('[') });

  // TODO should we do this by building an AST and generating code from it?

  auto basic_ptr_type = Type::get_pointer(Type::get_char())->llvm();

  auto four = data::const_int(4, true);
  auto zero = data::const_int(0, true);
  auto neg_four = bldr.CreateSub(zero, four);

  auto raw_len_ptr = bldr.CreateGEP(
      bldr.CreateBitCast(val, basic_ptr_type),
      { neg_four }, "ptr_to_len");

  auto len_ptr = bldr.CreateBitCast(raw_len_ptr,
      Type::get_pointer(Type::get_int())->llvm());

  auto array_len = bldr.CreateLoad(bldr.CreateGEP(len_ptr, { zero }));

  auto non_empty_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "non_empty", print_fn_);
  auto cond_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "cond", print_fn_);
  auto loop_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "loop", print_fn_);
  auto land_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "land", print_fn_);
  auto finish_block = llvm::BasicBlock::Create(
      llvm::getGlobalContext(), "finish", print_fn_);


  bldr.CreateCondBr(bldr.CreateICmpEQ(array_len, data::const_int(0)),
      finish_block, non_empty_block);
  bldr.SetInsertPoint(non_empty_block);

  auto array_len_less_one = bldr.CreateSub(array_len, data::const_int(1));

  auto i_store = bldr.CreateAlloca(Type::get_int()->llvm(), nullptr, "i");
  bldr.CreateStore(zero, i_store);
  bldr.CreateBr(cond_block);

  bldr.SetInsertPoint(cond_block);
  auto i_val = bldr.CreateLoad(i_store);
  auto cmp_val = bldr.CreateICmpSLT(i_val, array_len_less_one);
  bldr.CreateCondBr(cmp_val, loop_block, land_block);
  bldr.SetInsertPoint(loop_block);

  auto elem_ptr = bldr.CreateGEP(val, { i_val });

  bldr.CreateCall(data_type()->print_function(), { bldr.CreateLoad(elem_ptr) });

  auto comma_space = global_builder.CreateGlobalStringPtr(", ");
  bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), comma_space });

  auto new_i_val = bldr.CreateAdd(i_val, data::const_int(1));
  bldr.CreateStore(new_i_val, i_store);
  bldr.CreateBr(cond_block);

  bldr.SetInsertPoint(land_block);
  auto last_elem = bldr.CreateGEP(val, { array_len_less_one });
  bldr.CreateCall(data_type()->print_function(), { bldr.CreateLoad(last_elem) });

  bldr.CreateBr(finish_block);

  bldr.SetInsertPoint(finish_block);
  bldr.CreateCall(cstdlib::putchar(), { data::const_char(']') });

  fn_scope->exit();

  return print_fn_;
}

llvm::Function* Function::print_function() {
  if (print_fn_ != nullptr) return print_fn_;

  auto fn_print_str = global_builder.CreateGlobalStringPtr(
      "<function " + to_string() + ">");

  auto input_type = replace(Type::get_type(), Type::get_pointer(Type::get_char()));

  // TODO
  print_fn_ = llvm::Function::Create(
      llvm::FunctionType::get(
        Type::get_void()->llvm(), { Type::get_pointer(input_type)->llvm() }, false),
      llvm::Function::ExternalLinkage,
      "print." + to_string(), global_module);

  Scope::Scope* fn_scope = Scope::Scope::build(ScopeType::func);

  fn_scope->set_parent_function(print_fn_);
  fn_scope->set_return_type(Type::get_void());

  llvm::IRBuilder<>& bldr = fn_scope->builder();

  fn_scope->enter();
  bldr.CreateCall(cstdlib::printf(), { cstdlib::format<'s'>(), fn_print_str });
  fn_scope->exit();

  return print_fn_;
}

// TODO complete these
llvm::Function* Pointer::print_function() { return nullptr; }
llvm::Function* Tuple::print_function() { return nullptr; }
