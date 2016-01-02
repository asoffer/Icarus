#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* printf();
}  // namespace cstdlib

namespace data {
  extern llvm::ConstantInt* const_char(char c);
  extern llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s);
}  // namespace data


llvm::Function* Primitive::repr() {
  if (repr_fn_ != nullptr) return repr_fn_;

  // char doesn't require building a new function. We can just
  // call putchar outright.
  if (this == Type::get_char()) {
    repr_fn_ = llvm::Function::Create(
        llvm::FunctionType::get(get_void()->llvm(), { llvm() }, false),
        llvm::Function::ExternalLinkage, "repr." + to_string(), global_module);
    llvm::Value* val = repr_fn_->args().begin();

    FnScope* fn_scope = Scope::build<FnScope>();

    fn_scope->set_parent_function(repr_fn_);
    fn_scope->set_type(get_function(this, get_void()));

    llvm::IRBuilder<>& bldr = fn_scope->builder();

    fn_scope->enter();

    auto standard_block =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "standard", repr_fn_);
    auto exceptional_block =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "exceptional", repr_fn_);

    bldr.CreateCondBr(
        bldr.CreateOr(
          bldr.CreateICmpUGT(data::const_char(' '), val),
          bldr.CreateOr(
            bldr.CreateICmpEQ(data::const_char('\\'), val),
          bldr.CreateICmpULT(data::const_char('\x7e'), val))),
        exceptional_block,
        standard_block);

    bldr.SetInsertPoint(standard_block);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'%c'"), val });
    bldr.CreateBr(fn_scope->exit_block());

    bldr.SetInsertPoint(exceptional_block);

    auto branch_default =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "default", repr_fn_);
    
    auto switch_stmt = bldr.CreateSwitch(val, branch_default);

    bldr.SetInsertPoint(branch_default);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'\\x%02x'"), val });
    bldr.CreateBr(fn_scope->exit_block());

    auto branch_tab =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "tab", repr_fn_);
    switch_stmt->addCase(data::const_char('\t'), branch_tab);
    bldr.SetInsertPoint(branch_tab);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'\\t'") });
    bldr.CreateBr(fn_scope->exit_block());

    auto branch_newline =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "newline", repr_fn_);
    switch_stmt->addCase(data::const_char('\n'), branch_newline);
    bldr.SetInsertPoint(branch_newline);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'\\n'") });
    bldr.CreateBr(fn_scope->exit_block());

    auto branch_carriage_return =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "carriage_return", repr_fn_);
    switch_stmt->addCase(data::const_char('\r'), branch_carriage_return);
    bldr.SetInsertPoint(branch_carriage_return);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'\\r'") });
    bldr.CreateBr(fn_scope->exit_block());

    auto branch_backslash =
      llvm::BasicBlock::Create(llvm::getGlobalContext(), "backslash", repr_fn_);
    switch_stmt->addCase(data::const_char('\\'), branch_backslash);
    bldr.SetInsertPoint(branch_backslash);
    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "'\\\\'") });
    // bldr.CreateBr(fn_scope->exit_block());

    fn_scope->exit();
    return repr_fn_;
  }

  return repr_fn_ = print();
}


