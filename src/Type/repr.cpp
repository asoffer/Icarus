#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;
extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

namespace cstdlib {
  extern llvm::Constant* putchar();
  extern llvm::Constant* printf();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_neg(llvm::IRBuilder<>& bldr, size_t n);
  extern llvm::Value* const_uint(size_t n);
  extern llvm::ConstantInt* const_char(char c);
  extern llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s);
}  // namespace data

void add_branch(llvm::Function* fn, Scope* fn_scope, llvm::SwitchInst* switch_stmt,
    const std::string& name, char char_to_display, const std::string& display_as,
    bool add_jump = true) {

  llvm::IRBuilder<>& fn_bldr = fn_scope->builder();

  auto branch = make_block(name, fn);
  switch_stmt->addCase(data::const_char(char_to_display), branch);
  fn_bldr.SetInsertPoint(branch);
  fn_bldr.CreateCall(cstdlib::printf(), { data::global_string(fn_bldr, display_as) });
  if (add_jump) {
    fn_bldr.CreateBr(fn_scope->exit_block());
  }
}

void Primitive::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  if (this == get_bool()) {
    if (repr_fn_ == nullptr) {
      auto fn_type = get_function(this, get_void());

      repr_fn_ = llvm::Function::Create(fn_type->llvm(),
          llvm::Function::ExternalLinkage, "repr.bool", global_module);
      llvm::Value* arg = repr_fn_->args().begin();

      FnScope* fn_scope = Scope::build<FnScope>();

      fn_scope->set_parent_function(repr_fn_);
      fn_scope->set_type(fn_type);

      llvm::IRBuilder<>& fn_bldr = fn_scope->builder();

      fn_scope->enter();
      auto true_block  = make_block("true.block",  repr_fn_);
      auto false_block = make_block("false.block", repr_fn_);
      auto merge_block = make_block("merge_block", repr_fn_);

      // NOTE: Exactly one argument is provided always
      fn_bldr.CreateCondBr(arg, true_block, false_block);

      fn_bldr.SetInsertPoint(true_block);
      fn_bldr.CreateBr(merge_block);

      fn_bldr.SetInsertPoint(false_block);
      fn_bldr.CreateBr(merge_block);

      fn_bldr.SetInsertPoint(merge_block);
      llvm::PHINode* phi_node =
        fn_bldr.CreatePHI(get_pointer(get_char())->llvm(), 2, "merge");
      phi_node->addIncoming(data::global_string(fn_bldr, "true"),  true_block);
      phi_node->addIncoming(data::global_string(fn_bldr, "false"), false_block);

      fn_bldr.CreateCall(cstdlib::printf(),
          { data::global_string(fn_bldr, "%s"), phi_node });

      fn_scope->exit();
    }

    bldr.CreateCall(repr_fn_, { val });

  } else if (this == get_char()) {
    auto fn_type = get_function(this, get_void());

    repr_fn_ = llvm::Function::Create(fn_type->llvm(),
        llvm::Function::ExternalLinkage, "repr.char", global_module);
    llvm::Value* arg = repr_fn_->args().begin();

    FnScope* fn_scope = Scope::build<FnScope>();

    fn_scope->set_parent_function(repr_fn_);
    fn_scope->set_type(fn_type);

    llvm::IRBuilder<>& fn_bldr = fn_scope->builder();

    fn_scope->enter();

    auto standard_block = make_block("standard", repr_fn_);
    auto exceptional_block = make_block("exceptional", repr_fn_);

    fn_bldr.CreateCondBr(
        fn_bldr.CreateOr(
          fn_bldr.CreateICmpUGT(data::const_char(' '), arg),
          fn_bldr.CreateOr(
            fn_bldr.CreateICmpEQ(data::const_char('\\'), arg),
          fn_bldr.CreateICmpULT(data::const_char('\x7e'), arg))),
        exceptional_block,
        standard_block);

    fn_bldr.SetInsertPoint(standard_block);
    fn_bldr.CreateCall(cstdlib::printf(), { data::global_string(fn_bldr, "'%c'"), arg });
    fn_bldr.CreateBr(fn_scope->exit_block());

    fn_bldr.SetInsertPoint(exceptional_block);

    auto branch_default = make_block("default", repr_fn_);
    
    auto switch_stmt = fn_bldr.CreateSwitch(arg, branch_default);

    fn_bldr.SetInsertPoint(branch_default);
    fn_bldr.CreateCall(cstdlib::printf(), { data::global_string(fn_bldr, "'\\x%02x'"), arg });
    fn_bldr.CreateBr(fn_scope->exit_block());

    add_branch(repr_fn_, fn_scope, switch_stmt, "tab", '\t', "'\\t'");
    add_branch(repr_fn_, fn_scope, switch_stmt, "newline", '\n', "'\\n'");
    add_branch(repr_fn_, fn_scope, switch_stmt, "carriage_return", '\r', "'\\r'");
    add_branch(repr_fn_, fn_scope, switch_stmt, "backslash", '\\', "'\\\\'", false);

    fn_scope->exit();

    bldr.CreateCall(repr_fn_, { val });

  } else if (this == get_int()) {
    bldr.CreateCall(cstdlib::printf(),
        { data::global_string(bldr, "%d"), val });

  } else if (this == get_real()) {
    bldr.CreateCall(cstdlib::printf(),
        { data::global_string(bldr, "%f"), val });

  } else if (this == get_type()) {
    // NOTE: BE VERY CAREFUL HERE. YOU ARE TYPE PUNNING!
    auto type_val = reinterpret_cast<Type*>(val);

    bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "%s"),
        data::global_string(bldr, type_val->to_string()) });

  } else if (this == get_uint()) {
    bldr.CreateCall(cstdlib::printf(),
        { data::global_string(bldr, "%uu"), val });
  }
}

void Array::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  if (repr_fn_ == nullptr) {
    // TODO what about arrays of types?
    auto fn_type = get_function(this, get_void());
    repr_fn_ = llvm::Function::Create(fn_type->llvm(),
        llvm::Function::ExternalLinkage, "print." + to_string(), global_module);
    llvm::Value* arg = repr_fn_->args().begin();

    FnScope* fn_scope = Scope::build<FnScope>();

    fn_scope->set_parent_function(repr_fn_);
    fn_scope->set_type(fn_type);

    llvm::IRBuilder<>& fn_bldr = fn_scope->builder();

    fn_scope->enter();
    fn_bldr.CreateCall(cstdlib::putchar(), { data::const_char('[') });

    auto basic_ptr_type = get_pointer(get_char())->llvm();

    auto raw_len_ptr = fn_bldr.CreateGEP(
        fn_bldr.CreateBitCast(arg, basic_ptr_type),
        { data::const_neg(fn_bldr, get_uint()->bytes()) }, "ptr_to_len");

    auto len_ptr = fn_bldr.CreateBitCast(raw_len_ptr, get_pointer(get_uint())->llvm());
    auto len_val = fn_bldr.CreateLoad(fn_bldr.CreateGEP(len_ptr, { data::const_uint(0) }));

    auto loop_block = make_block("loop.body", repr_fn_);
    auto loop_head_block = make_block("loop.head", repr_fn_);
    auto done_block = make_block("loop.done", repr_fn_);

    fn_bldr.CreateCondBr(fn_bldr.CreateICmpEQ(len_val, data::const_uint(0)),
        done_block, loop_head_block);

    fn_bldr.SetInsertPoint(loop_head_block);

    // Start at position 1, not zero
    auto start_ptr = fn_bldr.CreateGEP(arg, { data::const_uint(1) });
    auto end_ptr = fn_bldr.CreateGEP(arg, { len_val });

    // TODO is this const_uint(0) superfluous?
    auto elem_ptr = fn_bldr.CreateGEP(arg, { data::const_uint(0) });

    data_type()->call_repr(fn_bldr, fn_bldr.CreateLoad(elem_ptr));
    fn_bldr.CreateCondBr(fn_bldr.CreateICmpEQ(len_val, data::const_uint(1)),
        done_block, loop_block);

    fn_bldr.SetInsertPoint(loop_block);
    llvm::PHINode* phi = fn_bldr.CreatePHI(get_pointer(data_type())->llvm(), 2, "loop_phi");
    phi->addIncoming(start_ptr, loop_head_block);

    fn_bldr.CreateCall(cstdlib::printf(), { data::global_string(fn_bldr, ", ") });

    data_type()->call_repr(fn_bldr, fn_bldr.CreateLoad(phi));

    auto next_ptr = fn_bldr.CreateGEP(phi, data::const_uint(1));
    fn_bldr.CreateCondBr(fn_bldr.CreateICmpULT(next_ptr, end_ptr), loop_block, done_block);
    phi->addIncoming(next_ptr, loop_block);
    fn_bldr.SetInsertPoint(done_block);

    fn_bldr.CreateCall(cstdlib::putchar(), { data::const_char(']') });
    fn_scope->exit();
  }

  bldr.CreateCall(repr_fn_, { val });
}

void Function::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "%s"),
      data::global_string(bldr, "<function " + to_string() + ">") });
}

void Pointer::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  bldr.CreateCall(cstdlib::printf(), { data::global_string(bldr, "&_%x"), val });
}

void Tuple::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {}
void UserDefined::call_repr(llvm::IRBuilder<>& bldr, llvm::Value* val) {}
