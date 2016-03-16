#include "Type.h"
#include "Scope.h"

extern llvm::Value *PtrCallFix(llvm::IRBuilder<> &bldr, Type *t, llvm::Value *ptr);

extern llvm::Module *global_module;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
namespace cstdlib {
extern llvm::Constant *putchar();
extern llvm::Constant *printf();
} // namespace cstdlib

namespace data {
extern llvm::Value *const_uint(size_t n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::Value *global_string(llvm::IRBuilder<> &bldr,
                                  const std::string &s);
} // namespace data

void add_branch(llvm::Function *fn, llvm::IRBuilder<> &bldr,
                llvm::SwitchInst *switch_stmt, const std::string &name,
                char char_to_display, const std::string &display_as,
                llvm::BasicBlock *jump_block = nullptr) {

  auto branch = make_block(name, fn);
  switch_stmt->addCase(data::const_char(char_to_display), branch);
  bldr.SetInsertPoint(branch);
  bldr.CreateCall(cstdlib::printf(), {data::global_string(bldr, display_as)});
  if (jump_block) { bldr.CreateBr(jump_block); }
}

void Primitive::call_repr(llvm::Value *val) {
  if (this == Bool) {
    if (repr_fn_ == nullptr) {
      repr_fn_ = llvm::Function::Create(*Func(this, Void),
                                        llvm::Function::ExternalLinkage,
                                        "repr.bool", global_module);
      llvm::Value *arg = repr_fn_->args().begin();

      auto entry = make_block("entry", repr_fn_);
      llvm::IRBuilder<> fn_bldr(llvm::getGlobalContext());
      fn_bldr.SetInsertPoint(entry);
      auto offset = fn_bldr.CreateMul(fn_bldr.CreateZExt(arg, *RawPtr),
                                      data::const_uint(6));

      auto str = fn_bldr.CreateGEP(data::global_string(fn_bldr, "false\0true"),
                                   offset);
      fn_bldr.CreateCall(cstdlib::printf(), str);
      fn_bldr.CreateRetVoid();
    }

    builder.CreateCall(repr_fn_, {val});

  } else if (this == Char) {
    if (repr_fn_ == nullptr) {
      repr_fn_ = llvm::Function::Create(*Func(this, Void),
                                        llvm::Function::ExternalLinkage,
                                        "repr.char", global_module);
      llvm::Value *arg = repr_fn_->args().begin();

      llvm::IRBuilder<> fn_bldr(llvm::getGlobalContext());
      auto entry = make_block("entry", repr_fn_);
      auto exit = make_block("exit", repr_fn_);
      fn_bldr.SetInsertPoint(entry);

      auto standard_block    = make_block("standard", repr_fn_);
      auto exceptional_block = make_block("exceptional", repr_fn_);

      fn_bldr.CreateCondBr(
          fn_bldr.CreateOr(
              fn_bldr.CreateICmpUGT(data::const_char(' '), arg),
              fn_bldr.CreateOr(
                  fn_bldr.CreateICmpEQ(data::const_char('\\'), arg),
                  fn_bldr.CreateICmpULT(data::const_char('\x7e'), arg))),
          exceptional_block, standard_block);

      fn_bldr.SetInsertPoint(standard_block);
      fn_bldr.CreateCall(cstdlib::printf(),
                         {data::global_string(fn_bldr, "'%c'"), arg});
      fn_bldr.CreateBr(exit);

      fn_bldr.SetInsertPoint(exceptional_block);

      auto branch_default = make_block("default", repr_fn_);

      auto switch_stmt = fn_bldr.CreateSwitch(arg, branch_default);

      fn_bldr.SetInsertPoint(branch_default);
      fn_bldr.CreateCall(cstdlib::printf(),
                         {data::global_string(fn_bldr, "'\\x%02x'"), arg});
      fn_bldr.CreateBr(exit);

      add_branch(repr_fn_, fn_bldr, switch_stmt, "tab", '\t', "'\\t'", exit);
      add_branch(repr_fn_, fn_bldr, switch_stmt, "newline", '\n', "'\\n'",
                 exit);
      add_branch(repr_fn_, fn_bldr, switch_stmt, "carriage_return", '\r',
                 "'\\r'", exit);
      add_branch(repr_fn_, fn_bldr, switch_stmt, "backslash", '\\', "'\\\\'",
                 exit);

      fn_bldr.SetInsertPoint(exit);
      fn_bldr.CreateRetVoid();
    }

    builder.CreateCall(repr_fn_, {val});

  } else if (this == Int) {
    builder.CreateCall(cstdlib::printf(),
                       {data::global_string(builder, "%d"), val});

  } else if (this == Real) {
    builder.CreateCall(cstdlib::printf(),
                       {data::global_string(builder, "%f"), val});

  } else if (this == Type_) {
    // NOTE: BE VERY CAREFUL HERE. YOU ARE TYPE PUNNING!
    auto type_val = reinterpret_cast<Type *>(val);

    builder.CreateCall(cstdlib::printf(),
                       {data::global_string(builder, "%s"),
                        data::global_string(builder, type_val->to_string())});

  } else if (this == Uint) {
    builder.CreateCall(cstdlib::printf(),
                       {data::global_string(builder, "%uu"), val});
  }
}

void Array::call_repr(llvm::Value *val) {
  if (repr_fn_ == nullptr) {
    // TODO what about arrays of types?
    auto fn_type = Func(Ptr(this), Void);
    repr_fn_ = llvm::Function::Create(*fn_type, llvm::Function::ExternalLinkage,
                                      "repr." + Mangle(this), global_module);
    llvm::Value *arg = repr_fn_->args().begin();

    llvm::IRBuilder<> fn_bldr(llvm::getGlobalContext());
    auto enter_block = make_block("enter", repr_fn_);
    fn_bldr.SetInsertPoint(enter_block);

    fn_bldr.CreateCall(cstdlib::putchar(), {data::const_char('[')});

    auto len_ptr =
        fn_bldr.CreateGEP(arg, {data::const_uint(0), data::const_uint(0)});
    auto len_val = fn_bldr.CreateLoad(len_ptr);

    auto loop_block      = make_block("loop.body", repr_fn_);
    auto loop_head_block = make_block("loop.head", repr_fn_);
    auto done_block      = make_block("exit", repr_fn_);

    fn_bldr.CreateCondBr(fn_bldr.CreateICmpEQ(len_val, data::const_uint(0)),
                         done_block, loop_head_block);

    fn_bldr.SetInsertPoint(loop_head_block);

    // Start at position 1, not zero
    auto data_ptr_ptr = fn_bldr.CreateGEP(
        arg, {data::const_uint(0), data::const_uint(1)}, "data_ptr_ptr");
    llvm::Value *start_ptr = fn_bldr.CreateLoad(data_ptr_ptr, "start_ptr");
    auto end_ptr           = fn_bldr.CreateGEP(start_ptr, {len_val}, "end_ptr");

    // TODO make calls to call_repr not have to first check if we pass the
    // object or a pointer to the object.
    data_type->call_repr(PtrCallFix(fn_bldr, data_type, start_ptr));

    start_ptr =
        fn_bldr.CreateGEP(start_ptr, data::const_uint(1), "second_elem");
    fn_bldr.CreateCondBr(fn_bldr.CreateICmpEQ(start_ptr, end_ptr), done_block,
                         loop_block);

    // Otherwise, print ", element" repeatedly.
    fn_bldr.SetInsertPoint(loop_block);
    auto phi = fn_bldr.CreatePHI(*Ptr(data_type), 2, "loop_phi");
    phi->addIncoming(start_ptr, loop_head_block);

    fn_bldr.CreateCall(cstdlib::printf(), {data::global_string(fn_bldr, ", ")});

    // TODO make calls to call_repr not have to first check if we pass the
    // object or a pointer to the object.
    data_type->call_repr(PtrCallFix(fn_bldr, data_type, phi));

    auto next_ptr = fn_bldr.CreateGEP(phi, data::const_uint(1));
    fn_bldr.CreateCondBr(fn_bldr.CreateICmpULT(next_ptr, end_ptr), loop_block,
                         done_block);
    phi->addIncoming(next_ptr, loop_block);
    fn_bldr.SetInsertPoint(done_block);

    fn_bldr.CreateCall(cstdlib::putchar(), {data::const_char(']')});
    fn_bldr.CreateRetVoid();
  }

  builder.CreateCall(repr_fn_, {val});
}

// NOTE: [function ...] probably looks too much like an array. That's why you
// used <function ...> in the first place.
void Function::call_repr(llvm::Value *val) {
  builder.CreateCall(
      cstdlib::printf(),
      {data::global_string(builder, "%s"),
       data::global_string(builder, "<function " + to_string() + ">")});
}

void ForwardDeclaration::call_repr(llvm::Value *val) {
  assert(false && "Cannot represent a forward declaration");
}

void DependentType::call_repr(llvm::Value *val) {
  builder.CreateCall(
      cstdlib::printf(),
      {data::global_string(builder, "%s"),
       data::global_string(builder, "<dependent " + to_string() + ">")});
}

void Pointer::call_repr(llvm::Value *val) {
  builder.CreateCall(cstdlib::printf(), {data::global_string(builder, "&_%x"), val});
}

void Enumeration::call_repr(llvm::Value *val) {
  // TODO print the enum's name as a string. This requires preallocating
  // an array of global strings and accesssing that.
  //
  // For now, just print the number in brackets after the enums name
  auto val_str = builder.CreateLoad(
      builder.CreateGEP(string_data, {data::const_uint(0), val}));
  builder.CreateCall(
      cstdlib::printf(),
      {data::global_string(builder, to_string() + ".%s"), val_str});
}

void Tuple::call_repr(llvm::Value *val) {}
void Structure::call_repr(llvm::Value *val) {}
void TypeVariable::call_repr(llvm::Value *val) {}
