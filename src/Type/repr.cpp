#include "Type.h"
#include "Scope.h"

extern llvm::Value *PtrCallFix(llvm::IRBuilder<> &bldr, Type *t,
                               llvm::Value *ptr);

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
extern llvm::Value *global_string(const std::string &s);
} // namespace data

void add_branch(llvm::Function *fn, llvm::SwitchInst *switch_stmt,
                const std::string &name, char char_to_display,
                const std::string &display_as) {

  auto branch = make_block(name, fn);
  switch_stmt->addCase(data::const_char(char_to_display), branch);
  builder.SetInsertPoint(branch);
  builder.CreateCall(cstdlib::printf(), {data::global_string(display_as)});
  builder.CreateRetVoid();
}

void Primitive::call_repr(llvm::Value *val) {
  auto prev_block = builder.GetInsertBlock();

  if (this == Bool) {
    if (repr_fn_ == nullptr) {
      repr_fn_ = llvm::Function::Create(*Func(this, Void),
                                        llvm::Function::ExternalLinkage,
                                        "repr.bool", global_module);
      llvm::Value *arg = repr_fn_->args().begin();
      arg->setName("b");

      auto entry       = make_block("entry", repr_fn_);
      auto true_block  = make_block("true.block", repr_fn_);
      auto false_block = make_block("false.block", repr_fn_);
      builder.SetInsertPoint(entry);
      builder.CreateCondBr(arg, true_block, false_block);

      builder.SetInsertPoint(true_block);
      builder.CreateCall(cstdlib::printf(), data::global_string("true"));
      builder.CreateRetVoid();

      builder.SetInsertPoint(false_block);
      builder.CreateCall(cstdlib::printf(), data::global_string("false"));
      builder.CreateRetVoid();
    }

    builder.SetInsertPoint(prev_block);
    builder.CreateCall(repr_fn_, {val});

  } else if (this == Char) {
    if (repr_fn_ == nullptr) {
      repr_fn_ = llvm::Function::Create(*Func(this, Void),
                                        llvm::Function::ExternalLinkage,
                                        "repr.char", global_module);
      llvm::Value *arg = repr_fn_->args().begin();

      auto entry = make_block("entry", repr_fn_);
      builder.SetInsertPoint(entry);

      auto standard_block    = make_block("standard", repr_fn_);
      auto exceptional_block = make_block("exceptional", repr_fn_);

      builder.CreateCondBr(
          builder.CreateOr(
              builder.CreateICmpUGT(data::const_char(' '), arg),
              builder.CreateOr(
                  builder.CreateICmpEQ(data::const_char('\\'), arg),
                  builder.CreateICmpULT(data::const_char('\x7e'), arg))),
          exceptional_block, standard_block);

      builder.SetInsertPoint(standard_block);
      builder.CreateCall(cstdlib::printf(), {data::global_string("'%c'"), arg});
      builder.CreateRetVoid();

      builder.SetInsertPoint(exceptional_block);

      auto branch_default = make_block("default", repr_fn_);

      auto switch_stmt = builder.CreateSwitch(arg, branch_default);

      builder.SetInsertPoint(branch_default);
      builder.CreateCall(cstdlib::printf(),
                         {data::global_string("'\\x%02x'"), arg});
      builder.CreateRetVoid();

      add_branch(repr_fn_, switch_stmt, "tab", '\t', "'\\t'");
      add_branch(repr_fn_, switch_stmt, "newline", '\n', "'\\n'");
      add_branch(repr_fn_, switch_stmt, "carriage_return", '\r', "'\\r'");
      add_branch(repr_fn_, switch_stmt, "backslash", '\\', "'\\\\'");
    }

    builder.SetInsertPoint(prev_block);
    builder.CreateCall(repr_fn_, {val});

  } else if (this == Int) {
    builder.CreateCall(cstdlib::printf(), {data::global_string("%d"), val});

  } else if (this == Real) {
    builder.CreateCall(cstdlib::printf(), {data::global_string("%f"), val});

  } else if (this == Type_) {
    // NOTE: BE VERY CAREFUL HERE. YOU ARE TYPE PUNNING!
    auto type_val = reinterpret_cast<Type *>(val);

    builder.CreateCall(cstdlib::printf(),
                       {data::global_string("%s"),
                        data::global_string(type_val->to_string())});

  } else if (this == Uint) {
    builder.CreateCall(cstdlib::printf(), {data::global_string("%uu"), val});
  }
}

void Array::call_repr(llvm::Value *val) {
  auto prev_block = builder.GetInsertBlock();

  if (repr_fn_ == nullptr) {
    // TODO what about arrays of types?
    auto fn_type = Func(Ptr(this), Void);
    repr_fn_ = llvm::Function::Create(*fn_type, llvm::Function::ExternalLinkage,
                                      "repr." + Mangle(this), global_module);
    llvm::Value *arg = repr_fn_->args().begin();

    auto enter_block = make_block("enter", repr_fn_);
    builder.SetInsertPoint(enter_block);

    builder.CreateCall(cstdlib::putchar(), {data::const_char('[')});

    auto len_ptr =
        builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(0)});
    auto len_val = builder.CreateLoad(len_ptr);

    auto loop_block      = make_block("loop.body", repr_fn_);
    auto loop_head_block = make_block("loop.head", repr_fn_);
    auto done_block      = make_block("exit", repr_fn_);

    builder.CreateCondBr(builder.CreateICmpEQ(len_val, data::const_uint(0)),
                         done_block, loop_head_block);

    builder.SetInsertPoint(loop_head_block);

    // Start at position 1, not zero
    auto data_ptr_ptr = builder.CreateGEP(
        arg, {data::const_uint(0), data::const_uint(1)}, "data_ptr_ptr");
    llvm::Value *start_ptr = builder.CreateLoad(data_ptr_ptr, "start_ptr");
    auto end_ptr           = builder.CreateGEP(start_ptr, {len_val}, "end_ptr");

    // TODO make calls to call_repr not have to first check if we pass the
    // object or a pointer to the object.
    data_type->call_repr(PtrCallFix(builder, data_type, start_ptr));

    start_ptr =
        builder.CreateGEP(start_ptr, data::const_uint(1), "second_elem");
    builder.CreateCondBr(builder.CreateICmpEQ(start_ptr, end_ptr), done_block,
                         loop_block);

    // Otherwise, print ", element" repeatedly.
    builder.SetInsertPoint(loop_block);
    auto phi = builder.CreatePHI(*Ptr(data_type), 2, "loop_phi");
    phi->addIncoming(start_ptr, loop_head_block);

    builder.CreateCall(cstdlib::printf(), {data::global_string(", ")});

    // TODO make calls to call_repr not have to first check if we pass the
    // object or a pointer to the object.
    data_type->call_repr(PtrCallFix(builder, data_type, phi));

    auto next_ptr = builder.CreateGEP(phi, data::const_uint(1));
    builder.CreateCondBr(builder.CreateICmpULT(next_ptr, end_ptr), loop_block,
                         done_block);
    phi->addIncoming(next_ptr, loop_block);
    builder.SetInsertPoint(done_block);

    builder.CreateCall(cstdlib::putchar(), {data::const_char(']')});
    builder.CreateRetVoid();
  }

  builder.SetInsertPoint(prev_block);
  builder.CreateCall(repr_fn_, {val});
}

// NOTE: [function ...] probably looks too much like an array. That's why you
// used <function ...> in the first place.
void Function::call_repr(llvm::Value *val) {
  builder.CreateCall(cstdlib::printf(),
                     {data::global_string("%s"),
                      data::global_string("<function " + to_string() + ">")});
}

void ForwardDeclaration::call_repr(llvm::Value *val) {
  assert(false && "Cannot represent a forward declaration");
}

void DependentType::call_repr(llvm::Value *val) {
  builder.CreateCall(cstdlib::printf(),
                     {data::global_string("%s"),
                      data::global_string("<dependent " + to_string() + ">")});
}

void Pointer::call_repr(llvm::Value *val) {
  builder.CreateCall(cstdlib::printf(), {data::global_string("&_%x"), val});
}

void Enumeration::call_repr(llvm::Value *val) {
  // TODO print the enum's name as a string. This requires preallocating
  // an array of global strings and accesssing that.
  //
  // For now, just print the number in brackets after the enums name
  auto val_str = builder.CreateLoad(
      builder.CreateGEP(string_data, {data::const_uint(0), val}));
  builder.CreateCall(cstdlib::printf(),
                     {data::global_string(to_string() + ".%s"), val_str});
}

void Tuple::call_repr(llvm::Value *val) {}
void Structure::call_repr(llvm::Value *val) {}
void TypeVariable::call_repr(llvm::Value *val) {}
