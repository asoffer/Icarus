#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

extern std::vector<IR::Func *> implicit_functions;

extern llvm::Value *PtrCallFix(Type *t, llvm::Value *ptr);
extern IR::Value PtrCallFix(Type *t, IR::Value v);

extern llvm::Module *global_module;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);
namespace cstdlib {
extern llvm::Constant *putchar();
extern llvm::Constant *printf();
} // namespace cstdlib

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
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

static void AddSpecialCharacter(IR::Value val, char c, char vis,
                                IR::Block *land) {
  auto eq            = IR::CEQ(val, IR::Value(c));
  auto special_block = IR::Func::Current->AddBlock("special");
  auto next_block    = IR::Func::Current->AddBlock("next");

  IR::Block::Current->exit.SetConditional(eq, special_block, next_block);
  IR::Block::Current = special_block;
  IR::Print(IR::Value(Char), IR::Value('\\'));
  IR::Print(IR::Value(Char), IR::Value(vis));
  IR::Block::Current->exit.SetUnconditional(land);
  special_block->dump();
  IR::Block::Current = next_block;
}

void Primitive::EmitRepr(IR::Value val) {
  if (this == Bool) {
    IR::Print(IR::Value(Bool), val);
  } else if (this == Char) {
    // TODO pull this out into a function.
    IR::Print(IR::Value(Char), IR::Value('\''));

    auto land_block = IR::Func::Current->AddBlock("land");
    AddSpecialCharacter(val, '\a', 'a', land_block);
    AddSpecialCharacter(val, '\b', 'b', land_block);
    AddSpecialCharacter(val, '\n', 'n', land_block);
    AddSpecialCharacter(val, '\r', 'r', land_block);
    AddSpecialCharacter(val, '\t', 't', land_block);
    AddSpecialCharacter(val, '\v', 'v', land_block);

    IR::Print(IR::Value(Char), val);
    IR::Block::Current->exit.SetUnconditional(land_block);
    IR::Block::Current = land_block;
    IR::Print(IR::Value(Char), IR::Value('\''));

  } else if (this == Int) {
    IR::Print(IR::Value(Int), val);

  } else if (this == Uint) {
    IR::Print(IR::Value(Uint), val);
    IR::Print(IR::Value(Char), IR::Value('u'));

  } else if (this == Real) {
    IR::Print(IR::Value(Real), val);

  } else if (this == Type_) {
    NOT_YET;

  } else {
    NOT_YET;
  }
}

void Function::EmitRepr(IR::Value) {
  IR::Print(IR::Value(Char), IR::Value('{'));
  IR::Print(IR::Value(Type_), IR::Value(this));
  IR::Print(IR::Value(Char), IR::Value('}'));
}

void Enumeration::EmitRepr(IR::Value val) { NOT_YET; }
void Pointer::EmitRepr(IR::Value val) { NOT_YET; }

void Array::EmitRepr(IR::Value val) {
  if (!repr_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    repr_func          = new IR::Func(Func(this, Void));
    repr_func->name    = "repr." + Mangle(this);
    implicit_functions.push_back(repr_func);
    IR::Func::Current  = repr_func;
    IR::Block::Current = repr_func->entry();

    IR::Print(IR::Value(Char), IR::Value('['));
    if (fixed_length) {
      if (len >= 1) {
        data_type->EmitRepr(PtrCallFix(
            data_type, IR::Access(data_type, IR::Value(0ul), IR::Value::Arg(0))));
      } 
      // TODO at some length we want to loop so code-gen isn't too expensive

      for (size_t i = 1; i < len; ++i) {
        IR::Print(IR::Value(Char), IR::Value(','));
        IR::Print(IR::Value(Char), IR::Value(' '));
        data_type->EmitRepr(PtrCallFix(
            data_type, IR::Access(data_type, IR::Value(i), IR::Value::Arg(0))));
      }
    }
    IR::Print(IR::Value(Char), IR::Value(']'));

    IR::Block::Current->exit.SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->exit.SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(repr_func);

  IR::Call(Void, IR::Value(repr_func), {val});
}

void Primitive::call_repr(llvm::Value *val) {
  auto prev_block = builder.GetInsertBlock();

  if (this == Bool) {
    auto to_show = builder.CreateSelect(val, data::global_string("true"),
                                        data::global_string("false"));
    builder.CreateCall(cstdlib::printf(), {data::global_string("%s"), to_show});

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

    if (fixed_length) {
      if (len > 0) {
        auto start_ptr =
            builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(0)});
        data_type->call_repr(PtrCallFix(data_type, start_ptr));
      }

      for (size_t i = 1; i < len; ++i) {
        builder.CreateCall(cstdlib::printf(), {data::global_string(", ")});
        auto ptr =
            builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(i)});
        data_type->call_repr(PtrCallFix(data_type, ptr));
      }
    } else {

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
      auto end_ptr           = builder.CreateGEP(start_ptr, len_val, "end_ptr");

      // TODO make calls to call_repr not have to first check if we pass the
      // object or a pointer to the object.
      data_type->call_repr(PtrCallFix(data_type, start_ptr));

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
      data_type->call_repr(PtrCallFix(data_type, phi));

      auto next_ptr = builder.CreateGEP(phi, data::const_uint(1));
      builder.CreateCondBr(builder.CreateICmpULT(next_ptr, end_ptr), loop_block,
                           done_block);
      phi->addIncoming(next_ptr, loop_block);
      builder.SetInsertPoint(done_block);
    }
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

void Pointer::call_repr(llvm::Value *val) {
  builder.CreateCall(cstdlib::printf(), {data::global_string("&_%x"), val});
}

void Enumeration::call_repr(llvm::Value *val) {
  auto val_str = builder.CreateLoad(
      builder.CreateGEP(string_data, {data::const_uint(0), val}));
  builder.CreateCall(cstdlib::printf(),
                     {data::global_string(to_string() + ".%s"), val_str});
}

void Tuple::call_repr(llvm::Value *val) { NOT_YET; }
void Structure::call_repr(llvm::Value *val) { NOT_YET; }
void TypeVariable::call_repr(llvm::Value *val) { NOT_YET; }
void ParametricStructure::call_repr(llvm::Value *val) { NOT_YET; }
void RangeType::call_repr(llvm::Value *val) { NOT_YET; }
void SliceType::call_repr(llvm::Value *val) { NOT_YET; }

void Tuple::EmitRepr(IR::Value val) { NOT_YET; }
void Structure::EmitRepr(IR::Value val) { NOT_YET; }
void TypeVariable::EmitRepr(IR::Value val) { NOT_YET; }
void ParametricStructure::EmitRepr(IR::Value val) { NOT_YET; }
void RangeType::EmitRepr(IR::Value val) { NOT_YET; }
void SliceType::EmitRepr(IR::Value val) { NOT_YET; }
