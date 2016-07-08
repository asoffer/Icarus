#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

extern llvm::Module* global_module;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

extern llvm::Value *PtrCallFix(Type *t, llvm::Value *ptr);

namespace cstdlib {
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace data {
extern llvm::Value *global_string(const std::string &s);
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

void Structure::EmitDefaultAssign(IR::Value to_var, IR::Value from_val) {
  if (!assign_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    assign_func        = new IR::Func(Func({Ptr(this), Ptr(this)}, Void));
    assign_func->name  = "assign." + Mangle(this);
    IR::Func::Current  = assign_func;
    IR::Block::Current = assign_func->entry();

    auto var = IR::Value::Arg(0);
    auto val = IR::Value::Arg(1);

    for (size_t i = 0; i < field_type.size(); ++i) {
      auto the_field_type = field_type AT(i);
      auto field_val = IR::Field(this, val, i);
      auto field_var = IR::Field(this, var, i);

      // TODO ptr call fix?
      if (!the_field_type->is_big()) {
        field_val = IR::Load(the_field_type, field_val);
      }

      Type::IR_CallAssignment(ast_expression->scope_, the_field_type,
                              the_field_type, field_val, field_var);
    }

    IR::Block::Current->exit.SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(assign_func);

  IR::Call(Void, IR::Value(assign_func), {to_var, from_val});
}

llvm::Function *Array::assign() {
  if (assign_fn_ != nullptr) return assign_fn_;

  auto save_block = builder.GetInsertBlock();

  assign_fn_ = llvm::Function::Create(
      *Func({Ptr(this), Uint, Ptr(data_type)}, Void),
      llvm::Function::ExternalLinkage, "assign." + Mangle(this), global_module);

  auto entry_block = make_block("entry", assign_fn_);
  builder.SetInsertPoint(entry_block);

  auto iter    = assign_fn_->args().begin();
  auto var     = iter;
  auto len     = ++iter;
  auto rhs_ptr = ++iter;
  len->setName("len");
  rhs_ptr->setName("rhs.ptr");
  var->setName("var");

  // release the resources held by var
  CallDestroy(nullptr, var);

  llvm::Value *to_head = nullptr;

  if (fixed_length) {
    to_head =
        builder.CreateGEP(var, {data::const_uint(0), data::const_uint(0)});
  } else {
    builder.CreateStore(len, builder.CreateGEP(var, {data::const_uint(0),
                                                     data::const_uint(0)}));

    auto bytes_to_alloc =
        builder.CreateMul(len, data::const_uint(data_type->bytes()));
    auto malloc_call = builder.CreateBitCast(
        builder.CreateCall(cstdlib::malloc(), {bytes_to_alloc}),
        *Ptr(data_type));
    builder.CreateStore(
        malloc_call,
        builder.CreateGEP(var, {data::const_uint(0), data::const_uint(1)}));
    to_head = malloc_call;
  }

  auto end_ptr = builder.CreateGEP(rhs_ptr, len);

  auto cond_block = make_block("cond", assign_fn_);
  auto loop_block = make_block("loop", assign_fn_);
  auto exit_block = make_block("exit", assign_fn_);

  builder.CreateBr(cond_block);
  builder.SetInsertPoint(cond_block);

  auto from_phi = builder.CreatePHI(*Ptr(data_type), 2, "from_phi");
  auto to_phi = builder.CreatePHI(*Ptr(data_type), 2, "to_phi");
  builder.CreateCondBr(builder.CreateICmpULT(from_phi, end_ptr), loop_block,
                       exit_block);

  // Write loop body
  builder.SetInsertPoint(loop_block);

  // This is ludicrous, but we have to init it here so that it can be
  // immeditaely uninitialized safely.
  data_type->call_init(to_phi);
  Type::CallAssignment(Scope::Global, data_type, data_type,
                       to_phi, PtrCallFix(data_type, from_phi));

  auto next_from_ptr = builder.CreateGEP(from_phi, data::const_uint(1));
  auto next_to_ptr   = builder.CreateGEP(to_phi, data::const_uint(1));

  from_phi->addIncoming(next_from_ptr, loop_block);
  to_phi->addIncoming(next_to_ptr, loop_block);

  from_phi->addIncoming(rhs_ptr, entry_block);
  to_phi->addIncoming(to_head, entry_block);

  builder.CreateBr(cond_block);

  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  builder.SetInsertPoint(save_block);

  return assign_fn_;
}

llvm::Function *Structure::assign() {
  if (assign_fn_ != nullptr) { return assign_fn_; }

  // TODO name mangling
  assign_fn_ = llvm::Function::Create(
      *Func({Ptr(this), Ptr(this)}, Void), llvm::Function::ExternalLinkage,
      "assign." + to_string(), global_module);

  auto save_block = builder.GetInsertBlock();

  auto block = make_block("entry", assign_fn_);
  builder.SetInsertPoint(block);

  auto iter = assign_fn_->args().begin();
  auto var  = iter;
  auto val  = ++iter;

  // assign all fields
  for (const auto &iter : field_num_to_llvm_num) {
    auto the_field_type = field_type AT(iter.first);
    auto field_val = builder.CreateGEP(
        val, {data::const_uint(0), data::const_uint(iter.second)});
    if (!the_field_type->is_big()) {
      field_val = builder.CreateLoad(*the_field_type, field_val);
    }
    auto field_var = builder.CreateGEP(
        var, {data::const_uint(0), data::const_uint(iter.second)});

    Type::CallAssignment(ast_expression->scope_, the_field_type, the_field_type,
                         field_var, field_val);
  }

  auto exit_block = make_block("exit", assign_fn_);
  builder.CreateBr(exit_block);
  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  builder.SetInsertPoint(save_block);
  return assign_fn_;
}
