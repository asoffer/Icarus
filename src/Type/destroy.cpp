#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

void Primitive::EmitDestroy(IR::Value id_val) {}
void Pointer::EmitDestroy(IR::Value id_val) {}
void Enum::EmitDestroy(IR::Value id_val) {}
void Function::EmitDestroy(IR::Value id_val) {}

extern IR::Value PtrCallFix(Type *t, IR::Value v);

void Array::EmitDestroy(IR::Value id_val) {
  if (!destroy_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    destroy_func = new IR::Func(Func(Ptr(this), Void));
    destroy_func->SetName("destroy." + Mangle(this));

    implicit_functions.push_back(destroy_func);
    IR::Func::Current  = destroy_func;
    IR::Block::Current = destroy_func->entry();

    IR::Value ptr, length_var;
    if (fixed_length) {
      ptr        = IR::Access(data_type, IR::Value::Uint(0), IR::Value::Arg(0));
      length_var = IR::Value::Uint(len);
    } else {
      ptr        = IR::Load(Ptr(data_type), IR::ArrayData(this, IR::Value::Arg(0)));
      length_var = IR::Load(Uint, IR::ArrayLength(IR::Value::Arg(0)));
    }
    auto end_ptr = IR::PtrIncr(Ptr(data_type), ptr, length_var);

    auto loop_phi  = IR::Func::Current->AddBlock("loop-phi");
    auto loop_cond = IR::Func::Current->AddBlock("loop-cond");
    auto loop_body = IR::Func::Current->AddBlock("loop-body");

    auto init_block = IR::Block::Current;
    IR::Block::Current->SetUnconditional(loop_phi);
    IR::Block::Current = loop_phi;

    auto phi = IR::Phi(Ptr(data_type));
    phi.args.emplace_back(init_block);
    phi.args.emplace_back(ptr);

    auto phi_reg = IR::Value::Reg(phi.result.reg);

    loop_phi->SetUnconditional(loop_cond);
    IR::Block::Current = loop_cond;

    auto cond = IR::EQ(Ptr(data_type), phi_reg, end_ptr);
    IR::Block::Current->SetConditional(cond, IR::Func::Current->exit(),
                                       loop_body);
    IR::Block::Current = loop_body;
    data_type->EmitDestroy(phi_reg);

    auto elem_ptr = IR::Increment(Ptr(data_type), phi_reg);

    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(elem_ptr);

    IR::Block::Current->SetUnconditional(loop_phi);
    loop_phi->push(phi);

    IR::Block::Current = IR::Func::Current->exit();
    if (!fixed_length) {
      IR::Free(
          IR::Load(Ptr(data_type), IR::ArrayData(this, IR::Value::Arg(0))));
    }
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(destroy_func);

  IR::Call(Void, IR::Value::Func(destroy_func), {id_val});
}

void Struct::EmitDestroy(IR::Value id_val) {
  if (!destroy_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    destroy_func = new IR::Func(Func(Ptr(this), Void));
    destroy_func->SetName("destroy." + Mangle(this));
    implicit_functions.push_back(destroy_func);

    IR::Func::Current  = destroy_func;
    IR::Block::Current = destroy_func->entry();

    for (size_t i = 0; i < field_type.size(); ++i) {
      field_type[i]->EmitDestroy(IR::Field(this, IR::Value::Arg(0), i));
    }

    IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(destroy_func);

  IR::Call(Void, IR::Value::Func(destroy_func), {id_val});
}

void Tuple::EmitDestroy(IR::Value id_val) { NOT_YET; }
void RangeType::EmitDestroy(IR::Value id_val) { UNREACHABLE; }
void SliceType::EmitDestroy(IR::Value id_val) { UNREACHABLE; }
void TypeVariable::EmitDestroy(IR::Value id_val) { UNREACHABLE; }
void Scope_Type::EmitDestroy(IR::Value id_val) { UNREACHABLE; }
