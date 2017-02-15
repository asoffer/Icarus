#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

void Primitive::EmitInit(IR::Value id_val) {
  IR::Store(this, EmitInitialValue(), id_val);
}
void Enum::EmitInit(IR::Value id_val) {
  IR::Store(ProxyType(), EmitInitialValue(), id_val);
}
void Function::EmitInit(IR::Value id_val) {
  IR::Store(this, EmitInitialValue(), id_val);
}

void Array::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->SetName("init." + Mangle(this));

    implicit_functions.push_back(init_func);
    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    IR::Value ptr, length_var;
    if (fixed_length) {
      ptr        = IR::Access(data_type, IR::Value::Uint(0), IR::Value::Arg(0));
      length_var = IR::Value::Uint(len);
    } else {
      auto ptr_to_data_cell = IR::ArrayData(this, IR::Value::Arg(0));
      IR::Store(Ptr(data_type), IR::Malloc(data_type, IR::Value::Uint(0ul)),
                ptr_to_data_cell);

      ptr        = IR::Load(Ptr(data_type), ptr_to_data_cell);
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

    auto cond = IR::PtrEQ(phi_reg, end_ptr);
    IR::Block::Current->SetConditional(cond, IR::Func::Current->exit(),
                                       loop_body);
    IR::Block::Current = loop_body;

    data_type->EmitInit(phi_reg);

    auto elem_ptr = IR::Increment(Ptr(data_type), phi_reg);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(elem_ptr);

    IR::Block::Current->SetUnconditional(loop_phi);
    loop_phi->push(phi);

    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  IR::Call(Void, IR::Value::Func(init_func), {id_val});
}

void Pointer::EmitInit(IR::Value id_val) {
  IR::Store(this, IR::Value::Null(this), id_val);
}

void Struct::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->SetName("init." + Mangle(this));
    implicit_functions.push_back(init_func);

    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    // TODO init expressions?

    for (size_t i = 0; i < field_type.size(); ++i) {
      if (init_values[i]) {
        if (init_values[i]->is_hole()) { continue; }
        Type::CallAssignment(init_values[i]->scope_, field_type[i],
                             init_values[i]->type, init_values[i]->EmitIR(),
                             IR::Field(this, IR::Value::Arg(0), i));
      } else {
        field_type[i]->EmitInit(IR::Field(this, IR::Value::Arg(0), i));
      }
    }

    IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  IR::Call(Void, IR::Value::Func(init_func), {id_val});
}

void Tuple::EmitInit(IR::Value id_val) { NOT_YET; }
void RangeType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void SliceType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void TypeVariable::EmitInit(IR::Value id_val) { UNREACHABLE; }
void Scope_Type::EmitInit(IR::Value id_val) { UNREACHABLE; }
