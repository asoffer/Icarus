#include "type.h"

#include "../ast/ast.h"
#include "../ir/ir.h"
#include "scope.h"

void Primitive::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}
void Enum::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}
void Function::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}

void Array::EmitInit(IR::Val id_val) {
  if (!init_func) {
    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->name = "init." + Mangle(this);
    implicit_functions.push_back(init_func);
    CURRENT_FUNC(init_func) {
      auto ptr = IR::Val::None();
      auto length_var = IR::Val::None();
      if (fixed_length) {
        ptr = IR::Access(IR::Val::Uint(0), IR::Val::Arg(this, 0));
        length_var = IR::Val::Uint(len);
      } else {
        auto ptr_to_data_cell = IR::ArrayData(IR::Val::Arg(this, 0));
        IR::Store(IR::Malloc(data_type, IR::Val::Uint(0ul)), ptr_to_data_cell);

        ptr = IR::Load(ptr_to_data_cell);
        length_var = IR::Load(IR::ArrayLength(IR::Val::Arg(this, 0)));
      }
      auto end_ptr = IR::PtrIncr(ptr, length_var);

      auto loop_phi = IR::Func::Current->AddBlock();
      auto loop_cond = IR::Func::Current->AddBlock();
      auto loop_body = IR::Func::Current->AddBlock();

      auto init_block = IR::Block::Current;
      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = loop_phi;
      IR::Jump::Unconditional(loop_cond);

      auto phi = IR::Phi(Ptr(data_type));

      IR::Block::Current = loop_cond;
      IR::Jump::Conditional(IR::Eq(phi, end_ptr), IR::Func::Current->exit(),
                            loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitInit(phi);

      auto elem_ptr = IR::PtrIncr(phi, IR::Val::Uint(1));

      // TODO FIXME XXX THIS IS HACKY!
      IR::Func::Current->blocks_[phi.as_reg.block_index.value]
          .cmds_[phi.as_reg.instr_index]
          .args = {IR::Val::Block(init_block), ptr,
                   IR::Val::Block(IR::Block::Current), elem_ptr};
      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = IR::Func::Current->exit();
      IR::Jump::Return();
    }
  }

  IR::Call(IR::Val::Func(init_func), std::vector<IR::Val>{id_val});
}

void Pointer::EmitInit(IR::Val id_val) {
  IR::Store(IR::Val::Null(this), id_val);
}

void Struct::EmitInit(IR::Val id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->name = "init." + Mangle(this);
    implicit_functions.push_back(init_func);

    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    // TODO init expressions?

    for (size_t i = 0; i < field_type.size(); ++i) {
      if (init_values[i]) {
        if (init_values[i]->is_hole()) { continue; }
        std::vector<Error> errors;
        Type::CallAssignment(init_values[i]->scope_, field_type[i],
                             init_values[i]->type,
                             init_values[i]->EmitIR(&errors),
                             IR::Field(IR::Val::Arg(this, 0), i));
      } else {
        field_type[i]->EmitInit(IR::Field(IR::Val::Arg(this, 0), i));
      }
    }

    IR::Jump::Unconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Jump::Return();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }

  IR::Call(IR::Val::Func(init_func), {id_val});
}

void Tuple::EmitInit(IR::Val) { NOT_YET; }
void RangeType::EmitInit(IR::Val) { UNREACHABLE; }
void SliceType::EmitInit(IR::Val) { UNREACHABLE; }
void TypeVariable::EmitInit(IR::Val) { UNREACHABLE; }
void Scope_Type::EmitInit(IR::Val) { UNREACHABLE; }
