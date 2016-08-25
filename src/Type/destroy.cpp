#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

void Primitive::EmitDestroy(IR::Value id_val) {}
void Pointer::EmitDestroy(IR::Value id_val) {}
void Enum::EmitDestroy(IR::Value id_val) {}
void Function::EmitDestroy(IR::Value id_val) {}

void Array::EmitDestroy(IR::Value id_val) {
  if (!destroy_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    destroy_func = new IR::Func(Func(Ptr(this), Void));
    destroy_func->SetName("destroy." + Mangle(this));
    implicit_functions.push_back(destroy_func);

    IR::Func::Current  = destroy_func;
    IR::Block::Current = destroy_func->entry();

    IR::Value val, ptr;
    if (fixed_length) {
      val = IR::Value::Arg(0);
    } else {
      auto len_ptr = IR::ArrayLength(IR::Value::Arg(0));
      IR::Store(Uint, IR::Value::Uint(0ul), len_ptr);
      ptr = IR::ArrayData(this, IR::Value::Arg(0));
      IR::Store(Ptr(data_type), IR::Malloc(data_type, IR::Value::Uint(0ul)), ptr);
      val = IR::Load(Ptr(data_type), ptr);
    }

    for (size_t i = 0; i < len; ++i) {
      data_type->EmitDestroy(IR::Access(data_type, IR::Value::Uint(i), val));
    }

    if (!fixed_length) { IR::Free(IR::Load(Ptr(data_type), ptr)); }

    IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
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
