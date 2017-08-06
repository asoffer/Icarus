#include "type.h"
#include "../ir/ir.h"
#include "../scope.h"

void Primitive::EmitDestroy(IR::Val) {}
void Pointer::EmitDestroy(IR::Val) {}
void Enum::EmitDestroy(IR::Val) {}
void Function::EmitDestroy(IR::Val) {}

extern IR::Val PtrCallFix(Type *t, IR::Val v);

void Array::EmitDestroy(IR::Val id_val) {
  if (!destroy_func) {
    destroy_func = new IR::Func(Func(Ptr(this), Void));
    destroy_func->name = "destroy." + Mangle(this);

    implicit_functions.push_back(destroy_func);
    CURRENT_FUNC(destroy_func) {
      IR::Block::Current = destroy_func->entry();

      IR::Val ptr = IR::Val::None();
      IR::Val length_var = IR::Val::None();
      if (fixed_length) {
        ptr        = IR::Index(IR::Val::Arg(this, 0), IR::Val::Uint(0));
        length_var = IR::Val::Uint(len);
      } else {
        ptr = IR::Load(IR::ArrayData(IR::Val::Arg(this, 0)));
        length_var = IR::Load(IR::ArrayLength(IR::Val::Arg(this, 0)));
      }
      auto end_ptr = IR::PtrIncr(ptr, length_var);

      auto loop_phi = IR::Func::Current->AddBlock();
      auto loop_body = IR::Func::Current->AddBlock();
      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi = IR::Phi(Ptr(data_type));
      IR::Jump::Conditional(IR::Eq(phi, end_ptr), IR::Func::Current->exit(),
                            loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitDestroy(phi);

      IR::Func::Current->SetArgs(phi.as_reg,
                                 {IR::Val::Block(destroy_func->entry()), ptr,
                                  IR::Val::Block(IR::Block::Current),
                                  IR::PtrIncr(phi, IR::Val::Uint(1))});

      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = IR::Func::Current->exit();
      if (!fixed_length) {
        IR::Free(IR::Load(IR::ArrayData(IR::Val::Arg(this, 0))));
      }
      IR::Jump::Return();
    }
  }
  IR::Call(IR::Val::Func(destroy_func), {id_val});
}

void Struct::EmitDestroy(IR::Val id_val) {
  if (!destroy_func) {
    destroy_func = new IR::Func(Func(Ptr(this), Void));
    destroy_func->name = "destroy." + Mangle(this);
    implicit_functions.push_back(destroy_func);

    CURRENT_FUNC(destroy_func) {
      IR::Block::Current = destroy_func->entry();
      for (size_t i = 0; i < field_type.size(); ++i) {
        field_type[i]->EmitDestroy(IR::Field(IR::Val::Arg(this, 0), i));
      }
      IR::Jump::Return();
    }
  }
  IR::Call(IR::Val::Func(destroy_func), {id_val});
}

void Tuple::EmitDestroy(IR::Val) { NOT_YET(); }
void RangeType::EmitDestroy(IR::Val) { UNREACHABLE(); }
void SliceType::EmitDestroy(IR::Val) { UNREACHABLE(); }
void Scope_Type::EmitDestroy(IR::Val) { UNREACHABLE(); }
