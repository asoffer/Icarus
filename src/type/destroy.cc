#include "../ir/ir.h"
#include "../scope.h"
#include "type.h"

void Primitive::EmitDestroy(IR::Val) {}
void Pointer::EmitDestroy(IR::Val) {}
void Enum::EmitDestroy(IR::Val) {}
void Function::EmitDestroy(IR::Val) {}

extern IR::Val PtrCallFix(Type *t, IR::Val v);

void Array::EmitDestroy(IR::Val id_val) {
  if (destroy_func == nullptr) {
    if (!needs_destroy()) { return; }

    destroy_func       = new IR::Func(Func(Ptr(this), Void));
    destroy_func->name = "destroy." + Mangle(this);

    CURRENT_FUNC(destroy_func) {
      IR::Block::Current = destroy_func->entry();

      auto arg       = IR::Val::Arg(Ptr(this), 0);
      auto loop_phi  = IR::Func::Current->AddBlock();
      auto loop_body = IR::Func::Current->AddBlock();

      IR::Val ptr = IR::Index(arg, IR::Val::Uint(0));
      auto end_ptr =
          IR::PtrIncr(ptr, fixed_length ? IR::Val::Uint(len)
                                        : IR::Load(IR::ArrayLength(arg)));
      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      IR::Jump::Conditional(IR::Eq(phi, end_ptr), destroy_func->exit(),
                            loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitDestroy(phi);
      IR::Jump::Unconditional(loop_phi);

      destroy_func->SetArgs(phi.value.as<IR::Register>(),
                            {IR::Val::Block(destroy_func->entry()), ptr,
                             IR::Val::Block(loop_body),
                             IR::PtrIncr(phi, IR::Val::Uint(1))});

      IR::Block::Current = destroy_func->exit();
      if (!fixed_length) { IR::Free(IR::Load(IR::ArrayData(arg))); }

      IR::Jump::Return();
    }
  }
  IR::Call(IR::Val::Func(destroy_func), {id_val});
}

void Struct::EmitDestroy(IR::Val id_val) {
  if (!destroy_func) {
    destroy_func       = new IR::Func(Func(Ptr(this), Void));
    destroy_func->name = "destroy." + Mangle(this);

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
