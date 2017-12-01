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

    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(Ptr(this), Void), std::vector<std::string>{"arg"}));
    destroy_func       = IR::Func::All.back().get();
    destroy_func->name = "destroy." + Mangle(this);

    CURRENT_FUNC(destroy_func) {
      IR::Block::Current = destroy_func->entry();

      auto arg        = destroy_func->Argument(0);
      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      auto exit_block = IR::Func::Current->AddBlock();

      IR::Val ptr = IR::Index(arg, IR::Val::Uint(0));
      auto end_ptr =
          IR::PtrIncr(ptr, fixed_length ? IR::Val::Uint(len)
                                        : IR::Load(IR::ArrayLength(arg)));
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      IR::CondJump(IR::Eq(phi_reg, end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitDestroy(phi_reg);
      IR::UncondJump(loop_phi);

      destroy_func->SetArgs(phi, {IR::Val::Block(destroy_func->entry()), ptr,
                                  IR::Val::Block(loop_body),
                                  IR::PtrIncr(phi_reg, IR::Val::Uint(1))});

      IR::Block::Current = exit_block;
      if (!fixed_length) { IR::Free(IR::Load(IR::ArrayData(arg))); }
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(destroy_func), {id_val});
}

void Struct::EmitDestroy(IR::Val id_val) {
  if (!destroy_func) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(Ptr(this), Void), std::vector<std::string>{"arg"}));
    destroy_func       = IR::Func::All.back().get();
    destroy_func->name = "destroy." + Mangle(this);

    CURRENT_FUNC(destroy_func) {
      IR::Block::Current = destroy_func->entry();
      for (size_t i = 0; i < field_type.size(); ++i) {
        field_type[i]->EmitDestroy(IR::Field(destroy_func->Argument(0), i));
      }
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(destroy_func), {id_val});
}

void Tuple::EmitDestroy(IR::Val) { NOT_YET(); }
void RangeType::EmitDestroy(IR::Val) { UNREACHABLE(); }
void SliceType::EmitDestroy(IR::Val) { UNREACHABLE(); }
void Scope_Type::EmitDestroy(IR::Val) { UNREACHABLE(); }
