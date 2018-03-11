#include "../ir/func.h"
#include "type.h"

void Primitive::EmitDestroy(IR::Val) const {}
void Pointer::EmitDestroy(IR::Val) const {}
void Enum::EmitDestroy(IR::Val) const {}
void Function::EmitDestroy(IR::Val) const {}
void Variant::EmitDestroy(IR::Val) const { NOT_YET(); }

extern IR::Val PtrCallFix(Type *t, IR::Val v);

void Array::EmitDestroy(IR::Val id_val) const {
  if (destroy_func_ == nullptr) {
    if (!needs_destroy()) { return; }

    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(Ptr(this), Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}}));
    destroy_func_       = IR::Func::All.back().get();
    destroy_func_->name = "destroy(" + this->to_string() + ")";

    CURRENT_FUNC(destroy_func_) {
      IR::Block::Current = destroy_func_->entry();

      auto arg        = destroy_func_->Argument(0);
      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      auto exit_block = IR::Func::Current->AddBlock();

      IR::Val ptr = IR::Index(arg, IR::Val::Int(0));
      auto end_ptr =
          IR::PtrIncr(ptr, fixed_length ? IR::Val::Int(static_cast<i32>(len))
                                        : IR::Load(IR::ArrayLength(arg)));
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      IR::CondJump(IR::Eq(phi_reg, end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitDestroy(phi_reg);
      IR::UncondJump(loop_phi);

      destroy_func_->SetArgs(phi, {IR::Val::Block(destroy_func_->entry()), ptr,
                                  IR::Val::Block(loop_body),
                                  IR::PtrIncr(phi_reg, IR::Val::Int(1))});

      IR::Block::Current = exit_block;
      if (!fixed_length) { IR::Free(IR::Load(IR::ArrayData(arg))); }
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(destroy_func_), {id_val}, {});
}

void Struct::EmitDestroy(IR::Val id_val) const {
  if (destroy_func_ == nullptr) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(Ptr(this), Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}}));
    destroy_func_       = IR::Func::All.back().get();
    destroy_func_->name = "destroy(" + this->to_string() + ")";

    CURRENT_FUNC(destroy_func_) {
      IR::Block::Current = destroy_func_->entry();
      for (size_t i = 0; i < fields_.size(); ++i) {
        fields_[i].type->EmitDestroy(IR::Field(destroy_func_->Argument(0), i));
      }
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(destroy_func_), {id_val}, {});
}

void Tuple::EmitDestroy(IR::Val) const { NOT_YET(); }
void RangeType::EmitDestroy(IR::Val) const { UNREACHABLE(); }
void SliceType::EmitDestroy(IR::Val) const { UNREACHABLE(); }
void Scope_Type::EmitDestroy(IR::Val) const { UNREACHABLE(); }
