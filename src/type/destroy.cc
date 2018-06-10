#include "../ir/func.h"

#include "context.h"
#include "all.h"
#include "module.h"

namespace type {
void Primitive::EmitDestroy(IR::Val, Context *ctx) const {}

extern IR::Val PtrCallFix(Type *t, IR::Val v);

void Array::ComputeDestroyWithoutLock(Context *ctx) const {
  if (destroy_func_ != nullptr) { return; }
  destroy_func_ = ctx->mod_->AddFunc(
      Func({Ptr(this)}, {}),
      std::vector<std::pair<std::string, AST::Expression *>>{{"arg", nullptr}});

  CURRENT_FUNC(destroy_func_) {
    IR::BasicBlock::Current = destroy_func_->entry();
    auto arg                = destroy_func_->Argument(0);

    if (data_type->needs_destroy()) {
      IR::Val ptr = IR::Index(arg, IR::Val::Int(0));
      auto end_ptr =
          IR::PtrIncr(ptr, fixed_length ? IR::Val::Int(static_cast<i32>(len))
                                        : IR::Load(IR::ArrayLength(arg)));

      CreateLoop({ptr},
                 [&](const std::vector<IR::Val> &phis) {
                   return IR::Eq(phis[0], end_ptr);
                 },
                 [&](const std::vector<IR::Val> &phis) {
                   data_type->EmitDestroy(phis[0], ctx);
                   return std::vector{IR::PtrIncr(phis[0], IR::Val::Int(1))};
                 });
    }

    if (!fixed_length) { IR::Free(IR::Load(IR::ArrayData(arg))); }
    IR::ReturnJump();
  }
}

void Array::EmitDestroy(IR::Val id_val, Context *ctx) const {
  if (!needs_destroy()) { return; }

  {
    std::unique_lock lock(mtx_);
    ComputeDestroyWithoutLock(ctx);
  }
  IR::Call(IR::Val::Func(destroy_func_), {id_val}, {});
}

void Enum::EmitDestroy(IR::Val, Context *ctx) const {}
void Flags::EmitDestroy(IR::Val, Context *ctx) const {}
void Function::EmitDestroy(IR::Val, Context *ctx) const {}
void Pointer::EmitDestroy(IR::Val, Context *ctx) const {}
void Variant::EmitDestroy(IR::Val, Context *ctx) const { NOT_YET(); }
void Scope::EmitDestroy(IR::Val, Context *ctx) const { UNREACHABLE(); }

void Struct::EmitDestroy(IR::Val id_val, Context *ctx) const {
  {
    std::unique_lock lock(mtx_);
    if (destroy_func_ == nullptr) {
      destroy_func_ = ctx->mod_->AddFunc(
          Func({Ptr(this)}, {}),
          std::vector<std::pair<std::string, AST::Expression *>>{
              {"arg", nullptr}});

      CURRENT_FUNC(destroy_func_) {
        IR::BasicBlock::Current = destroy_func_->entry();
        for (size_t i = 0; i < fields_.size(); ++i) {
          fields_[i].type->EmitDestroy(IR::Field(destroy_func_->Argument(0), i),
                                       ctx);
        }
        IR::ReturnJump();
      }
    }
  }
  IR::Call(IR::Val::Func(destroy_func_), {id_val}, {});
}
}
