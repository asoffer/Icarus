#include "ir/func.h"

#include "context.h"
#include "type/all.h"
#include "module.h"

namespace type {
void Primitive::EmitDestroy(IR::Val, Context *ctx) const {}

void Array::ComputeDestroyWithoutLock(Context *ctx) const {
  if (destroy_func_ != nullptr) { return; }
  destroy_func_ = ctx->mod_->AddFunc(
      Func({Ptr(this)}, {}),
      base::vector<std::pair<std::string, AST::Expression *>>{{"arg", nullptr}});

  CURRENT_FUNC(destroy_func_) {
    IR::BasicBlock::Current = destroy_func_->entry();
    auto arg                = destroy_func_->Argument(0);

    if (data_type->needs_destroy()) {
      IR::Register ptr =
          IR::Index(arg.type, std::get<IR::Register>(arg.value), 0);
      auto end_ptr = IR::PtrIncr(ptr,
                                 [&]() -> IR::RegisterOr<i32> {
                                   if (fixed_length) {
                                     return static_cast<i32>(len);
                                   } else {
                                     return IR::LoadInt(IR::ArrayLength(
                                         std::get<IR::Register>(arg.value)));
                                   }
                                 }(),
                                 type::Ptr(data_type));

      CreateLoop({IR::Val::Reg(ptr, type::Ptr(data_type))},
                 [&](const base::vector<IR::Val> &phis) {
                   return IR::ValFrom(IR::EqAddr(
                       std::get<IR::Register>(phis[0].value), end_ptr));
                 },
                 [&](const base::vector<IR::Val> &phis) {
                   data_type->EmitDestroy(phis[0], ctx);
                   return base::vector<IR::Val>{IR::Val::Reg(
                       IR::PtrIncr(std::get<IR::Register>(phis[0].value), 1,
                                   phis[0].type),
                       phis[0].type)};
                 });
    }

    if (!fixed_length) {
      IR::Free(
          IR::Load(IR::ArrayData(std::get<IR::Register>(arg.value), arg.type),
                   data_type));
    }
    IR::ReturnJump();
  }
}

void Array::EmitDestroy(IR::Val id_val, Context *ctx) const {
  if (!needs_destroy()) { return; }

  {
    std::unique_lock lock(mtx_);
    ComputeDestroyWithoutLock(ctx);
  }

  IR::LongArgs call_args;
  call_args.append(id_val);
  IR::Call(IR::Val::Func(destroy_func_), std::move(call_args));
}

void Enum::EmitDestroy(IR::Val, Context *ctx) const {}
void Flags::EmitDestroy(IR::Val, Context *ctx) const {}
void CharBuffer::EmitDestroy(IR::Val, Context *ctx) const {}
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
          base::vector<std::pair<std::string, AST::Expression *>>{
              {"arg", nullptr}});

      CURRENT_FUNC(destroy_func_) {
        IR::BasicBlock::Current = destroy_func_->entry();
        for (size_t i = 0; i < fields_.size(); ++i) {
          fields_[i].type->EmitDestroy(
              IR::Val::Reg(IR::Field(std::get<IR::Register>(
                                         destroy_func_->Argument(0).value),
                                     this, i),
                           type::Ptr(fields_.at(i).type)),
              ctx);
        }
        IR::ReturnJump();
      }
    }
  }
  IR::LongArgs call_args;
  call_args.append(id_val);
  IR::Call(IR::Val::Func(destroy_func_), std::move(call_args));
}
}
