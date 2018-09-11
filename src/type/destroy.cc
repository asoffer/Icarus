#include "ir/func.h"

#include "context.h"
#include "module.h"
#include "type/all.h"

namespace type {
void Primitive::EmitDestroy(IR::Register, Context *ctx) const {}

void Array::ComputeDestroyWithoutLock(Context *ctx) const {
  if (destroy_func_ != nullptr) { return; }
  destroy_func_ = ctx->mod_->AddFunc(
      type::Func({type::Ptr(this)}, {}),
      base::vector<std::pair<std::string, AST::Expression *>>{
          {"arg", nullptr}});

  CURRENT_FUNC(destroy_func_) {
    IR::BasicBlock::Current = destroy_func_->entry();
    auto arg                = destroy_func_->Argument(0);

    if (data_type->needs_destroy()) {
      IR::Register ptr = IR::Index(type::Ptr(this), arg, 0);
      auto end_ptr     = IR::PtrIncr(ptr,
                                 [&]() -> IR::RegisterOr<i32> {
                                   if (fixed_length) {
                                     return static_cast<i32>(len);
                                   } else {
                                     return IR::LoadInt(IR::ArrayLength(arg));
                                   }
                                 }(),
                                 type::Ptr(data_type));

      CreateLoop(
          {IR::Val::Reg(ptr, type::Ptr(data_type))},
          [&](const base::vector<IR::Val> &phis) {
            return IR::EqAddr(std::get<IR::Register>(phis[0].value), end_ptr);
          },
          [&](const base::vector<IR::Val> &phis) {
            data_type->EmitDestroy(std::get<IR::Register>(phis[0].value), ctx);
            return base::vector<IR::Val>{
                IR::Val::Reg(IR::PtrIncr(std::get<IR::Register>(phis[0].value),
                                         1, type::Ptr(data_type)),
                             type::Ptr(data_type))};
          });
    }

    if (!fixed_length) {
      IR::Free(IR::LoadAddr(IR::ArrayData(arg, type::Ptr(this)), data_type));
    }
    IR::ReturnJump();
  }
}

void Array::EmitDestroy(IR::Register reg, Context *ctx) const {
  if (!needs_destroy()) { return; }

  {
    std::unique_lock lock(mtx_);
    ComputeDestroyWithoutLock(ctx);
  }

  IR::LongArgs call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_->type_;
  IR::Call(IR::AnyFunc{destroy_func_}, std::move(call_args));
}

void Enum::EmitDestroy(IR::Register, Context *ctx) const {}
void Flags::EmitDestroy(IR::Register, Context *ctx) const {}
void CharBuffer::EmitDestroy(IR::Register, Context *ctx) const {}
void Function::EmitDestroy(IR::Register, Context *ctx) const {}
void Pointer::EmitDestroy(IR::Register, Context *ctx) const {}
void Variant::EmitDestroy(IR::Register, Context *ctx) const { NOT_YET(); }
void Scope::EmitDestroy(IR::Register, Context *ctx) const { UNREACHABLE(); }

void Struct::EmitDestroy(IR::Register reg, Context *ctx) const {
  {
    std::unique_lock lock(mtx_);
    if (destroy_func_ == nullptr) {
      destroy_func_ = ctx->mod_->AddFunc(
          type::Func({type::Ptr(this)}, {}),
          base::vector<std::pair<std::string, AST::Expression *>>{
              {"arg", nullptr}});

      CURRENT_FUNC(destroy_func_) {
        IR::BasicBlock::Current = destroy_func_->entry();
        for (size_t i = 0; i < fields_.size(); ++i) {
          fields_[i].type->EmitDestroy(
              IR::Field(destroy_func_->Argument(0), this, i), ctx);
        }
        IR::ReturnJump();
      }
    }
  }
  IR::LongArgs call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_->type_;
  IR::Call(IR::AnyFunc{destroy_func_}, std::move(call_args));
}
}  // namespace type
