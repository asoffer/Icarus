#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"

namespace type {
void Primitive::EmitDestroy(ir::Register, Context *ctx) const {}

void Array::ComputeDestroyWithoutLock(Context *ctx) const {
  if (destroy_func_ != nullptr) { return; }
  destroy_func_ = ctx->mod_->AddFunc(
      type::Func({type::Ptr(this)}, {}),
      base::vector<std::pair<std::string, ast::Expression *>>{
          {"arg", nullptr}});

  CURRENT_FUNC(destroy_func_) {
    ir::BasicBlock::Current = destroy_func_->entry();
    auto arg                = destroy_func_->Argument(0);

    if (data_type->needs_destroy()) {
      auto ptr     = ir::Index(type::Ptr(this), arg, 0);
      auto end_ptr = ir::PtrIncr(ptr, len, type::Ptr(data_type));

      using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitDestroy(std::get<0>(phis).reg_, ctx);
            return tup{
                ir::PtrIncr(std::get<0>(phis).reg_, 1, type::Ptr(data_type))};
          },
          std::tuple<type::Type const *>{type::Ptr(data_type)},
          tup{ptr});
    }

    ir::ReturnJump();
  }
}

void Array::EmitDestroy(ir::Register reg, Context *ctx) const {
  if (!needs_destroy()) { return; }

  {
    std::unique_lock lock(mtx_);
    ComputeDestroyWithoutLock(ctx);
  }

  ir::Arguments call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_->type_;
  ir::Call(ir::AnyFunc{destroy_func_}, std::move(call_args));
}

void Enum::EmitDestroy(ir::Register, Context *ctx) const {}
void Flags::EmitDestroy(ir::Register, Context *ctx) const {}
void Function::EmitDestroy(ir::Register, Context *ctx) const {}
void Pointer::EmitDestroy(ir::Register, Context *ctx) const {}
void Variant::EmitDestroy(ir::Register, Context *ctx) const { NOT_YET(); }

}  // namespace type
