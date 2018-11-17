#include "type/array.h"

#include "architecture.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, ir::Func *>>>
    eq_funcs;
static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, ir::Func *>>>
    ne_funcs;
ir::Val Array::Compare(const Array *lhs_type, ir::Val lhs_ir,
                       const Array *rhs_type, ir::Val rhs_ir, bool equality,
                       Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    base::vector<std::pair<std::string, ast::Expression *>> args = {
        {"lhs", nullptr}, {"rhs", nullptr}};
    auto *fn = ctx->mod_->AddFunc(
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool}),
        std::move(args));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto lhs_len = [&]() -> ir::RegisterOr<i32> {
        if (lhs_type->fixed_length) { return static_cast<i32>(lhs_type->len); }
        return ir::Load<i32>(ir::ArrayLength(fn->Argument(0)));
      }();

      auto rhs_len = [&]() -> ir::RegisterOr<i32> {
        if (rhs_type->fixed_length) { return static_cast<i32>(rhs_type->len); }
        return ir::Load<i32>(ir::ArrayLength(fn->Argument(1)));
      }();

      auto equal_len_block = ir::Func::Current->AddBlock();
      auto true_block      = ir::Func::Current->AddBlock();
      auto false_block     = ir::Func::Current->AddBlock();
      auto phi_block       = ir::Func::Current->AddBlock();
      auto body_block      = ir::Func::Current->AddBlock();
      auto incr_block      = ir::Func::Current->AddBlock();

      ir::CondJump(ir::Eq(lhs_len, rhs_len), equal_len_block, false_block);

      ir::BasicBlock::Current = true_block;
      ir::SetRet(0, true);
      ir::ReturnJump();

      ir::BasicBlock::Current = false_block;
      ir::SetRet(0, false);
      ir::ReturnJump();

      ir::BasicBlock::Current = equal_len_block;
      auto lhs_start = ir::Index(type::Ptr(lhs_type), fn->Argument(0), 0);
      auto rhs_start = ir::Index(type::Ptr(rhs_type), fn->Argument(1), 0);
      auto lhs_end =
          ir::PtrIncr(lhs_start, lhs_len, type::Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::BasicBlock::Current = phi_block;
      auto lhs_phi_index      = ir::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi_index      = ir::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg = ir::Func::Current->Command(lhs_phi_index).result;
      auto rhs_phi_reg = ir::Func::Current->Command(rhs_phi_index).result;

      ir::CondJump(ir::Eq(ir::RegisterOr<ir::Addr>(lhs_phi_reg), lhs_end),
                   true_block, body_block);

      ir::BasicBlock::Current = body_block;
      // TODO what if data type is an array?
      ir::CondJump(ir::Eq(ir::Load<ir::Addr>(lhs_phi_reg, lhs_type->data_type),
                          ir::Load<ir::Addr>(rhs_phi_reg, rhs_type->data_type)),
                   incr_block, false_block);

      ir::BasicBlock::Current = incr_block;
      auto lhs_incr =
          ir::PtrIncr(lhs_phi_reg, 1, type::Ptr(lhs_type->data_type));
      auto rhs_incr =
          ir::PtrIncr(rhs_phi_reg, 1, type::Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::MakePhi<ir::Addr>(lhs_phi_index, {{equal_len_block, lhs_start},
                                            {incr_block, lhs_incr}});
      ir::MakePhi<ir::Addr>(rhs_phi_index, {{equal_len_block, rhs_start},
                                            {incr_block, rhs_incr}});
    }
  }

  ir::LongArgs call_args;
  call_args.append(lhs_ir);
  call_args.append(rhs_ir);
  call_args.type_ = iter->second->type_;

  ir::OutParams outs;
  auto result = outs.AppendReg(type::Bool);

  ir::Call(ir::AnyFunc{iter->second}, std::move(call_args), std::move(outs));
  return {ir::Val::Reg(result, type::Bool)};
}

static ir::RegisterOr<i32> ComputeMin(ir::RegisterOr<i32> x,
                                      ir::RegisterOr<i32> y) {
  auto entry_block = ir::BasicBlock::Current;
  auto x_block     = ir::Func::Current->AddBlock();
  auto land_block  = ir::Func::Current->AddBlock();
  ir::CondJump(ir::Lt(x, y), x_block, land_block);
  ir::BasicBlock::Current = x_block;
  ir::UncondJump(land_block);

  ir::BasicBlock::Current = land_block;
  return ir::MakePhi<i32>(ir::Phi(type::Int32),
                          {{x_block, x}, {entry_block, y}});
}

// TODO resize shoud probably take a custom allocator
void Array::EmitResize(ir::Val ptr_to_array, ir::Val new_size,
                       Context *ctx) const {
  // TODO these could maybe be moved into separate structs, or templated
  ASSERT(!fixed_length);
  {
    std::unique_lock lock(mtx_);
    if (resize_func_ != nullptr) { goto call_fn; }

    resize_func_ = ctx->mod_->AddFunc(
        type::Func({type::Ptr(this), type::Int64}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"arg", nullptr}, {"new_size", nullptr}});

    CURRENT_FUNC(resize_func_) {
      ir::BasicBlock::Current = resize_func_->entry();
      auto arg                = resize_func_->Argument(0);
      auto size_arg = ir::TypedRegister<i32>(resize_func_->Argument(1));

      auto new_arr = ir::Malloc(
          data_type,
          ir::Mul(size_arg,
                  static_cast<i32>(
                      Architecture::InterprettingMachine().bytes(data_type))));

      auto *ptr_data_type   = type::Ptr(data_type);
      auto from_ptr         = ir::Index(type::Ptr(this), arg, 0);
      ir::RegisterOr<i32> min_val =
          ComputeMin(ir::Load<i32>(ir::ArrayLength(arg)), size_arg);
      auto end_ptr = ir::PtrIncr(from_ptr, min_val, ptr_data_type);

      using tup2 =
          std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
      auto finish_phis = ir::CreateLoop(
          [&](tup2 const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup2 const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            ASSERT(std::get<1>(phis).is_reg_);
            data_type->EmitAssign(
                data_type,
                ir::Val::Reg(ir::PtrFix(std::get<0>(phis).reg_, data_type),
                             data_type),
                std::get<1>(phis).reg_, ctx);
            data_type->EmitDestroy(std::get<0>(phis).reg_, ctx);
            return tup2{ir::PtrIncr(std::get<0>(phis).reg_, 1, ptr_data_type),
                        ir::PtrIncr(std::get<1>(phis).reg_, 1, ptr_data_type)};
          },
          std::tuple<type::Type const *, type::Type const *>{ptr_data_type,
                                                             ptr_data_type},
          tup2{from_ptr, new_arr});

      if (data_type->needs_destroy()) {
        auto end_old_buf =
            ir::PtrIncr(ir::ArrayData(arg, type::Ptr(this)),
                        ir::Load<i32>(ir::ArrayLength(arg)), type::Ptr(this));

        using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
        ir::CreateLoop(
            [&](tup const &phis) {
              return ir::Eq(std::get<0>(phis), end_old_buf);
            },
            [&](tup const &phis) {
              ASSERT(std::get<0>(phis).is_reg_);
              data_type->EmitDestroy(std::get<0>(phis).reg_, ctx);
              return tup{ir::PtrIncr(std::get<0>(phis).reg_, 1, ptr_data_type)};
            },
            std::tuple<type::Type const *>{ptr_data_type}, tup{end_ptr});
      }

      auto end_to_ptr = ir::PtrIncr(new_arr, size_arg, ptr_data_type);
      using tup       = std::tuple<ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) {
            return ir::Eq(std::get<0>(phis), end_to_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitInit(std::get<0>(phis).reg_, ctx);
            return tup{ir::PtrIncr(std::get<0>(phis).reg_, 1, ptr_data_type)};
          },
          std::tuple<type::Type const *>{ptr_data_type},
          tup{std::get<1>(finish_phis)});

      auto old_buf = ir::ArrayData(arg, type::Ptr(this));
      ir::Store(size_arg, ir::ArrayLength(arg));
      ir::Free(ir::Load<ir::Addr>(old_buf, data_type));
      ir::Store(new_arr, old_buf);
      ir::ReturnJump();
    }
  }

call_fn:
  ASSERT(ir::Func::Current != nullptr);
  ir::LongArgs call_args;
  call_args.append(ptr_to_array);
  call_args.append(new_size);
  call_args.type_ = ASSERT_NOT_NULL(resize_func_)->type_;
  ir::Call(ir::AnyFunc{resize_func_}, std::move(call_args));
}
}  // namespace type
