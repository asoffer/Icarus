#include "type/array.h"

#include "architecture.h"
#include "ast/fn_params.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

template <typename LoopPhiFn, typename LoopBodyFn, typename TypeTup,
          typename... Ts>
static void CreateLoop(LoopPhiFn &&loop_phi_fn, LoopBodyFn &&loop_body_fn,
                       TypeTup &&types,
                       std::tuple<ir::RegisterOr<Ts>...> entry_vals) {
  auto entry_block = ir::BasicBlock::Current;

  auto loop_phi   = ir::Func::Current->AddBlock();
  auto loop_body  = ir::Func::Current->AddBlock();
  auto exit_block = ir::Func::Current->AddBlock();

  ir::UncondJump(loop_phi);
  ir::BasicBlock::Current = loop_phi;

  auto phi_indices = base::tuple::transform(ir::Phi, types);
  auto phi_vals    = base::tuple::transform(
      [](auto &&val) { return ir::Func::Current->Command(val).result; },
      phi_indices);

  auto exit_cond = std::forward<LoopPhiFn>(loop_phi_fn)(phi_vals);
  ir::CondJump(exit_cond, exit_block, loop_body);

  ir::BasicBlock::Current = loop_body;
  auto new_phis           = std::forward<LoopBodyFn>(loop_body_fn)(phi_vals);
  ir::UncondJump(loop_phi);

  base::tuple::for_each(
      [&](auto &&phi_index, auto &&entry_val, auto &&new_phi) {
        using T = std::decay_t<decltype(entry_val.val_)>;
        ir::MakePhi<T>(phi_index,
                       std::unordered_map<ir::BlockIndex, ir::RegisterOr<T>>{
                           std::pair<ir::BlockIndex, ir::RegisterOr<T>>{
                               entry_block, entry_val},
                           std::pair<ir::BlockIndex, ir::RegisterOr<T>>{
                               loop_body, new_phi}});
      },
      std::move(phi_indices), std::move(entry_vals), std::move(new_phis));

  ir::BasicBlock::Current = exit_block;
}

namespace type {
using base::check::Is;

static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, ir::Func *>>>
    eq_funcs;
static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, ir::Func *>>>
    ne_funcs;
// TODO this should early exit if the types aren't equal.
ir::Val Array::Compare(const Array *lhs_type, ir::Val lhs_ir,
                       const Array *rhs_type, ir::Val rhs_ir, bool equality,
                       Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto *fn = ctx->mod_->AddFunc(
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool}),
        ast::FnParams<ast::Expression *>(2));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto equal_len_block = ir::Func::Current->AddBlock();
      auto true_block      = ir::Func::Current->AddBlock();
      auto false_block     = ir::Func::Current->AddBlock();
      auto phi_block       = ir::Func::Current->AddBlock();
      auto body_block      = ir::Func::Current->AddBlock();
      auto incr_block      = ir::Func::Current->AddBlock();

      ir::CondJump(ir::Eq(lhs_type->len, rhs_type->len), equal_len_block,
                   false_block);

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
          ir::PtrIncr(lhs_start, lhs_type->len, type::Ptr(rhs_type->data_type));
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

  ir::Arguments call_args;
  call_args.append(lhs_ir);
  call_args.append(rhs_ir);
  call_args.type_ = iter->second->type_;

  ir::OutParams outs;
  auto result = outs.AppendReg(type::Bool);

  ir::Call(ir::AnyFunc{iter->second}, std::move(call_args), std::move(outs));
  return {ir::Val::Reg(result, type::Bool)};
}

static base::guarded<
    base::unordered_map<Type const *, base::unordered_map<size_t, Array>>>
    fixed_arrays_;
const Array *Arr(Type const *t, size_t len) {
  auto handle = fixed_arrays_.lock();
  return &(*handle)[t]
              .emplace(std::piecewise_construct, std::forward_as_tuple(len),
                       std::forward_as_tuple(t, len))
              .first->second;
}

void Array::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  data_type->defining_modules(modules);
}

template <SpecialFunctionCategory Cat>
static ir::AnyFunc CreateAssign(Array const *a, Context *ctx) {
  Pointer const *ptr_type = Ptr(a);
  auto *data_ptr_type     = Ptr(a->data_type);
  ir::AnyFunc fn          = ctx->mod_->AddFunc(Func({ptr_type, ptr_type}, {}),
                                      ast::FnParams<ast::Expression *>(2));
  CURRENT_FUNC(fn.func()) {
    ir::BasicBlock::Current = fn.func()->entry();
    auto val                = fn.func()->Argument(0);
    auto var                = fn.func()->Argument(1);

    auto from_ptr     = ir::Index(ptr_type, val, 0);
    auto from_end_ptr = ir::PtrIncr(from_ptr, a->len, data_ptr_type);
    auto to_ptr       = ir::Index(ptr_type, var, 0);

    using tup = std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
    CreateLoop(
        [&](tup const &phis) {
          return ir::Eq(std::get<0>(phis), from_end_ptr);
        },
        [&](tup const &phis) {
          ASSERT(std::get<0>(phis).is_reg_);
          ASSERT(std::get<1>(phis).is_reg_);

          auto from_val = ir::Val::Reg(
              PtrFix(std::get<0>(phis).reg_, a->data_type),
              a->data_type->is_big() ? a->data_type : data_ptr_type);

          if constexpr (Cat == Copy) {
            a->data_type->EmitCopyAssign(a->data_type, from_val,
                                         std::get<1>(phis).reg_, ctx);
          } else if constexpr (Cat == Move) {
            a->data_type->EmitMoveAssign(a->data_type, from_val,
                                         std::get<1>(phis).reg_, ctx);
          } else {
            UNREACHABLE();
          }

          return std::tuple{
              ir::PtrIncr(std::get<0>(phis).reg_, 1, data_ptr_type),
              ir::PtrIncr(std::get<1>(phis).reg_, 1, data_ptr_type)};
        },
        std::tuple{data_ptr_type, data_ptr_type}, tup{from_ptr, to_ptr});
    ir::ReturnJump();
  }
  return fn;
}

void Array::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  copy_assign_func_.init(
      [this, ctx]() { return CreateAssign<Copy>(this, ctx); });
  ir::Copy(this, std::get<ir::Register>(from.value), to);
}

void Array::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  move_assign_func_.init(
      [this, ctx]() { return CreateAssign<Move>(this, ctx); });
  ir::Move(this, std::get<ir::Register>(from.value), to);
}

void Array::EmitInit(ir::Register id_reg, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(Func({Ptr(this)}, {}),
                                    ast::FnParams<ast::Expression *>(1));

    CURRENT_FUNC(init_func_) {
      ir::BasicBlock::Current = init_func_->entry();

      auto ptr = ir::Index(type::Ptr(this), init_func_->Argument(0), 0);
      auto end_ptr =
          ir::PtrIncr(ptr, static_cast<i32>(len), type::Ptr(data_type));

      using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
      CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitInit(std::get<0>(phis).reg_, ctx);
            return tup{
                ir::PtrIncr(std::get<0>(phis).reg_, 1, type::Ptr(data_type))};
          },
          std::tuple<type::Type const *>{type::Ptr(data_type)}, tup{ptr});

      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}

static void ComputeDestroyWithoutLock(Array const *a, Context *ctx) {
  if (a->destroy_func_ != nullptr) { return; }
  a->destroy_func_ = ctx->mod_->AddFunc(type::Func({type::Ptr(a)}, {}),
                                        ast::FnParams<ast::Expression *>(1));

  CURRENT_FUNC(a->destroy_func_) {
    ir::BasicBlock::Current = a->destroy_func_->entry();
    auto arg                = a->destroy_func_->Argument(0);

    if (a->data_type->needs_destroy()) {
      Pointer const *ptr_to_data_type = type::Ptr(a->data_type);
      auto ptr                        = ir::Index(type::Ptr(a), arg, 0);
      auto end_ptr = ir::PtrIncr(ptr, a->len, ptr_to_data_type);

      using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
      CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            a->data_type->EmitDestroy(std::get<0>(phis).reg_, ctx);
            return tup{
                ir::PtrIncr(std::get<0>(phis).reg_, 1, ptr_to_data_type)};
          },
          std::tuple<type::Type const *>{ptr_to_data_type}, tup{ptr});
    }

    ir::ReturnJump();
  }
}

void Array::EmitDestroy(ir::Register reg, Context *ctx) const {
  if (!needs_destroy()) { return; }

  {
    std::unique_lock lock(mtx_);
    ComputeDestroyWithoutLock(this, ctx);
  }

  ir::Arguments call_args;
  call_args.append(reg);
  call_args.type_ = destroy_func_->type_;
  ir::Call(ir::AnyFunc{destroy_func_}, std::move(call_args));
}

void Array::EmitRepr(ir::Val const &val, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(Func({this}, {}),
                                    ast::FnParams<ast::Expression *>(1));

    CURRENT_FUNC(repr_func_) {
      ir::BasicBlock::Current = repr_func_->entry();

      auto exit_block = repr_func_->AddBlock();

      ir::Print(std::string_view{"["});

      ir::BasicBlock::Current = ir::EarlyExitOn<true>(exit_block, len == 0);
      auto ptr = ir::Index(type::Ptr(this), repr_func_->Argument(0), 0);

      data_type->EmitRepr(ir::Val::Reg(ir::PtrFix(ptr, data_type), data_type),
                          ctx);

      using tup = std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<i32>>;
      CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<1>(phis), 0); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            auto elem_ptr = ir::PtrIncr(std::get<0>(phis).reg_, 1,
                                        type::Ptr(this->data_type));

            ir::Print(std::string_view{", "});
            data_type->EmitRepr(
                ir::Val::Reg(ir::PtrFix(elem_ptr, data_type), data_type), ctx);

            return std::make_tuple(
                elem_ptr, ir::Sub(ir::RegisterOr<i32>(std::get<1>(phis)), 1));
          },
          std::tuple<type::Type const *, type::Type const *>{
              type::Ptr(this->data_type), type::Int32},
          tup{ptr, len - 1});
      ir::UncondJump(exit_block);

      ir::BasicBlock::Current = exit_block;
      ir::Print(std::string_view{"]"});
      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(val);
  call_args.type_ = repr_func_->type_;
  ir::Call(ir::AnyFunc{repr_func_}, std::move(call_args));
}

void Array::WriteTo(std::string *result) const {
  result->append("[");
  result->append(std::to_string(len));
  Type const *t = data_type;
  while (auto *array_ptr = t->if_as<Array>()) {
    result->append(", ");
    result->append(std::to_string(array_ptr->len));
    t = array_ptr->data_type;
  }
  result->append("; ");
  data_type->WriteTo(result);
  result->append("]");
}

bool Array::IsCopyable() const { return data_type->IsCopyable(); }
bool Array::IsMovable() const { return data_type->IsMovable(); }

}  // namespace type
