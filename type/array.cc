#include "type/array.h"

#include "base/guarded.h"
#include "base/tuple.h"
#include "core/fn_params.h"
#include "ir/any_func.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "layout/arch.h"
#include "misc/context.h"
#include "misc/module.h"
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
                       {{entry_block, entry_val}, {loop_body, new_phi}});
      },
      std::move(phi_indices), std::move(entry_vals), std::move(new_phis));

  ir::BasicBlock::Current = exit_block;
}

namespace type {

static base::guarded<absl::flat_hash_map<
    Array const *, absl::flat_hash_map<Array const *, ir::Func *>>>
    eq_funcs;
static base::guarded<absl::flat_hash_map<
    Array const *, absl::flat_hash_map<Array const *, ir::Func *>>>
    ne_funcs;
// TODO this should early exit if the types aren't equal.
ir::Results Array::Compare(Array const *lhs_type, ir::Results const &lhs_ir,
                           Array const *rhs_type, ir::Results const &rhs_ir,
                           bool equality, Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto [iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto *fn = ctx->mod_->AddFunc(
        Func({Ptr(lhs_type), Ptr(rhs_type)}, {Bool}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(lhs_type)}},
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(rhs_type)}}));

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
      auto lhs_start          = ir::Index(Ptr(lhs_type), fn->Argument(0), 0);
      auto rhs_start          = ir::Index(Ptr(rhs_type), fn->Argument(1), 0);
      auto lhs_end =
          ir::PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
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
      auto lhs_incr = ir::PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type));
      auto rhs_incr = ir::PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::MakePhi<ir::Addr>(lhs_phi_index, {{equal_len_block, lhs_start},
                                            {incr_block, lhs_incr}});
      ir::MakePhi<ir::Addr>(rhs_phi_index, {{equal_len_block, rhs_start},
                                            {incr_block, rhs_incr}});
    }
  }

  ir::Arguments call_args{iter->second->type_, ir::Results{lhs_ir, rhs_ir}};
  ir::OutParams outs;
  auto result = outs.AppendReg(Bool);

  ir::Call(ir::AnyFunc{iter->second}, std::move(call_args), std::move(outs));
  return ir::Results{result};
}

static base::guarded<
    absl::flat_hash_map<Type const *, absl::flat_hash_map<size_t, Array *>>>
    fixed_arrays_;
Array const *Arr(Type const *t, size_t len) {
  auto handle = fixed_arrays_.lock();
  return (*handle)[t].emplace(len, new Array(t, len)).first->second;
}

void Array::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  data_type->defining_modules(modules);
}

template <SpecialFunctionCategory Cat>
static ir::Func *CreateAssign(Array const *a, Context *ctx) {
  Pointer const *ptr_type = Ptr(a);
  auto *data_ptr_type     = Ptr(a->data_type);
  auto *fn                = ctx->mod_->AddFunc(
      Func({ptr_type, ptr_type}, {}),
      core::FnParams(
          core::Param{"", Typed<ast::Expression *>{nullptr, ptr_type}},
          core::Param{"", Typed<ast::Expression *>{nullptr, ptr_type}}));
  CURRENT_FUNC(fn) {
    ir::BasicBlock::Current = fn->entry();
    auto val                = fn->Argument(0);
    auto var                = fn->Argument(1);

    auto from_ptr     = ir::Index(ptr_type, val, 0);
    auto from_end_ptr = ir::PtrIncr(from_ptr, a->len, data_ptr_type);
    auto to_ptr       = ir::Index(ptr_type, var, 0);

    using tup = std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
    CreateLoop(
        [&](tup const &phis) {
          return ir::Eq(std::get<0>(phis), from_end_ptr);
        },
        [&](tup const &phis) {
          ASSERT(std::get<0>(phis).is_reg_ == true);
          ASSERT(std::get<1>(phis).is_reg_ == true);

          auto from_val =
              ir::Results{PtrFix(std::get<0>(phis).reg_, a->data_type)};

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

void Array::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  copy_assign_func_.init(
      [this, ctx]() { return CreateAssign<Copy>(this, ctx); });
  ir::Copy(this, from.get<ir::Reg>(0), to);
}

void Array::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  move_assign_func_.init(
      [this, ctx]() { return CreateAssign<Move>(this, ctx); });
  ir::Move(this, from.get<ir::Reg>(0), to);
}

template <typename F>
static void OnEachElement(Array const *t, ir::Func *fn, F &&fn_to_apply) {
  CURRENT_FUNC(fn) {
    ir::BasicBlock::Current = fn->entry();
    auto *data_ptr_type     = Ptr(t->data_type);

    auto ptr = ir::Index(Ptr(t), fn->Argument(0), 0);
    auto end_ptr =
        ir::PtrIncr(ptr, static_cast<int32_t>(t->len), data_ptr_type);

    using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
    CreateLoop(
        [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
        [&](tup const &phis) {
          ASSERT(std::get<0>(phis).is_reg_ == true);
          fn_to_apply(std::get<0>(phis).reg_);
          return tup{ir::PtrIncr(std::get<0>(phis).reg_, 1, data_ptr_type)};
        },
        std::tuple{data_ptr_type}, tup{ptr});

    ir::ReturnJump();
  }
}

void Array::EmitInit(ir::Register id_reg, Context *ctx) const {
  init_func_.init([this, ctx]() {
    // TODO special function?
    auto *fn = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(this)}}));
    OnEachElement(this, fn,
                  [this, ctx](ir::Register r) { data_type->EmitInit(r, ctx); });
    return fn;
  });

  ir::Init(this, id_reg);
}

void Array::EmitDestroy(ir::Register reg, Context *ctx) const {
  if (!data_type->needs_destroy()) { return; }
  destroy_func_.init([this, ctx]() {
    // TODO special function?
    auto *fn = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression *>{nullptr, Ptr(this)}}));

    OnEachElement(this, fn, [this, ctx](ir::Register r) {
      data_type->EmitDestroy(r, ctx);
    });
    return fn;
  });

  ir::Destroy(this, reg);
}

void Array::EmitRepr(ir::Results const &val, Context *ctx) const {
  repr_func_.init([this, ctx]() {
    // TODO special function?
    ir::Func *fn = ctx->mod_->AddFunc(
        Func({this}, {}), core::FnParams(core::Param{
                              "", Typed<ast::Expression *>{nullptr, this}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto exit_block = fn->AddBlock();

      ir::Print(std::string_view{"["});

      ir::BasicBlock::Current = ir::EarlyExitOn<true>(exit_block, len == 0);
      auto ptr                = ir::Index(Ptr(this), fn->Argument(0), 0);

      data_type->EmitRepr(ir::Results{ir::PtrFix(ptr, data_type)}, ctx);

      using tup = std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<int32_t>>;
      CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<1>(phis), 0); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_ == true);
            auto elem_ptr =
                ir::PtrIncr(std::get<0>(phis).reg_, 1, Ptr(data_type));

            ir::Print(std::string_view{", "});
            data_type->EmitRepr(ir::Results{ir::PtrFix(elem_ptr, data_type)}, ctx);

            return std::make_tuple(
                elem_ptr, ir::Sub(ir::RegisterOr<int32_t>(std::get<1>(phis)), 1));
          },
          std::tuple{Ptr(this->data_type), Int32}, tup{ptr, len - 1});
      ir::UncondJump(exit_block);

      ir::BasicBlock::Current = exit_block;
      ir::Print(std::string_view{"]"});
      ir::ReturnJump();
    }

    return fn;
  });

  ir::Call(ir::AnyFunc{repr_func_.get()},
           ir::Arguments{repr_func_.get()->type_, val});
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
  t->WriteTo(result);
  result->append("]");
}

bool Array::IsCopyable() const { return data_type->IsCopyable(); }
bool Array::IsMovable() const { return data_type->IsMovable(); }

ir::Results Array::PrepareArgument(Type const *from, ir::Results const &val,
                                   Context *ctx) const {
  if (from->is<Variant>()) {
    NOT_YET(this, from);
  } else {
    ASSERT(from == this);
    // TODO Copy may be overkill. Think about value category.
    auto arg = ir::Alloca(from);
    from->EmitMoveAssign(from, val, arg, ctx);
    return ir::Results{arg};
  }
}

layout::Bytes Array::bytes(layout::Arch const &a) const {
  return layout::FwdAlign(data_type->bytes(a), data_type->alignment(a)) * len;
}

layout::Alignment Array::alignment(layout::Arch const &a) const {
  return data_type->alignment(a);
}

// TODO arrays are tricky because they may contain structs and so just using the
// result of this function is... maybe not what you intended.
Cmp Array::Comparator() const { return data_type->Comparator(); }

}  // namespace type
