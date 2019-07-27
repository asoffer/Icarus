#ifndef ICARUS_IR_COMPONENTS_H
#define ICARUS_IR_COMPONENTS_H

#include "ir/cmd/basic.h"
#include "ir/cmd/load.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "ir/reg.h"
#include "ir/register.h"

namespace ir {
template <bool B>
BlockIndex EarlyExitOn(BlockIndex exit_block, RegOr<bool> cond) {
  auto continue_block = CompiledFn::Current->AddBlock();
  if constexpr (B) {
    CondJump(cond, exit_block, continue_block);
  } else {
    CondJump(cond, continue_block, exit_block);
  }
  return continue_block;
}

inline Reg PtrFix(Reg r, type::Type const *desired_type) {
  return desired_type->is_big() ? r : Load(r, desired_type);
}

// TODO: Typed<Results>
RegOr<bool> EmitEq(type::Type const *lhs_type, Results const &lhs_val,
                   type::Type const *rhs_type, Results const &rhs_val);

template <typename LoopPhiFn, typename LoopBodyFn, typename TypeTup,
          typename... Ts>
void CreateLoop(LoopPhiFn &&loop_phi_fn, LoopBodyFn &&loop_body_fn,
                TypeTup &&types, std::tuple<RegOr<Ts>...> entry_vals) {
  auto entry_block = BasicBlock::Current;

  auto loop_phi   = CompiledFn::Current->AddBlock();
  auto loop_body  = CompiledFn::Current->AddBlock();
  auto exit_block = CompiledFn::Current->AddBlock();

  UncondJump(loop_phi);
  BasicBlock::Current = loop_phi;

  auto phi_indices = base::tuple::transform(Phi, types);
  auto phi_vals    = base::tuple::transform(
      [](auto &&val) { return CompiledFn::Current->Command(val).result; },
      phi_indices);

  auto exit_cond = std::forward<LoopPhiFn>(loop_phi_fn)(phi_vals);
  CondJump(exit_cond, exit_block, loop_body);

  BasicBlock::Current = loop_body;
  auto new_phis       = std::forward<LoopBodyFn>(loop_body_fn)(phi_vals);
  UncondJump(loop_phi);

  base::tuple::for_each(
      [&](auto &&phi_index, auto &&entry_val, auto &&new_phi) {
        using T = std::decay_t<decltype(entry_val.val_)>;
        MakePhi<T>(phi_index, {{entry_block, entry_val}, {loop_body, new_phi}});
      },
      std::move(phi_indices), std::move(entry_vals), std::move(new_phis));

  BasicBlock::Current = exit_block;
}

template <typename F>
void OnEachArrayElement(type::Array const *t, CompiledFn *fn, F &&fn_to_apply) {
  CURRENT_FUNC(fn) {
    BasicBlock::Current = fn->entry();
    auto *data_ptr_type = type::Ptr(t->data_type);

    auto ptr     = Index(type::Ptr(t), Reg::Arg(0), 0);
    auto end_ptr = PtrIncr(ptr, static_cast<int32_t>(t->len), data_ptr_type);

    using tup = std::tuple<RegOr<Addr>>;
    CreateLoop([&](tup const &phis) { return Eq(std::get<0>(phis), end_ptr); },
               [&](tup const &phis) {
                 ASSERT(std::get<0>(phis).is_reg_ == true);
                 fn_to_apply(std::get<0>(phis).reg_);
                 return tup{PtrIncr(std::get<0>(phis).reg_, 1, data_ptr_type)};
               },
               std::tuple{data_ptr_type}, tup{ptr});

    ReturnJump();
  }
}

}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
