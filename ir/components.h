#ifndef ICARUS_IR_COMPONENTS_H
#define ICARUS_IR_COMPONENTS_H

#include "ir/builder.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"

namespace ir {
base::Tagged<Addr, Reg> Index(type::Pointer const *t, Reg array_ptr,
                              RegOr<int64_t> offset);

template <bool B>
BasicBlock *EarlyExitOn(BasicBlock *exit_block, RegOr<bool> cond) {
  auto *continue_block = GetBuilder().AddBlock();
  if constexpr (B) {
    GetBuilder().CondJump(cond, exit_block, continue_block);
  } else {
    GetBuilder().CondJump(cond, continue_block, exit_block);
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
  NOT_YET();
  /*
  auto entry_block = GetBuilder().CurrentBlock();

  auto *loop_phi   = GetBuilder().AddBlock();
  auto *loop_body  = GetBuilder().AddBlock();
  auto *exit_block = GetBuilder().AddBlock();

  GetBuilder().UncondJump(loop_phi);
  GetBuilder().CurrentBlock() = loop_phi;

  auto phi_indices = base::tuple::transform([&]() { ir::Phi(); }, types);
  auto phi_vals    = base::tuple::transform(
      [](auto &&val) { return GetBuilder().Command(val).result; },
      phi_indices);

  auto exit_cond = std::apply(std::forward<LoopPhiFn>(loop_phi_fn), phi_vals);
  GetBuilder().CondJump(exit_cond, exit_block, loop_body);

  GetBuilder().CurrentBlock() = loop_body;
  auto new_phis = std::apply(std::forward<LoopBodyFn>(loop_body_fn), phi_vals);
  GetBuilder().UncondJump(loop_phi);

  base::tuple::for_each(
      [&](auto &&phi_index, auto &&entry_val, auto &&new_phi) {
        using T = std::decay_t<decltype(entry_val.value())>;
        MakePhi<T>(phi_index, {{entry_block, entry_val}, {loop_body, new_phi}});
      },
      std::move(phi_indices), std::move(entry_vals), std::move(new_phis));

  GetBuilder().CurrentBlock() = exit_block;
  */
}

template <typename F>
void OnEachArrayElement(type::Array const *t, CompiledFn *fn, F &&fn_to_apply) {
  ICARUS_SCOPE(SetCurrent(fn)) {
    GetBuilder().CurrentBlock() = fn->entry();
    auto *data_ptr_type         = type::Ptr(t->data_type);

    auto ptr = Index(type::Ptr(t), Reg::Arg(0), 0);
    auto end_ptr =
        GetBuilder().PtrIncr(ptr, static_cast<int32_t>(t->len), data_ptr_type);

    using tup = std::tuple<RegOr<Addr>>;
    CreateLoop(
        [&](tup const &phis) {
          return GetBuilder().Eq(std::get<0>(phis), end_ptr);
        },
        [&](tup const &phis) {
          ASSERT(std::get<0>(phis).is_reg() == true);
          fn_to_apply(std::get<0>(phis).reg());
          return tup{
              GetBuilder().PtrIncr(std::get<0>(phis).reg(), 1, data_ptr_type)};
        },
        std::tuple{data_ptr_type}, tup{ptr});

    GetBuilder().ReturnJump();
  }
}

}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
