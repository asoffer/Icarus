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

namespace type {
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

}  // namespace type
