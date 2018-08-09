#include "type/array.h"

#include "architecture.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

IR::Val PtrCallFix(const IR::Val& v);

namespace type {
static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, IR::Func *>>>
    eq_funcs;
static base::guarded<base::unordered_map<
    const Array *, base::unordered_map<const Array *, IR::Func *>>>
    ne_funcs;
IR::Val Array::Compare(const Array *lhs_type, IR::Val lhs_ir,
                       const Array *rhs_type, IR::Val rhs_ir, bool equality,
                       Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    base::vector<std::pair<std::string, AST::Expression *>> args = {
        {"lhs", nullptr}, {"rhs", nullptr}};
    auto *fn = ctx->mod_->AddFunc(Func({Ptr(lhs_type), Ptr(rhs_type)}, {Bool}),
                                  std::move(args));
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();

      auto lhs_len = lhs_type->fixed_length
                         ? IR::Val::Int(static_cast<i32>(lhs_type->len))
                         : IR::Load(IR::ArrayLength(fn->Argument(0)));

      auto rhs_len = rhs_type->fixed_length
                         ? IR::Val::Int(static_cast<i32>(rhs_type->len))
                         : IR::Load(IR::ArrayLength(fn->Argument(1)));

      auto len_cmp = IR::Eq(lhs_len, rhs_len);

      auto equal_len_block = IR::Func::Current->AddBlock();
      auto true_block      = IR::Func::Current->AddBlock();
      auto false_block     = IR::Func::Current->AddBlock();
      auto phi_block       = IR::Func::Current->AddBlock();
      auto body_block      = IR::Func::Current->AddBlock();
      auto incr_block      = IR::Func::Current->AddBlock();

      IR::CondJump(len_cmp, equal_len_block, false_block);

      IR::BasicBlock::Current = true_block;
      IR::SetReturn(0, IR::Val::Bool(true));
      IR::ReturnJump();

      IR::BasicBlock::Current = false_block;
      IR::SetReturn(0, IR::Val::Bool(false));
      IR::ReturnJump();

      IR::BasicBlock::Current = equal_len_block;
      auto lhs_start          = IR::Index(fn->Argument(0), IR::Val::Int(0));
      auto rhs_start          = IR::Index(fn->Argument(1), IR::Val::Int(0));
      auto lhs_end            = IR::PtrIncr(lhs_start, lhs_len);
      IR::UncondJump(phi_block);

      IR::BasicBlock::Current = phi_block;
      auto lhs_phi_index      = IR::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi_index      = IR::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg =
          IR::Val::Reg(IR::Func::Current->Command(lhs_phi_index).result,
                       Ptr(lhs_type->data_type));
      auto rhs_phi_reg =
          IR::Val::Reg(IR::Func::Current->Command(rhs_phi_index).result,
                       Ptr(rhs_type->data_type));
      IR::CondJump(IR::Eq(lhs_phi_reg, lhs_end), true_block, body_block);

      IR::BasicBlock::Current = body_block;
      // TODO what if data type is an array?
      IR::CondJump(IR::Eq(IR::Load(lhs_phi_reg), IR::Load(rhs_phi_reg)),
                   incr_block, false_block);

      IR::BasicBlock::Current = incr_block;
      auto lhs_incr           = IR::PtrIncr(lhs_phi_reg, IR::Val::Int(1));
      auto rhs_incr           = IR::PtrIncr(rhs_phi_reg, IR::Val::Int(1));
      IR::UncondJump(phi_block);

      IR::MakePhi(lhs_phi_index,
                  {{equal_len_block, lhs_start}, {incr_block, lhs_incr}});
      IR::MakePhi(rhs_phi_index,
                  {{equal_len_block, rhs_start}, {incr_block, rhs_incr}});
    }
  }

  auto call_args = std::make_unique<IR::LongArgs>();
  call_args->append(lhs_ir);
  call_args->append(rhs_ir);
  auto outs = std::make_unique<IR::OutParams>();
  auto result = outs->AppendReg(type::Bool);

  IR::Call(IR::Val::Func(iter->second), std::move(call_args), std::move(outs));
  return {result};
}

static IR::Val ComputeMin(IR::Val x, IR::Val y) {
  ASSERT(x.type == y.type);
  auto entry_block = IR::BasicBlock::Current;
  auto x_block     = IR::Func::Current->AddBlock();
  auto land_block  = IR::Func::Current->AddBlock();
  IR::CondJump(IR::Lt(x, y), x_block, land_block);
  IR::BasicBlock::Current = x_block;
  IR::UncondJump(land_block);

  IR::BasicBlock::Current = land_block;
  return IR::MakePhi(IR::Phi(x.type),
                     {{x_block, std::move(x)}, {entry_block, std::move(y)}});
}

// TODO pass funcs by ref
base::vector<IR::Val> CreateLoop(
    const base::vector<IR::Val> &entry_vals,
    std::function<IR::Val(const base::vector<IR::Val> &)> loop_phi_fn,
    std::function<base::vector<IR::Val>(const base::vector<IR::Val> &)>
        loop_body_fn) {
  auto entry_block = IR::BasicBlock::Current;

  auto loop_phi   = IR::Func::Current->AddBlock();
  auto loop_body  = IR::Func::Current->AddBlock();
  auto exit_block = IR::Func::Current->AddBlock();

  IR::UncondJump(loop_phi);
  IR::BasicBlock::Current = loop_phi;

  base::vector<IR::Val> phi_vals;
  phi_vals.reserve(entry_vals.size());
  base::vector<IR::CmdIndex> phi_indices;
  phi_indices.reserve(entry_vals.size());
  for (const auto &val : entry_vals) {
    phi_vals.push_back(IR::Val::Reg(
        IR::Func::Current->Command(phi_indices.emplace_back(IR::Phi(val.type)))
            .result,
        val.type));
  }

  auto exit_cond = loop_phi_fn(phi_vals);
  IR::CondJump(exit_cond, exit_block, loop_body);

  IR::BasicBlock::Current = loop_body;
  auto new_phis           = loop_body_fn(phi_vals);
  IR::UncondJump(loop_phi);

  for (size_t i = 0; i < phi_indices.size(); ++i) {
    IR::MakePhi(phi_indices[i],
                {{entry_block, entry_vals[i]}, {loop_body, new_phis[i]}});
  }

  IR::BasicBlock::Current = exit_block;
  return phi_vals;
}

// TODO resize shoud probably take a custom allocator
void Array::EmitResize(IR::Val ptr_to_array, IR::Val new_size,
                       Context *ctx) const {
  // TODO these could maybe be moved into separate structs, or templated
  ASSERT(!fixed_length);
  {
    std::unique_lock lock(mtx_);
    if (resize_func_ != nullptr) { goto call_fn; }

    resize_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this), Int}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}, {"new_size", nullptr}});

    CURRENT_FUNC(resize_func_) {
      IR::BasicBlock::Current = resize_func_->entry();
      auto arg                = resize_func_->Argument(0);
      auto size_arg           = resize_func_->Argument(1);
      auto new_arr            = IR::Malloc(
          data_type,
          IR::Mul(size_arg,
                  IR::Val::Int(
                      Architecture::InterprettingMachine().bytes(data_type))));

      IR::Val from_ptr = IR::Index(arg, IR::Val::Int(0));
      IR::Val min_val  = ComputeMin(IR::Load(IR::ArrayLength(arg)), size_arg);
      IR::Val end_ptr  = IR::PtrIncr(from_ptr, min_val);

      auto finish_phis = CreateLoop(
          {from_ptr, new_arr},
          [&](const base::vector<IR::Val> &phis) {
            return IR::Eq(phis[0], end_ptr);
          },
          [&](const base::vector<IR::Val> &phis) {
            data_type->EmitAssign(data_type, PtrCallFix(phis[0]), phis[1], ctx);
            data_type->EmitDestroy(phis[0], ctx);
            return base::vector<IR::Val>{IR::PtrIncr(phis[0], IR::Val::Int(1)),
                                         IR::PtrIncr(phis[1], IR::Val::Int(1))};
          });

      if (data_type->needs_destroy()) {
        auto end_old_buf =
            IR::PtrIncr(IR::ArrayData(arg), IR::ArrayLength(arg));
        CreateLoop({end_ptr},
                   [&](const base::vector<IR::Val> &phis) {
                     return IR::Eq(phis[0], end_old_buf);
                   },
                   [&](const base::vector<IR::Val> &phis) {
                     data_type->EmitDestroy(phis[0], ctx);
                     return base::vector<IR::Val>{
                         IR::PtrIncr(phis[0], IR::Val::Int(1))};
                   });
      }

      auto end_to_ptr = IR::PtrIncr(new_arr, size_arg);
      CreateLoop(
          {finish_phis[1]},
          [&](const base::vector<IR::Val> &phis) {
            return IR::Eq(phis[0], end_to_ptr);
          },
          [&](const base::vector<IR::Val> &phis) {
            data_type->EmitInit(phis[0], ctx);
            return base::vector<IR::Val>{IR::PtrIncr(phis[0], IR::Val::Int(1))};
          });

      auto old_buf = IR::ArrayData(arg);
      IR::Store(size_arg, IR::ArrayLength(arg));
      IR::Free(IR::Load(old_buf));
      IR::Store(new_arr, old_buf);
      IR::ReturnJump();
    }
  }

call_fn:
  ASSERT(IR::Func::Current != nullptr);
  auto call_args = std::make_unique<IR::LongArgs>();
  call_args->append(ptr_to_array);
  call_args->append(new_size);
  IR::Call(IR::Val::Func(ASSERT_NOT_NULL(resize_func_)), std::move(call_args), nullptr);
}
}  // namespace type
