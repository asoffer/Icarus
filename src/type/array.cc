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
static base::guarded<std::unordered_map<
    const Array *, std::unordered_map<const Array *, IR::Func *>>>
    eq_funcs;
static base::guarded<std::unordered_map<
    const Array *, std::unordered_map<const Array *, IR::Func *>>>
    ne_funcs;
IR::Val Array::Compare(const Array *lhs_type, IR::Val lhs_ir,
                       const Array *rhs_type, IR::Val rhs_ir, bool equality,
                       Context *ctx) {
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    std::vector<std::pair<std::string, AST::Expression *>> args = {
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
      auto lhs_phi            = IR::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi            = IR::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg        = IR::Func::Current->Command(lhs_phi).reg();
      auto rhs_phi_reg        = IR::Func::Current->Command(rhs_phi).reg();
      IR::CondJump(IR::Eq(lhs_phi_reg, lhs_end), true_block, body_block);

      IR::BasicBlock::Current = body_block;
      // TODO what if data type is an array?
      IR::CondJump(IR::Eq(IR::Load(lhs_phi_reg), IR::Load(rhs_phi_reg)),
                   incr_block, false_block);

      IR::BasicBlock::Current = incr_block;
      auto lhs_incr           = IR::PtrIncr(lhs_phi_reg, IR::Val::Int(1));
      auto rhs_incr           = IR::PtrIncr(rhs_phi_reg, IR::Val::Int(1));
      IR::UncondJump(phi_block);

      fn->SetArgs(lhs_phi, {IR::Val::BasicBlock(equal_len_block), lhs_start,
                            IR::Val::BasicBlock(incr_block), lhs_incr});
      fn->SetArgs(rhs_phi, {IR::Val::BasicBlock(equal_len_block), rhs_start,
                            IR::Val::BasicBlock(incr_block), rhs_incr});
    }
  }

  return IR::Call(IR::Val::Func(iter->second), {lhs_ir, rhs_ir}, {});
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
  auto phi                = IR::Phi(x.type);
  IR::Func::Current->SetArgs(phi,
                             {IR::Val::BasicBlock(x_block), std::move(x),
                              IR::Val::BasicBlock(entry_block), std::move(y)});
  return IR::Func::Current->Command(phi).reg();
}

void Array::EmitResize(IR::Val ptr_to_array, IR::Val new_size,
                       Context *ctx) const {
  // TODO these could maybe be moved into separate structs, or templated
  ASSERT(!fixed_length);
  {
    std::unique_lock lock(mtx_);
    if (resize_func_ != nullptr) { goto call_fn; }

    resize_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this), Int}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
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

      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      auto exit_block = IR::Func::Current->AddBlock();

      IR::Val from_ptr = IR::Index(arg, IR::Val::Int(0));

      IR::Val min_val = ComputeMin(IR::Load(IR::ArrayLength(arg)), size_arg);
      auto start_block_val = IR::Val::BasicBlock(IR::BasicBlock::Current);

      auto end_ptr = IR::PtrIncr(from_ptr, min_val);
      IR::UncondJump(loop_phi);

      IR::BasicBlock::Current = loop_phi;
      auto from_phi           = IR::Phi(Ptr(data_type));
      auto from_phi_reg       = IR::Func::Current->Command(from_phi).reg();
      auto to_phi             = IR::Phi(Ptr(data_type));
      auto to_phi_reg         = IR::Func::Current->Command(to_phi).reg();

      IR::CondJump(IR::Eq(from_phi_reg, end_ptr), exit_block, loop_body);

      IR::BasicBlock::Current = loop_body;
      data_type->EmitDestroy(from_phi_reg, ctx);
      data_type->EmitAssign(data_type, PtrCallFix(from_phi_reg), to_phi_reg, ctx);

      data_type->EmitDestroy(from_phi_reg, ctx);

      auto from_incr = IR::PtrIncr(from_phi_reg, IR::Val::Int(1));
      auto to_incr   = IR::PtrIncr(to_phi_reg, IR::Val::Int(1));
      IR::UncondJump(loop_phi);

      resize_func_->SetArgs(
          from_phi, {start_block_val, from_ptr, IR::Val::BasicBlock(loop_body),
                     from_incr});
      resize_func_->SetArgs(to_phi, {start_block_val, new_arr,
                                     IR::Val::BasicBlock(loop_body), to_incr});
      IR::BasicBlock::Current = exit_block;



      IR::Store(size_arg, IR::ArrayLength(arg));
      IR::Store(new_arr, IR::ArrayData(arg));
      // TODO free the old buffer
      IR::ReturnJump();
    }
  }

call_fn:
  ASSERT(IR::Func::Current != nullptr);
  IR::Call(IR::Val::Func(ASSERT_NOT_NULL(resize_func_)),
           {std::move(ptr_to_array), std::move(new_size)}, {});
}
}  // namespace type
