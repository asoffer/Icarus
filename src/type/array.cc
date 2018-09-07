#include "type/array.h"

#include "architecture.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/func.h"
#include "ir/phi.h"
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
    auto *fn = ctx->mod_->AddFunc(
        type::Func({type::Ptr(lhs_type), type::Ptr(rhs_type)}, {type::Bool}),
        std::move(args));
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();

      auto lhs_len = [&]() -> IR::RegisterOr<i32> {
        if (lhs_type->fixed_length) { return static_cast<i32>(lhs_type->len); }
        return IR::LoadInt(IR::ArrayLength(fn->Argument(0)));
      }();

      auto rhs_len = [&]() -> IR::RegisterOr<i32> {
        if (rhs_type->fixed_length) { return static_cast<i32>(rhs_type->len); }
        return IR::LoadInt(IR::ArrayLength(fn->Argument(1)));
      }();

      auto equal_len_block = IR::Func::Current->AddBlock();
      auto true_block      = IR::Func::Current->AddBlock();
      auto false_block     = IR::Func::Current->AddBlock();
      auto phi_block       = IR::Func::Current->AddBlock();
      auto body_block      = IR::Func::Current->AddBlock();
      auto incr_block      = IR::Func::Current->AddBlock();

      IR::CondJump(IR::EqInt(lhs_len, rhs_len), equal_len_block, false_block);

      IR::BasicBlock::Current = true_block;
      IR::SetReturnBool(0, true);
      IR::ReturnJump();

      IR::BasicBlock::Current = false_block;
      IR::SetReturnBool(0, false);
      IR::ReturnJump();

      IR::BasicBlock::Current = equal_len_block;
      auto lhs_start = IR::Index(type::Ptr(lhs_type), fn->Argument(0), 0);
      auto rhs_start = IR::Index(type::Ptr(rhs_type), fn->Argument(1), 0);
      auto lhs_end =
          IR::PtrIncr(lhs_start, lhs_len, type::Ptr(rhs_type->data_type));
      IR::UncondJump(phi_block);

      IR::BasicBlock::Current = phi_block;
      auto lhs_phi_index      = IR::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi_index      = IR::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg = IR::Func::Current->Command(lhs_phi_index).result;
      auto rhs_phi_reg = IR::Func::Current->Command(rhs_phi_index).result;

      IR::CondJump(IR::EqAddr(lhs_phi_reg, lhs_end), true_block, body_block);

      IR::BasicBlock::Current = body_block;
      // TODO what if data type is an array?
      IR::CondJump(IR::EqAddr(IR::LoadAddr(lhs_phi_reg, lhs_type->data_type),
                              IR::LoadAddr(rhs_phi_reg, rhs_type->data_type)),
                   incr_block, false_block);

      IR::BasicBlock::Current = incr_block;
      auto lhs_incr           = IR::PtrIncr(lhs_phi_reg, 1, type::Ptr(lhs_type->data_type));
      auto rhs_incr           = IR::PtrIncr(rhs_phi_reg, 1, type::Ptr(rhs_type->data_type));
      IR::UncondJump(phi_block);

      IR::MakePhi<IR::Addr>(lhs_phi_index, {{equal_len_block, lhs_start},
                                            {incr_block, lhs_incr}});
      IR::MakePhi<IR::Addr>(rhs_phi_index, {{equal_len_block, rhs_start},
                                            {incr_block, rhs_incr}});
    }
  }

  IR::LongArgs call_args;
  call_args.append(lhs_ir);
  call_args.append(rhs_ir);
  call_args.type_ = iter->second->type_;

  IR::OutParams outs;
  auto result = outs.AppendReg(type::Bool);

  IR::Call(IR::AnyFunc{iter->second}, std::move(call_args), std::move(outs));
  return {IR::Val::Reg(result, type::Bool)};
}

static IR::RegisterOr<i32> ComputeMin(IR::RegisterOr<i32> x,
                                      IR::RegisterOr<i32> y) {
  auto entry_block = IR::BasicBlock::Current;
  auto x_block     = IR::Func::Current->AddBlock();
  auto land_block  = IR::Func::Current->AddBlock();
  IR::CondJump(IR::LtInt(x, y), x_block, land_block);
  IR::BasicBlock::Current = x_block;
  IR::UncondJump(land_block);

  IR::BasicBlock::Current = land_block;
  return IR::MakePhi<i32>(IR::Phi(type::Int), {{x_block, x}, {entry_block, y}});
}

// TODO pass funcs by ref
base::vector<IR::Val> CreateLoop(
    const base::vector<IR::Val> &entry_vals,
    std::function<IR::RegisterOr<bool>(const base::vector<IR::Val> &)>
        loop_phi_fn,
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
        type::Func({type::Ptr(this), type::Int}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}, {"new_size", nullptr}});

    CURRENT_FUNC(resize_func_) {
      IR::BasicBlock::Current = resize_func_->entry();
      auto arg                = resize_func_->Argument(0);
      auto size_arg           = resize_func_->Argument(1);

      auto new_arr = IR::Malloc(
          data_type,
          IR::MulInt(size_arg,
                     Architecture::InterprettingMachine().bytes(data_type)));

      IR::Register from_ptr = IR::Index(type::Ptr(this), arg, 0);
      IR::RegisterOr<i32> min_val =
          ComputeMin(IR::LoadInt(IR::ArrayLength(arg)), size_arg);
      IR::Register end_ptr =
          IR::PtrIncr(from_ptr, min_val, type::Ptr(data_type));

      auto finish_phis = CreateLoop(
          {IR::Val::Reg(from_ptr, type::Ptr(data_type)),
           IR::Val::Reg(new_arr, type::Ptr(data_type))},
          [&](const base::vector<IR::Val> &phis) {
            return IR::EqAddr(std::get<IR::Register>(phis[0].value), end_ptr);
          },
          [&](const base::vector<IR::Val> &phis) {
            data_type->EmitAssign(data_type, PtrCallFix(phis[0]), phis[1], ctx);
            data_type->EmitDestroy(phis[0], ctx);
            return base::vector<IR::Val>{
                IR::Val::Reg(IR::PtrIncr(std::get<IR::Register>(phis[0].value),
                                         1, phis[0].type),
                             phis[0].type),
                IR::Val::Reg(IR::PtrIncr(std::get<IR::Register>(phis[1].value),
                                         1, phis[1].type),
                             phis[1].type)};
          });

      if (data_type->needs_destroy()) {
        auto end_old_buf =
            IR::PtrIncr(IR::ArrayData(arg, type::Ptr(this)),
                        IR::LoadInt(IR::ArrayLength(arg)), type::Ptr(this));
        CreateLoop({IR::Val::Reg(end_ptr, type::Ptr(data_type))},
                   [&](const base::vector<IR::Val> &phis) {
                     return IR::EqAddr(std::get<IR::Register>(phis[0].value),
                                       end_old_buf);
                   },
                   [&](const base::vector<IR::Val> &phis) {
                     data_type->EmitDestroy(phis[0], ctx);
                     return base::vector<IR::Val>{IR::Val::Reg(
                         IR::PtrIncr(std::get<IR::Register>(phis[0].value), 1,
                                     phis[0].type),
                         phis[0].type)};
                   });
      }

      auto end_to_ptr = IR::PtrIncr(new_arr, size_arg, type::Ptr(data_type));
      CreateLoop(
          {finish_phis[1]},
          [&](const base::vector<IR::Val> &phis) {
            return IR::EqAddr(std::get<IR::Register>(phis[0].value),
                              end_to_ptr);
          },
          [&](const base::vector<IR::Val> &phis) {
            data_type->EmitInit(std::get<IR::Register>(phis[0].value), ctx);
            return base::vector<IR::Val>{IR::Val::Reg(
                IR::PtrIncr(std::get<IR::Register>(phis[0].value), 1, phis[0].type),
                phis[0].type)};
          });

      auto old_buf = IR::ArrayData(arg, type::Ptr(this));
      IR::StoreInt(size_arg, IR::ArrayLength(arg));
      IR::Free(IR::LoadAddr(old_buf, data_type));
      IR::StoreAddr(new_arr, old_buf);
      IR::ReturnJump();
    }
  }

call_fn:
  ASSERT(IR::Func::Current != nullptr);
  IR::LongArgs call_args;
  call_args.append(ptr_to_array);
  call_args.append(new_size);
  call_args.type_ = ASSERT_NOT_NULL(resize_func_)->type_;
  IR::Call(IR::AnyFunc{resize_func_}, std::move(call_args));
}
}  // namespace type
