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

void Array::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(from_type, Is<Array>());
  auto *from_array_type = &from_type->as<Array>();

  std::unique_lock lock(mtx_);
  if (copy_assign_func_ == nullptr) {
    copy_assign_func_ = ctx->mod_->AddFunc(type::Func({from_type, type::Ptr(this)}, {}),
                            ast::FnParams<ast::Expression *>(2));

    CURRENT_FUNC(copy_assign_func_) {
      ir::BasicBlock::Current = copy_assign_func_->entry();
      auto val                = copy_assign_func_->Argument(0);
      auto var                = copy_assign_func_->Argument(1);

      auto *from_ptr_type = type::Ptr(from_array_type->data_type);
      auto from_ptr       = ir::Index(type::Ptr(from_type), val, 0);
      auto from_end_ptr =
          ir::PtrIncr(from_ptr, from_array_type->len, from_ptr_type);
      auto *to_ptr_type               = type::Ptr(data_type);
      ir::RegisterOr<ir::Addr> to_ptr = ir::Index(type::Ptr(this), var, 0);

      using tup =
          std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) {
            return ir::Eq(std::get<0>(phis), from_end_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            ASSERT(std::get<1>(phis).is_reg_);

            ir::Register ptr_fixed_reg =
                from_array_type->data_type->is_big()
                    ? std::get<0>(phis).reg_
                    : ir::Load(std::get<0>(phis).reg_, data_type);
            auto ptr_fixed_type = from_array_type->data_type->is_big()
                                      ? from_array_type->data_type
                                      : type::Ptr(from_array_type->data_type);

            EmitCopyInit(from_array_type->data_type, data_type,
                         ir::Val::Reg(ptr_fixed_reg, ptr_fixed_type),
                         std::get<1>(phis).reg_, ctx);
            return std::make_tuple(
                ir::PtrIncr(std::get<0>(phis).reg_, 1, from_ptr_type),
                ir::PtrIncr(std::get<1>(phis).reg_, 1, to_ptr_type));
          },
          std::tuple<type::Type const *, type::Type const *>{from_ptr_type,
                                                             to_ptr_type},
          tup{from_ptr, to_ptr});
      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = copy_assign_func_->type_;
  ir::Call(ir::AnyFunc{copy_assign_func_}, std::move(call_args));
}

void Array::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  ASSERT(from_type, Is<Array>());
  auto *from_array_type = &from_type->as<Array>();

  std::unique_lock lock(mtx_);
  if (move_assign_func_ == nullptr) {
    move_assign_func_ =
        ctx->mod_->AddFunc(type::Func({from_type, type::Ptr(this)}, {}),
                           ast::FnParams<ast::Expression *>(2));

    CURRENT_FUNC(move_assign_func_) {
      ir::BasicBlock::Current = move_assign_func_->entry();
      auto val                = move_assign_func_->Argument(0);
      auto var                = move_assign_func_->Argument(1);

      auto *from_ptr_type = type::Ptr(from_array_type->data_type);
      auto from_ptr       = ir::Index(type::Ptr(from_type), val, 0);
      auto from_end_ptr =
          ir::PtrIncr(from_ptr, from_array_type->len, from_ptr_type);
      auto *to_ptr_type               = type::Ptr(data_type);
      ir::RegisterOr<ir::Addr> to_ptr = ir::Index(type::Ptr(this), var, 0);

      using tup =
          std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) {
            return ir::Eq(std::get<0>(phis), from_end_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            ASSERT(std::get<1>(phis).is_reg_);

            ir::Register ptr_fixed_reg =
                from_array_type->data_type->is_big()
                    ? std::get<0>(phis).reg_
                    : ir::Load(std::get<0>(phis).reg_, data_type);
            auto ptr_fixed_type = from_array_type->data_type->is_big()
                                      ? from_array_type->data_type
                                      : type::Ptr(from_array_type->data_type);

            EmitMoveInit(from_array_type->data_type, data_type,
                         ir::Val::Reg(ptr_fixed_reg, ptr_fixed_type),
                         std::get<1>(phis).reg_, ctx);
            return std::make_tuple(
                ir::PtrIncr(std::get<0>(phis).reg_, 1, from_ptr_type),
                ir::PtrIncr(std::get<1>(phis).reg_, 1, to_ptr_type));
          },
          std::tuple<type::Type const *, type::Type const *>{from_ptr_type,
                                                             to_ptr_type},
          tup{from_ptr, to_ptr});
      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = move_assign_func_->type_;
  ir::Call(ir::AnyFunc{move_assign_func_}, std::move(call_args));
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
      ir::CreateLoop(
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
      ir::CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            a->data_type->EmitDestroy(std::get<0>(phis).reg_, ctx);
            return tup{ir::PtrIncr(std::get<0>(phis).reg_, 1, ptr_to_data_type)};
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
      ir::CreateLoop(
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
