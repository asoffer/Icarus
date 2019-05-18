#include "type/array.h"

#include "base/guarded.h"
#include "base/tuple.h"
#include "core/fn_params.h"
#include "ir/any_func.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "core/arch.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {

static base::guarded<absl::flat_hash_map<
    Array const *, absl::flat_hash_map<Array const *, ir::CompiledFn *>>>
    eq_funcs;
static base::guarded<absl::flat_hash_map<
    Array const *, absl::flat_hash_map<Array const *, ir::CompiledFn *>>>
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
            core::Param{"",
                        Typed<ast::Expression const *>{nullptr, Ptr(lhs_type)}},
            core::Param{
                "", Typed<ast::Expression const *>{nullptr, Ptr(rhs_type)}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto equal_len_block = ir::CompiledFn::Current->AddBlock();
      auto true_block      = ir::CompiledFn::Current->AddBlock();
      auto false_block     = ir::CompiledFn::Current->AddBlock();
      auto phi_block       = ir::CompiledFn::Current->AddBlock();
      auto body_block      = ir::CompiledFn::Current->AddBlock();
      auto incr_block      = ir::CompiledFn::Current->AddBlock();

      ir::CondJump(ir::Eq(lhs_type->len, rhs_type->len), equal_len_block,
                   false_block);

      ir::BasicBlock::Current = true_block;
      ir::SetRet(0, true);
      ir::ReturnJump();

      ir::BasicBlock::Current = false_block;
      ir::SetRet(0, false);
      ir::ReturnJump();

      ir::BasicBlock::Current = equal_len_block;
      auto lhs_start          = ir::Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start          = ir::Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end =
          ir::PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
      ir::UncondJump(phi_block);

      ir::BasicBlock::Current = phi_block;
      auto lhs_phi_index      = ir::Phi(Ptr(lhs_type->data_type));
      auto rhs_phi_index      = ir::Phi(Ptr(rhs_type->data_type));
      auto lhs_phi_reg = ir::CompiledFn::Current->Command(lhs_phi_index).result;
      auto rhs_phi_reg = ir::CompiledFn::Current->Command(rhs_phi_index).result;

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
Array const *Arr(size_t len, Type const *t) {
  auto handle = fixed_arrays_.lock();
  return (*handle)[t].emplace(len, new Array(len, t)).first->second;
}

void Array::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  data_type->defining_modules(modules);
}

void Array::EmitRepr(ir::Results const &val, Context *ctx) const {
  repr_func_.init([this, ctx]() {
    // TODO special function?
    ir::CompiledFn *fn = ctx->mod_->AddFunc(
        Func({this}, {}),
        core::FnParams(
            core::Param{"", Typed<ast::Expression const *>{nullptr, this}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto exit_block = fn->AddBlock();

      ir::Print(std::string_view{"["});

      ir::BasicBlock::Current = ir::EarlyExitOn<true>(exit_block, len == 0);
      auto ptr                = ir::Index(Ptr(this), ir::Reg::Arg(0), 0);

      data_type->EmitRepr(ir::Results{ir::PtrFix(ptr, data_type)}, ctx);

      using tup = std::tuple<ir::RegisterOr<ir::Addr>, ir::RegisterOr<int32_t>>;
      ir::CreateLoop(
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
  // TODO consider who is responsible for destruction here.
  auto arg = ir::Alloca(this);
  visitor::EmitIr visitor;
  if (from->is<Variant>()) {
    EmitMoveAssign(&visitor, this,
                   ir::Results{ir::VariantValue(this, val.get<ir::Reg>(0))},
                   arg, ctx);
  } else if (this == from) {
    EmitMoveAssign(&visitor, from, val, arg, ctx);
  } else {
    UNREACHABLE(from);
  }
  return ir::Results{arg};
}

core::Bytes Array::bytes(core::Arch const &a) const {
  return core::FwdAlign(data_type->bytes(a), data_type->alignment(a)) * len;
}

core::Alignment Array::alignment(core::Arch const &a) const {
  return data_type->alignment(a);
}

bool Array::ReinterpretAs(Type const *t) const {
  if (auto *a = t->if_as<Array>()) {
    return len == a->len && data_type->ReinterpretAs(a->data_type);
  }
  return false;
}

// TODO arrays are tricky because they may contain structs and so just using the
// result of this function is... maybe not what you intended.
Cmp Array::Comparator() const { return data_type->Comparator(); }

}  // namespace type
