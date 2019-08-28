#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "ir/builder.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/store.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "misc/context.h"
#include "type/primitive.h"
#include "visitor/emit_ir.h"
#include "visitor/special_function.h"

namespace visitor {

template <SpecialFunctionCategory Cat>
static ir::CompiledFn *CreateAssign(EmitIr *visitor, type::Array const *a,
                                    Context *ctx) {
  type::Pointer const *ptr_type = type::Ptr(a);
  auto *data_ptr_type           = type::Ptr(a->data_type);
  auto *fn                      = ctx->mod_->AddFunc(
      type::Func({ptr_type, ptr_type}, {}),
      core::FnParams(
          core::Param{"",
                      type::Typed<ast::Expression const *>{nullptr, ptr_type}},
          core::Param{
              "", type::Typed<ast::Expression const *>{nullptr, ptr_type}}));
  ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
    visitor->builder().CurrentBlock() = fn->entry();
    auto val                          = ir::Reg::Arg(0);
    auto var                          = ir::Reg::Arg(1);

    auto from_ptr     = ir::Index(ptr_type, val, 0);
    auto from_end_ptr = ir::PtrIncr(from_ptr, a->len, data_ptr_type);
    auto to_ptr       = ir::Index(ptr_type, var, 0);

    ir::CreateLoop(
        [&](ir::RegOr<ir::Addr> const &phi, ir::RegOr<ir::Addr> const &) {
          return ir::Eq(phi, from_end_ptr);
        },
        [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<ir::Addr> const &phi1) {
          ASSERT(phi0.is_reg_ == true);
          ASSERT(phi1.is_reg_ == true);

          auto from_val = ir::Results{PtrFix(phi0.reg_, a->data_type)};

          if constexpr (Cat == Copy) {
            a->data_type->EmitCopyAssign(visitor, a->data_type, from_val,
                                         phi1.reg_, ctx);
          } else if constexpr (Cat == Move) {
            a->data_type->EmitMoveAssign(visitor, a->data_type, from_val,
                                         phi1.reg_, ctx);
          } else {
            UNREACHABLE();
          }

          return std::tuple{ir::PtrIncr(phi0.reg_, 1, data_ptr_type),
                            ir::PtrIncr(phi1.reg_, 1, data_ptr_type)};
        },
        std::tuple{data_ptr_type, data_ptr_type},
        std::tuple{ir::RegOr<ir::Addr>(from_ptr), ir::RegOr<ir::Addr>(to_ptr)});
    ir::ReturnJump();
  }
  return fn;
}

template <SpecialFunctionCategory Cat>
static ir::AnyFunc CreateAssign(EmitIr *visitor, type::Struct const *s,
                                Context *ctx) {
  if (auto fn = SpecialFunction(visitor, s, Name<Cat>(), ctx)) { return *fn; }
  type::Pointer const *pt = type::Ptr(s);
  ir::AnyFunc fn          = s->mod_->AddFunc(
      type::Func({pt, pt}, {}),
      core::FnParams(
          core::Param{"", type::Typed<ast::Expression const *>{nullptr, pt}},
          core::Param{"", type::Typed<ast::Expression const *>{nullptr, pt}}));
  ICARUS_SCOPE(ir::SetCurrentFunc(fn.func())) {
    visitor->builder().CurrentBlock() = fn.func()->entry();
    auto val                          = ir::Reg::Arg(0);
    auto var                          = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields_.size(); ++i) {
      auto *field_type = s->fields_.at(i).type;
      auto from =
          ir::Results{ir::PtrFix(ir::Field(val, s, i).get(), field_type)};
      auto to = ir::Field(var, s, i).get();

      if constexpr (Cat == Copy) {
        field_type->EmitCopyAssign(visitor, field_type, from, to, ctx);
      } else if constexpr (Cat == Move) {
        field_type->EmitMoveAssign(visitor, field_type, from, to, ctx);
      } else {
        UNREACHABLE();
      }
    }

    ir::ReturnJump();
  }
  return fn;
}

void EmitIr::CopyAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->copy_assign_func_.init([=]() { return CreateAssign<Copy>(this, t, ctx); });
  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void EmitIr::MoveAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->move_assign_func_.init([=]() { return CreateAssign<Move>(this, t, ctx); });
  ir::Move(t, from->get<ir::Reg>(0), to);
}

void EmitIr::CopyAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::EnumVal>(0), to);
}

void EmitIr::MoveAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  CopyAssign(t, to, from, ctx);
}

void EmitIr::CopyAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::FlagsVal>(0), to);
}

void EmitIr::MoveAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  CopyAssign(t, to, from, ctx);
}

void EmitIr::CopyAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::AnyFunc>(0), to);
}

void EmitIr::MoveAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  CopyAssign(t, to, from, ctx);
}

void EmitIr::CopyAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  if (t == from.type()) {
    ir::Store(from->get<ir::Addr>(0), to);
  } else if (from.type() == type::NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE();
  }
}

void EmitIr::MoveAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  CopyAssign(t, to, from, ctx);
}

void EmitIr::CopyAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  ASSERT(t == from.type());
  switch (t->type_) {
    case type::PrimType::Type_:
      ir::Store(from->get<type::Type const *>(0), to);
      break;
    case type::PrimType::NullPtr: UNREACHABLE();
    case type::PrimType::EmptyArray: UNREACHABLE();
    case type::PrimType::Bool: ir::Store(from->get<bool>(0), to); break;
    case type::PrimType::Int8: ir::Store(from->get<int8_t>(0), to); break;
    case type::PrimType::Int16: ir::Store(from->get<int16_t>(0), to); break;
    case type::PrimType::Int32: ir::Store(from->get<int32_t>(0), to); break;
    case type::PrimType::Int64: ir::Store(from->get<int64_t>(0), to); break;
    case type::PrimType::Nat8: ir::Store(from->get<uint8_t>(0), to); break;
    case type::PrimType::Nat16: ir::Store(from->get<uint16_t>(0), to); break;
    case type::PrimType::Nat32: ir::Store(from->get<uint32_t>(0), to); break;
    case type::PrimType::Nat64: ir::Store(from->get<uint64_t>(0), to); break;
    case type::PrimType::Float32: ir::Store(from->get<float>(0), to); break;
    case type::PrimType::Float64: ir::Store(from->get<double>(0), to); break;
    default: UNREACHABLE();
  }
}

void EmitIr::MoveAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  CopyAssign(t, to, from, ctx);
}

void EmitIr::CopyAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->copy_assign_func_.init([=]() {
    type::Pointer const *p = type::Ptr(t);
    auto *fn               = ctx->mod_->AddFunc(
        type::Func({p, p}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, p}},
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, p}}));
    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        entry->EmitCopyAssign(
            this, entry,
            ir::Results{ir::PtrFix(ir::Field(val, t, i).get(), entry)},
            ir::Field(var, t, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void EmitIr::MoveAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->move_assign_func_.init([=]() {
    type::Pointer const *p = type::Ptr(t);
    auto *fn               = ctx->mod_->AddFunc(
        type::Func({p, p}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, p}},
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, p}}));
    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        entry->EmitMoveAssign(
            this, entry,
            ir::Results{ir::PtrFix(ir::Field(val, t, i).get(), entry)},
            ir::Field(var, t, i).get(), ctx);
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Move(t, from->get<ir::Reg>(0), to);
}

void EmitIr::CopyAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_ == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  t->EmitDestroy(this, to.reg_, ctx);

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type =
        ir::Load<type::Type const *>(ir::VariantType(from->get<ir::Reg>(0)));
    auto landing = builder().AddBlock();
    auto var_val = ir::VariantValue(from_var_type, from->get<ir::Reg>(0));
    for (type::Type const *v : from_var_type->variants_) {
      auto next_block = builder().AddBlock();
      builder().CurrentBlock() =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitCopyAssign(this, v, ir::Results{ir::PtrFix(var_val, v)},
                        ir::VariantValue(t, to), ctx);
      ir::UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    ir::UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    ir::Store(from.type(), ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    best_match->EmitCopyAssign(this, from.type(), from.get(),
                               ir::VariantValue(t, to), ctx);
  }
}

void EmitIr::MoveAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg_ == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  t->EmitDestroy(this, to.reg_, ctx);

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type =
        ir::Load<type::Type const *>(ir::VariantType(from->get<ir::Reg>(0)));
    auto landing = builder().AddBlock();
    auto var_val = ir::VariantValue(from_var_type, from->get<ir::Reg>(0));
    for (type::Type const *v : from_var_type->variants_) {
      auto next_block = builder().AddBlock();
      builder().CurrentBlock() =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      v->EmitMoveAssign(this, v, ir::Results{ir::PtrFix(var_val, v)},
                        ir::VariantValue(t, to), ctx);
      ir::UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    ir::UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    ir::Store(from.type(), ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    best_match->EmitMoveAssign(this, from.type(), from.get(),
                               ir::VariantValue(t, to), ctx);
  }
}

void EmitIr::CopyAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->copy_assign_func_.init([=]() { return CreateAssign<Copy>(this, t, ctx); });
  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void EmitIr::MoveAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                        type::Typed<ir::Results> const &from, Context *ctx) {
  t->move_assign_func_.init([=]() { return CreateAssign<Move>(this, t, ctx); });
  ir::Move(t, from->get<ir::Reg>(0), to);
}

}  // namespace visitor
