#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "compiler/special_function.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/fn.h"
#include "ir/value/value.h"
#include "type/primitive.h"

namespace compiler {

template <SpecialFunctionCategory Cat>
static ir::NativeFn CreateAssign(Compiler *compiler, type::Array const *a) {
  type::QualType q    = type::QualType::NonConstant(type::Ptr(a));
  auto *data_ptr_type = type::Ptr(a->data_type());
  auto fn_type =
      type::Func(core::Params<type::QualType>{core::AnonymousParam(q),
                                              core::AnonymousParam(q)},
                 {});
  ir::NativeFn fn = compiler->AddFunc(
      fn_type, fn_type->params().Transform([](type::QualType q) {
        return type::Typed<ast::Declaration const *>(nullptr, q.type());
      }));
  ICARUS_SCOPE(ir::SetCurrent(fn)) {
    auto &bldr          = compiler->builder();
    bldr.CurrentBlock() = fn->entry();
    auto val            = ir::Reg::Arg(0);
    auto var            = ir::Reg::Arg(1);

    auto from_ptr     = bldr.PtrIncr(val, 0, data_ptr_type);
    auto from_end_ptr = bldr.PtrIncr(from_ptr, a->length(), data_ptr_type);
    auto to_ptr       = bldr.PtrIncr(var, 0, data_ptr_type);

    auto *loop_body  = bldr.AddBlock();
    auto *land_block = bldr.AddBlock();
    auto *cond_block = bldr.AddBlock();

    bldr.UncondJump(cond_block);

    bldr.CurrentBlock() = cond_block;
    auto *from_phi      = bldr.PhiInst<ir::Addr>();
    auto *to_phi        = bldr.PhiInst<ir::Addr>();
    bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(from_phi->result), from_end_ptr),
                  land_block, loop_body);

    bldr.CurrentBlock() = loop_body;
    if constexpr (Cat == Copy) {
      compiler->Visit(a->data_type(), to_phi->result,
                      type::Typed{ir::Value(compiler->builder().PtrFix(
                                      from_phi->result, a->data_type())),
                                  a->data_type()},
                      EmitCopyAssignTag{});
    } else if constexpr (Cat == Move) {
      compiler->Visit(a->data_type(), to_phi->result,
                      type::Typed{ir::Value(compiler->builder().PtrFix(
                                      from_phi->result, a->data_type())),
                                  a->data_type()},
                      EmitMoveAssignTag{});
    } else {
      UNREACHABLE();
    }

    ir::Reg next_to   = bldr.PtrIncr(to_phi->result, 1, data_ptr_type);
    ir::Reg next_from = bldr.PtrIncr(from_phi->result, 1, data_ptr_type);
    bldr.UncondJump(cond_block);

    to_phi->add(fn->entry(), to_ptr);
    to_phi->add(bldr.CurrentBlock(), next_to);
    from_phi->add(fn->entry(), from_ptr);
    from_phi->add(bldr.CurrentBlock(), next_from);

    bldr.CurrentBlock() = land_block;
    bldr.ReturnJump();
  }

  fn->WriteByteCode();
  return fn;
}

template <SpecialFunctionCategory Cat>
static ir::NativeFn CreateAssign(Compiler *compiler, type::Struct const *s) {
  if (auto fn = SpecialFunction(compiler, s, Name<Cat>())) {
    return fn->native();
  }
  auto &bldr       = compiler->builder();
  type::QualType q = type::QualType::NonConstant(type::Ptr(s));
  auto fn_type =
      type::Func(core::Params<type::QualType>{core::AnonymousParam(q),
                                              core::AnonymousParam(q)},
                 {});
  ir::NativeFn fn = compiler->AddFunc(
      fn_type, fn_type->params().Transform([](type::QualType q) {
        return type::Typed<ast::Declaration const *>(nullptr, q.type());
      }));
  ICARUS_SCOPE(ir::SetCurrent(fn)) {
    bldr.CurrentBlock() = fn->entry();
    auto val            = ir::Reg::Arg(0);
    auto var            = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields_.size(); ++i) {
      auto *field_type = s->fields_.at(i).type;
      auto from        = ir::Value(
          compiler->builder().PtrFix(bldr.Field(val, s, i).get(), field_type));
      auto to = bldr.Field(var, s, i).get();

      // TODO use the tag in place of `Cat`.
      if constexpr (Cat == Copy) {
        compiler->Visit(field_type, to, type::Typed{from, field_type},
                        EmitCopyAssignTag{});
      } else if constexpr (Cat == Move) {
        compiler->Visit(field_type, to, type::Typed{from, field_type},
                        EmitMoveAssignTag{});
      } else {
        UNREACHABLE();
      }
    }

    bldr.ReturnJump();
  }
  fn->WriteByteCode();
  return fn;
}

void Compiler::Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  data().copy_assign_.emplace(
      t, base::lazy_convert{[&] { return CreateAssign<Copy>(this, t); }});
  builder().Copy(t, from->get<ir::Reg>(), to);
}

void Compiler::Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  data().move_assign_.emplace(
      t, base::lazy_convert{[&] { return CreateAssign<Move>(this, t); }});
  builder().Move(t, from->get<ir::Reg>(), to);
}

void Compiler::Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  builder().Store(from->get<ir::EnumVal>(), to);
}

void Compiler::Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  builder().Store(from->get<ir::FlagsVal>(), to);
}

void Compiler::Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  builder().Store(from->get<ir::Fn>(), to);
}

void Compiler::Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  if (t == from.type()) {
    builder().Store(from->get<ir::Addr>(), to);
  } else if (from.type() == type::NullPtr) {
    builder().Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE();
  }
}

void Compiler::Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  switch (t->type_) {
    case type::BasicType::Type_:
      builder().Store(from->get<ir::RegOr<type::Type const *>>(), to);
      break;
    case type::BasicType::NullPtr: UNREACHABLE();
    case type::BasicType::EmptyArray: UNREACHABLE();
    case type::BasicType::Bool:
      builder().Store(from->get<ir::RegOr<bool>>(), to);
      break;
    case type::BasicType::Int8:
      builder().Store(from->get<ir::RegOr<int8_t>>(), to);
      break;
    case type::BasicType::Int16:
      builder().Store(from->get<ir::RegOr<int16_t>>(), to);
      break;
    case type::BasicType::Int32:
      builder().Store(from->get<ir::RegOr<int32_t>>(), to);
      break;
    case type::BasicType::Int64:
      builder().Store(from->get<ir::RegOr<int64_t>>(), to);
      break;
    case type::BasicType::Nat8:
      builder().Store(from->get<ir::RegOr<uint8_t>>(), to);
      break;
    case type::BasicType::Nat16:
      builder().Store(from->get<ir::RegOr<uint16_t>>(), to);
      break;
    case type::BasicType::Nat32:
      builder().Store(from->get<ir::RegOr<uint32_t>>(), to);
      break;
    case type::BasicType::Nat64:
      builder().Store(from->get<ir::RegOr<uint64_t>>(), to);
      break;
    case type::BasicType::Float32:
      builder().Store(from->get<ir::RegOr<float>>(), to);
      break;
    case type::BasicType::Float64:
      builder().Store(from->get<ir::RegOr<double>>(), to);
      break;
    case type::BasicType::ByteView:
      builder().Store(from->get<ir::RegOr<ir::String>>(), to);
      break;
    default: UNREACHABLE();
  }
}

void Compiler::Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  t->copy_assign_func_.init([=]() {
    type::QualType q = type::QualType::NonConstant(type::Ptr(t));
    auto fn_type =
        type::Func(core::Params<type::QualType>{core::AnonymousParam(q),
                                                core::AnonymousParam(q)},
                   {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));
    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        Visit(entry, builder().Field(var, t, i).get(),
              type::Typed{ir::Value(builder().PtrFix(
                              builder().Field(val, t, i).get(), entry)),
                          entry},
              EmitCopyAssignTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });

  builder().Copy(t, from->get<ir::Reg>(), to);
}

void Compiler::Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  t->move_assign_func_.init([=]() {
    type::QualType q = type::QualType::NonConstant(type::Ptr(t));
    auto fn_type =
        type::Func(core::Params<type::QualType>{core::AnonymousParam(q),
                                                core::AnonymousParam(q)},
                   {});
    ir::NativeFn fn =
        AddFunc(fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));
    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        Visit(entry, builder().Field(var, t, i).get(),
              type::Typed{ir::Value(builder().PtrFix(
                              builder().Field(val, t, i).get(), entry)),
                          entry},
              EmitMoveAssignTag{});
      }

      builder().ReturnJump();
    }
    return fn;
  });

  builder().Move(t, from->get<ir::Reg>(), to);
}

void Compiler::Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg() == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  Visit(t, to.reg(), EmitDestroyTag{});

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type = builder().Load<type::Type const *>(
        builder().VariantType(from->get<ir::Reg>()));
    auto *landing = builder().AddBlock();
    auto var_val  = builder().VariantValue(from_var_type, from->get<ir::Reg>());
    for (type::Type const *v : from_var_type->variants_) {
      auto *next_block         = builder().AddBlock();
      builder().CurrentBlock() = builder().EarlyExitOn<false>(
          next_block, builder().Eq(actual_type, v));
      builder().Store(v, builder().VariantType(to));
      Visit(v, builder().VariantValue(t, to),
            type::Typed{ir::Value(builder().PtrFix(var_val, v)), v},
            EmitCopyAssignTag{});
      builder().UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    builder().UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    builder().Store(from.type(), builder().VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    Visit(best_match, builder().VariantValue(t, to), from, EmitCopyAssignTag{});
  }
}

void Compiler::Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg() == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  Visit(t, to.reg(), EmitDestroyTag{});

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type = builder().Load<type::Type const *>(
        builder().VariantType(from->get<ir::Reg>()));
    auto *landing = builder().AddBlock();
    auto var_val  = builder().VariantValue(from_var_type, from->get<ir::Reg>());
    for (type::Type const *v : from_var_type->variants_) {
      auto *next_block         = builder().AddBlock();
      builder().CurrentBlock() = builder().EarlyExitOn<false>(
          next_block, builder().Eq(actual_type, v));
      builder().Store(v, builder().VariantType(to));
      Visit(v, builder().VariantValue(t, to),
            type::Typed{ir::Value(builder().PtrFix(var_val, v)), v},
            EmitMoveAssignTag{});
      builder().UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    builder().UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    builder().Store(from.type(), builder().VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    Visit(best_match, builder().VariantValue(t, to), from, EmitMoveAssignTag{});
  }
}

void Compiler::Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitCopyAssignTag) {
  t->copy_assign_func_.init([=]() { return CreateAssign<Copy>(this, t); });
  builder().Copy(t, from->get<ir::Reg>(), to);
}

void Compiler::Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Value> const &from, EmitMoveAssignTag) {
  t->move_assign_func_.init([=]() { return CreateAssign<Move>(this, t); });
  builder().Move(t, from->get<ir::Reg>(), to);
}

}  // namespace compiler
