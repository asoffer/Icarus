#include "absl/random/random.h"
#include "ast/ast.h"
#include "base/permutation.h"
#include "compiler/compiler.h"
#include "compiler/special_function.h"
#include "ir/builder.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/store.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "type/primitive.h"

namespace compiler {

template <SpecialFunctionCategory Cat>
static ir::CompiledFn *CreateAssign(Compiler *compiler, type::Array const *a) {
  type::Pointer const *ptr_type = type::Ptr(a);
  auto *data_ptr_type           = type::Ptr(a->data_type);
  auto fn_type                  = type::Func({ptr_type, ptr_type}, {});
  auto *fn = compiler->AddFunc(fn_type, fn_type->AnonymousFnParams());
  ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
    compiler->builder().CurrentBlock() = fn->entry();
    auto val                           = ir::Reg::Arg(0);
    auto var                           = ir::Reg::Arg(1);

    auto from_ptr     = ir::Index(ptr_type, val, 0);
    auto from_end_ptr = ir::PtrIncr(from_ptr, a->len, data_ptr_type);
    auto to_ptr       = ir::Index(ptr_type, var, 0);

    ir::CreateLoop(
        [&](ir::RegOr<ir::Addr> const &phi, ir::RegOr<ir::Addr> const &) {
          return ir::Eq(phi, from_end_ptr);
        },
        [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<ir::Addr> const &phi1) {
          ASSERT(phi0.is_reg() == true);
          ASSERT(phi1.is_reg() == true);

          auto from_val = ir::Results{PtrFix(phi0.reg(), a->data_type)};

          if constexpr (Cat == Copy) {
            compiler->Visit(a->data_type, phi1.reg(),
                            type::Typed{from_val, a->data_type},
                            EmitCopyAssignTag{});
          } else if constexpr (Cat == Move) {
            compiler->Visit(a->data_type, phi1.reg(),
                            type::Typed{from_val, a->data_type},
                            EmitMoveAssignTag{});
          } else {
            UNREACHABLE();
          }

          return std::tuple{ir::PtrIncr(phi0.reg(), 1, data_ptr_type),
                            ir::PtrIncr(phi1.reg(), 1, data_ptr_type)};
        },
        std::tuple{data_ptr_type, data_ptr_type},
        std::tuple{ir::RegOr<ir::Addr>(from_ptr), ir::RegOr<ir::Addr>(to_ptr)});
    ir::ReturnJump();
  }
  return fn;
}

template <SpecialFunctionCategory Cat>
static ir::AnyFunc CreateAssign(Compiler *compiler, type::Struct const *s) {
  if (auto fn = SpecialFunction(compiler, s, Name<Cat>())) { return *fn; }
  type::Pointer const *pt = type::Ptr(s);
  auto fn_type            = type::Func({pt, pt}, {});
  ir::AnyFunc fn = compiler->AddFunc(fn_type, fn_type->AnonymousFnParams());
  ICARUS_SCOPE(ir::SetCurrentFunc(fn.func())) {
    compiler->builder().CurrentBlock() = fn.func()->entry();
    auto val                           = ir::Reg::Arg(0);
    auto var                           = ir::Reg::Arg(1);

    for (size_t i = 0; i < s->fields_.size(); ++i) {
      auto *field_type = s->fields_.at(i).type;
      auto from =
          ir::Results{ir::PtrFix(ir::Field(val, s, i).get(), field_type)};
      auto to = ir::Field(var, s, i).get();

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

    ir::ReturnJump();
  }
  return fn;
}

void Compiler::Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  t->copy_assign_func_.init([=]() { return CreateAssign<Copy>(this, t); });
  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void Compiler::Visit(type::Array const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  t->move_assign_func_.init([=]() { return CreateAssign<Move>(this, t); });
  ir::Move(t, from->get<ir::Reg>(0), to);
}

void Compiler::Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::EnumVal>(0), to);
}

void Compiler::Visit(type::Enum const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::FlagsVal>(0), to);
}

void Compiler::Visit(type::Flags const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  ir::Store(from->get<ir::AnyFunc>(0), to);
}

void Compiler::Visit(type::Function const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  if (t == from.type()) {
    ir::Store(from->get<ir::Addr>(0), to);
  } else if (from.type() == type::NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE();
  }
}

void Compiler::Visit(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  ASSERT(t == from.type());
  switch (t->type_) {
    case type::BasicType::Type_:
      ir::Store(from->get<type::Type const *>(0), to);
      break;
    case type::BasicType::NullPtr: UNREACHABLE();
    case type::BasicType::EmptyArray: UNREACHABLE();
    case type::BasicType::Bool: ir::Store(from->get<bool>(0), to); break;
    case type::BasicType::Int8: ir::Store(from->get<int8_t>(0), to); break;
    case type::BasicType::Int16: ir::Store(from->get<int16_t>(0), to); break;
    case type::BasicType::Int32: ir::Store(from->get<int32_t>(0), to); break;
    case type::BasicType::Int64: ir::Store(from->get<int64_t>(0), to); break;
    case type::BasicType::Nat8: ir::Store(from->get<uint8_t>(0), to); break;
    case type::BasicType::Nat16: ir::Store(from->get<uint16_t>(0), to); break;
    case type::BasicType::Nat32: ir::Store(from->get<uint32_t>(0), to); break;
    case type::BasicType::Nat64: ir::Store(from->get<uint64_t>(0), to); break;
    case type::BasicType::Float32: ir::Store(from->get<float>(0), to); break;
    case type::BasicType::Float64: ir::Store(from->get<double>(0), to); break;
    default: UNREACHABLE();
  }
}

void Compiler::Visit(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  Visit(t, to, from, EmitCopyAssignTag{});
}

void Compiler::Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  t->copy_assign_func_.init([=]() {
    type::Pointer const *p = type::Ptr(t);
    auto fn_type           = type::Func({p, p}, {});
    auto *fn               = AddFunc(fn_type, fn_type->AnonymousFnParams());
    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        Visit(entry, ir::Field(var, t, i).get(),
              type::Typed{
                  ir::Results{ir::PtrFix(ir::Field(val, t, i).get(), entry)},
                  entry},
              EmitCopyAssignTag{});
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void Compiler::Visit(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  t->move_assign_func_.init([=]() {
    type::Pointer const *p = type::Ptr(t);
    auto fn_type           = type::Func({p, p}, {});
    auto *fn               = AddFunc(fn_type, fn_type->AnonymousFnParams());
    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = fn->entry();
      auto val                 = ir::Reg::Arg(0);
      auto var                 = ir::Reg::Arg(1);

      for (size_t i :
           base::make_random_permutation(absl::BitGen{}, t->entries_.size())) {
        auto *entry = t->entries_.at(i);
        Visit(entry, ir::Field(var, t, i).get(),
              type::Typed{
                  ir::Results{ir::PtrFix(ir::Field(val, t, i).get(), entry)},
                  entry},
              EmitMoveAssignTag{});
      }

      ir::ReturnJump();
    }
    return fn;
  });

  ir::Move(t, from->get<ir::Reg>(0), to);
}

void Compiler::Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg() == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  Visit(t, to.reg(), EmitDestroyTag{});

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type =
        ir::Load<type::Type const *>(ir::VariantType(from->get<ir::Reg>(0)));
    auto *landing = builder().AddBlock();
    auto var_val  = ir::VariantValue(from_var_type, from->get<ir::Reg>(0));
    for (type::Type const *v : from_var_type->variants_) {
      auto *next_block = builder().AddBlock();
      builder().CurrentBlock() =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      Visit(v, ir::VariantValue(t, to),
            type::Typed{ir::Results{ir::PtrFix(var_val, v)}, v},
            EmitCopyAssignTag{});
      ir::UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    ir::UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    ir::Store(from.type(), ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    Visit(best_match, ir::VariantValue(t, to), from, EmitCopyAssignTag{});
  }
}

void Compiler::Visit(type::Variant const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  // TODO full destruction is only necessary if the type is changing.
  ASSERT(to.is_reg() == true);
  // TODO have EmitDestroy take RegistorOr<Addr>
  Visit(t, to.reg(), EmitDestroyTag{});

  if (type::Variant const *from_var_type =
          from.type()->if_as<type::Variant>()) {
    auto actual_type =
        ir::Load<type::Type const *>(ir::VariantType(from->get<ir::Reg>(0)));
    auto *landing = builder().AddBlock();
    auto var_val  = ir::VariantValue(from_var_type, from->get<ir::Reg>(0));
    for (type::Type const *v : from_var_type->variants_) {
      auto *next_block = builder().AddBlock();
      builder().CurrentBlock() =
          ir::EarlyExitOn<false>(next_block, ir::Eq(actual_type, v));
      ir::Store(v, ir::VariantType(to));
      Visit(v, ir::VariantValue(t, to),
            type::Typed{ir::Results{ir::PtrFix(var_val, v)}, v},
            EmitMoveAssignTag{});
      ir::UncondJump(landing);
      builder().CurrentBlock() = next_block;
    }
    ir::UncondJump(landing);
    builder().CurrentBlock() = landing;
  } else {
    ir::Store(from.type(), ir::VariantType(to));
    // TODO Find the best match amongst the variants available.
    type::Type const *best_match = from.type();
    Visit(best_match, ir::VariantValue(t, to), from, EmitMoveAssignTag{});
  }
}

void Compiler::Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitCopyAssignTag) {
  t->copy_assign_func_.init([=]() { return CreateAssign<Copy>(this, t); });
  ir::Copy(t, from->get<ir::Reg>(0), to);
}

void Compiler::Visit(type::Struct const *t, ir::RegOr<ir::Addr> to,
                     type::Typed<ir::Results> const &from, EmitMoveAssignTag) {
  t->move_assign_func_.init([=]() { return CreateAssign<Move>(this, t); });
  ir::Move(t, from->get<ir::Reg>(0), to);
}

}  // namespace compiler
