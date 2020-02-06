#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "module/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type.h"

namespace compiler {

void Compiler::Visit(type::Array const *a, ir::Results const &val,
                     EmitPrintTag) {
  a->repr_func_.init([=]() {
    // TODO special function?
    auto const *fn_type = type::Func(
        core::FnParams<type::Type const *>{core::AnonymousParam(a)}, {});
    ir::CompiledFn *fn  = AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      builder().CurrentBlock() = fn->entry();

      builder().Print("[");
      auto *data_ptr_type = type::Ptr(a->data_type);
      auto ptr     = builder().PtrIncr(ir::Reg::Arg(0), 0, data_ptr_type);
      auto end_ptr = builder().PtrIncr(ptr, a->len, data_ptr_type);

      auto *loop_body  = builder().AddBlock();
      auto *land_block = builder().AddBlock();
      auto *cond_block = builder().AddBlock();

      builder().UncondJump(cond_block);

      builder().CurrentBlock() = cond_block;
      auto *phi                = builder().PhiInst<ir::Addr>();
      auto *sep                = builder().PhiInst<std::string_view>();
      builder().CondJump(
          builder().Eq(ir::RegOr<ir::Addr>(phi->result), end_ptr), land_block,
          loop_body);

      builder().CurrentBlock() = loop_body;
      builder().Print(ir::RegOr<std::string_view>(sep->result));
      Visit(a->data_type, ir::Results{ir::PtrFix(phi->result, a->data_type)},
            EmitPrintTag{});

      ir::Reg next = builder().PtrIncr(phi->result, 1, data_ptr_type);
      builder().UncondJump(cond_block);

      phi->add(fn->entry(), ptr);
      phi->add(builder().CurrentBlock(), next);
      sep->add(fn->entry(), std::string_view(""));
      sep->add(builder().CurrentBlock(), std::string_view(", "));

      builder().CurrentBlock() = land_block;

      builder().Print("]");
      builder().ReturnJump();
    }
    fn->WriteByteCode();
    return fn;
  });

  builder().Call(ir::AnyFunc{a->repr_func_.get()}, a->repr_func_.get()->type_,
                 {val}, ir::OutParams());
}

void Compiler::Visit(type::Enum const *t, ir::Results const &val,
                     EmitPrintTag) {
  // TODO print something friendlier
  builder().Print(val.get<ir::EnumVal>(0), t);
}

void Compiler::Visit(type::Flags const *t, ir::Results const &val,
                     EmitPrintTag) {
  // TODO print something friendlier
  builder().Print(val.get<ir::FlagsVal>(0), t);
}

void Compiler::Visit(type::Pointer const *t, ir::Results const &val,
                     EmitPrintTag) {
  builder().Print(val.get<ir::Addr>(0));
}

void Compiler::Visit(type::Primitive const *t, ir::Results const &val,
                     EmitPrintTag) {
  switch (t->type_) {
    case type::BasicType::Bool: builder().Print(val.get<bool>(0)); break;
    case type::BasicType::Int8: builder().Print(val.get<int8_t>(0)); break;
    case type::BasicType::Int16: builder().Print(val.get<int16_t>(0)); break;
    case type::BasicType::Int32: builder().Print(val.get<int32_t>(0)); break;
    case type::BasicType::Int64: builder().Print(val.get<int64_t>(0)); break;
    case type::BasicType::Nat8: builder().Print(val.get<uint8_t>(0)); break;
    case type::BasicType::Nat16: builder().Print(val.get<uint16_t>(0)); break;
    case type::BasicType::Nat32: builder().Print(val.get<uint32_t>(0)); break;
    case type::BasicType::Nat64: builder().Print(val.get<uint64_t>(0)); break;
    case type::BasicType::Float32: builder().Print(val.get<float>(0)); break;
    case type::BasicType::Float64: builder().Print(val.get<double>(0)); break;
    case type::BasicType::Type_:
      builder().Print(val.get<type::Type const *>(0));
      break;
    case type::BasicType::Scope:
    case type::BasicType::NullPtr:
    case type::BasicType::EmptyArray:
    case type::BasicType::Module:
    case type::BasicType::Label:
    case type::BasicType::Block: UNREACHABLE();
    case type::BasicType::ByteView:
      builder().Print(val.get<std::string_view>(0));
      break;
  }
}

void Compiler::Visit(type::Tuple const *t, ir::Results const &val,
                     EmitPrintTag) {
  auto reg = val.get<ir::Reg>(0);
  builder().Print("(");
  for (int i = 0; i < static_cast<int>(t->entries_.size()) - 1; ++i) {
    Visit(t->entries_[i],
          ir::Results{
              ir::PtrFix(builder().Field(reg, t, i).get(), t->entries_[i])},
          EmitPrintTag{});
    builder().Print(", ");
  }

  if (not t->entries_.empty()) {
    Visit(t->entries_.back(),
          ir::Results{
              ir::PtrFix(builder().Field(reg, t, t->entries_.size() - 1).get(),
                         t->entries_.back())},
          EmitPrintTag{});
  }
  builder().Print(")");
}

void Compiler::Visit(type::Variant const *t, ir::Results const &val,
                     EmitPrintTag) {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(t->mtx_);
  if (not t->repr_func_) {
    auto const *fn_type = type::Func(
        core::FnParams<type::Type const *>{core::AnonymousParam(t)}, {});
    t->repr_func_       = AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrent(t->repr_func_)) {
      builder().CurrentBlock() = t->repr_func_->entry();
      auto *landing            = builder().AddBlock();
      auto type =
          ir::Load<type::Type const *>(builder().VariantType(ir::Reg::Arg(0)));

      auto var_val = builder().VariantValue(t, ir::Reg::Arg(0));
      for (type::Type const *v : t->variants_) {
        auto old_block    = builder().CurrentBlock();
        auto *found_block = builder().AddBlock();

        builder().CurrentBlock() = found_block;
        Visit(v, ir::Results{ir::PtrFix(var_val, v)}, EmitPrintTag{});
        builder().UncondJump(landing);

        builder().CurrentBlock() = old_block;
        builder().CurrentBlock() = ir::EarlyExitOn<true>(
            found_block, builder().Eq(ir::RegOr<type::Type const *>(type), v));
      }

      builder().UncondJump(landing);
      builder().CurrentBlock() = landing;
      builder().ReturnJump();
    }
  }

  builder().Call(ir::AnyFunc{t->repr_func_}, t->repr_func_->type_, {val},
                 ir::OutParams());
}

}  // namespace compiler
