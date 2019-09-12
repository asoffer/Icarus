#include "ir/builder.h"
#include "ir/results.h"
#include "ir/arguments.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/print.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type.h"

namespace visitor {

void TraditionalCompilation::EmitPrint(type::Array const *t,
                                       ir::Results const &val) {
  t->repr_func_.init([=]() {
    // TODO special function?
    ir::CompiledFn *fn = module()->AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));

    ICARUS_SCOPE(ir::SetCurrentFunc(fn)) {
      builder().CurrentBlock() = fn->entry();

      auto exit_block = builder().AddBlock();

      ir::Print(std::string_view{"["});

      builder().CurrentBlock() = ir::EarlyExitOn<true>(exit_block, t->len == 0);
      auto ptr                 = ir::Index(type::Ptr(t), ir::Reg::Arg(0), 0);

      t->data_type->EmitPrint(this, ir::Results{ir::PtrFix(ptr, t->data_type)});

      ir::CreateLoop(
          [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<int32_t> const &phi1) {
            return ir::Eq(phi1, 0);
          },
          [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<int32_t> const &phi1) {
            ASSERT(phi0.is_reg_ == true);
            auto elem_ptr = ir::PtrIncr(phi0.reg_, 1, type::Ptr(t->data_type));

            ir::Print(std::string_view{", "});
            t->data_type->EmitPrint(
                this, ir::Results{ir::PtrFix(elem_ptr, t->data_type)});

            return std::make_tuple(elem_ptr,
                                   ir::Sub(ir::RegOr<int32_t>(phi1), 1));
          },
          std::tuple{type::Ptr(t->data_type), type::Int32},
          std::tuple{ir::RegOr<ir::Addr>(ptr), ir::RegOr<int32_t>(t->len - 1)});
      ir::UncondJump(exit_block);

      builder().CurrentBlock() = exit_block;
      ir::Print(std::string_view{"]"});
      ir::ReturnJump();
    }

    return fn;
  });

  ir::Call(ir::AnyFunc{t->repr_func_.get()}, t->repr_func_.get()->type_, {val});
}

void TraditionalCompilation::EmitPrint(type::Enum const *t,
                                       ir::Results const &val) {
  // TODO print something friendlier
  ir::Print(val.get<ir::EnumVal>(0), t);
}

void TraditionalCompilation::EmitPrint(type::Flags const *t,
                                       ir::Results const &val) {
  // TODO print something friendlier
  ir::Print(val.get<ir::FlagsVal>(0), t);
}

void TraditionalCompilation::EmitPrint(type::Pointer const *t,
                                       ir::Results const &val) {
  ir::Print(val.get<ir::Addr>(0));
}

void TraditionalCompilation::EmitPrint(type::Primitive const *t,
                                       ir::Results const &val) {
  switch (t->type_) {
    case type::PrimType::Bool: ir::Print(val.get<bool>(0)); break;
    case type::PrimType::Int8: ir::Print(val.get<int8_t>(0)); break;
    case type::PrimType::Int16: ir::Print(val.get<int16_t>(0)); break;
    case type::PrimType::Int32: ir::Print(val.get<int32_t>(0)); break;
    case type::PrimType::Int64: ir::Print(val.get<int64_t>(0)); break;
    case type::PrimType::Nat8: ir::Print(val.get<uint8_t>(0)); break;
    case type::PrimType::Nat16: ir::Print(val.get<uint16_t>(0)); break;
    case type::PrimType::Nat32: ir::Print(val.get<uint32_t>(0)); break;
    case type::PrimType::Nat64: ir::Print(val.get<uint64_t>(0)); break;
    case type::PrimType::Float32: ir::Print(val.get<float>(0)); break;
    case type::PrimType::Float64: ir::Print(val.get<double>(0)); break;
    case type::PrimType::Type_:
      ir::Print(val.get<type::Type const *>(0));
      break;
    case type::PrimType::Ctx:
    case type::PrimType::Scope:
    case type::PrimType::NullPtr:
    case type::PrimType::EmptyArray:
    case type::PrimType::Module:
    case type::PrimType::Block: UNREACHABLE();
    case type::PrimType::ByteView:
      ir::Print(val.get<std::string_view>(0));
      break;
  }
}

void TraditionalCompilation::EmitPrint(type::Tuple const *t,
                                       ir::Results const &val) {
  auto reg = val.get<ir::Reg>(0);
  ir::Print(std::string_view{"("});
  for (int i = 0; i < static_cast<int>(t->entries_.size()) - 1; ++i) {
    t->entries_[i]->EmitPrint(
        this,
        ir::Results{ir::PtrFix(ir::Field(reg, t, i).get(), t->entries_[i])});
    ir::Print(std::string_view{", "});
  }

  if (!t->entries_.empty()) {
    t->entries_.back()->EmitPrint(
        this,
        ir::Results{ir::PtrFix(ir::Field(reg, t, t->entries_.size() - 1).get(),
                               t->entries_.back())});
  }
  ir::Print(std::string_view{")"});
}

void TraditionalCompilation::EmitPrint(type::Variant const *t,
                                       ir::Results const &val) {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(t->mtx_);
  if (!t->repr_func_) {
    t->repr_func_ = module()->AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));

    ICARUS_SCOPE(ir::SetCurrentFunc(t->repr_func_)) {
      builder().CurrentBlock() = t->repr_func_->entry();
      auto landing             = builder().AddBlock();
      auto type =
          ir::Load<type::Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      auto var_val = ir::VariantValue(t, ir::Reg::Arg(0));
      for (type::Type const *v : t->variants_) {
        auto old_block   = builder().CurrentBlock();
        auto found_block = builder().AddBlock();

        builder().CurrentBlock() = found_block;
        v->EmitPrint(this, ir::Results{ir::PtrFix(var_val, v)});
        ir::UncondJump(landing);

        builder().CurrentBlock() = old_block;
        builder().CurrentBlock() = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      builder().CurrentBlock() = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{t->repr_func_}, t->repr_func_->type_, {val});
}

}  // namespace visitor
