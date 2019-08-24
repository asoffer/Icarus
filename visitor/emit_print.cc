#include "ir/arguments.h"
#include "ir/cmd.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/print.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/type.h"
#include "visitor/emit_ir.h"

namespace visitor {

void EmitIr::Print(type::Array const *t, ir::Results const &val, Context *ctx) {
  t->repr_func_.init([=]() {
    // TODO special function?
    ir::CompiledFn *fn = ctx->mod_->AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));

    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();

      auto exit_block = fn->AddBlock();

      ir::Print(std::string_view{"["});

      ir::BasicBlock::Current = ir::EarlyExitOn<true>(exit_block, t->len == 0);
      auto ptr                = ir::Index(type::Ptr(t), ir::Reg::Arg(0), 0);

      t->data_type->EmitPrint(this, ir::Results{ir::PtrFix(ptr, t->data_type)},
                              ctx);

      ir::CreateLoop(
          [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<int32_t> const &phi1) {
            return ir::Eq(phi1, 0);
          },
          [&](ir::RegOr<ir::Addr> const &phi0, ir::RegOr<int32_t> const &phi1) {
            ASSERT(phi0.is_reg_ == true);
            auto elem_ptr = ir::PtrIncr(phi0.reg_, 1, type::Ptr(t->data_type));

            ir::Print(std::string_view{", "});
            t->data_type->EmitPrint(
                this, ir::Results{ir::PtrFix(elem_ptr, t->data_type)}, ctx);

            return std::make_tuple(elem_ptr,
                                   ir::Sub(ir::RegOr<int32_t>(phi1), 1));
          },
          std::tuple{type::Ptr(t->data_type), type::Int32},
          std::tuple{ir::RegOr<ir::Addr>(ptr), ir::RegOr<int32_t>(t->len - 1)});
      ir::UncondJump(exit_block);

      ir::BasicBlock::Current = exit_block;
      ir::Print(std::string_view{"]"});
      ir::ReturnJump();
    }

    return fn;
  });

  ir::Call(ir::AnyFunc{t->repr_func_.get()},
           ir::Arguments{t->repr_func_.get()->type_, val});
}

void EmitIr::Print(type::Enum const *t, ir::Results const &val, Context *ctx) {
  // TODO print something friendlier
  ir::Print(val.get<ir::EnumVal>(0), t);
}

void EmitIr::Print(type::Flags const *t, ir::Results const &val, Context *ctx) {
  // TODO print something friendlier
  ir::Print(val.get<ir::FlagsVal>(0), t);
}

void EmitIr::Print(type::Pointer const *t, ir::Results const &val,
                   Context *ctx) {
  ir::Print(val.get<ir::Addr>(0));
}

void EmitIr::Print(type::Primitive const *t, ir::Results const &val,
                   Context *ctx) {
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

void EmitIr::Print(type::Tuple const *t, ir::Results const &val, Context *ctx) {
  auto reg = val.get<ir::Reg>(0);
  ir::Print(std::string_view{"("});
  for (int i = 0; i < static_cast<int>(t->entries_.size()) - 1; ++i) {
    t->entries_[i]->EmitPrint(
        this,
        ir::Results{ir::PtrFix(ir::Field(reg, t, i).get(), t->entries_[i])},
        ctx);
    ir::Print(std::string_view{", "});
  }

  if (!t->entries_.empty()) {
    t->entries_.back()->EmitPrint(
        this,
        ir::Results{ir::PtrFix(ir::Field(reg, t, t->entries_.size() - 1).get(),
                               t->entries_.back())},
        ctx);
  }
  ir::Print(std::string_view{")"});
}

void EmitIr::Print(type::Variant const *t, ir::Results const &val,
                   Context *ctx) {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(t->mtx_);
  if (!t->repr_func_) {
    t->repr_func_ = ctx->mod_->AddFunc(
        type::Func({t}, {}),
        core::FnParams(
            core::Param{"", type::Typed<ast::Expression const *>{nullptr, t}}));

    CURRENT_FUNC(t->repr_func_) {
      ir::BasicBlock::Current = t->repr_func_->entry();
      auto landing            = ir::CompiledFn::Current->AddBlock();
      auto type =
          ir::Load<type::Type const *>(ir::VariantType(ir::Reg::Arg(0)));

      for (type::Type const *v : t->variants_) {
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::CompiledFn::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitPrint(
            this,
            ir::Results{ir::PtrFix(ir::VariantValue(v, ir::Reg::Arg(0)), v)},
            ctx);
        ir::UncondJump(landing);

        ir::BasicBlock::Current = old_block;
        ir::BasicBlock::Current = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      ir::BasicBlock::Current = landing;
      ir::ReturnJump();
    }
  }

  ir::Call(ir::AnyFunc{t->repr_func_},
           ir::Arguments{t->repr_func_->type_, val});
}

}  // namespace visitor
