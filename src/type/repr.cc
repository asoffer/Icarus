#include "type/all.h"

#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

namespace type {
void Primitive::EmitRepr(ir::Val const &val, Context *ctx) const {
  switch (type_) {
    case PrimType::Bool: ir::Print(val.reg_or<bool>()); break;
    case PrimType::Int8: ir::Print(val.reg_or<i8>()); break;
    case PrimType::Int16: ir::Print(val.reg_or<i16>()); break;
    case PrimType::Int32: ir::Print(val.reg_or<i32>()); break;
    case PrimType::Int64: ir::Print(val.reg_or<i64>()); break;
    case PrimType::Nat8: ir::Print(val.reg_or<u8>()); break;
    case PrimType::Nat16: ir::Print(val.reg_or<u16>()); break;
    case PrimType::Nat32: ir::Print(val.reg_or<u32>()); break;
    case PrimType::Nat64: ir::Print(val.reg_or<u64>()); break;
    case PrimType::Float32: ir::Print(val.reg_or<float>()); break;
    case PrimType::Float64: ir::Print(val.reg_or<double>()); break;
    case PrimType::Type_: ir::Print(val.reg_or<type::Type const *>()); break;
    case PrimType::Scope:
    case PrimType::StatefulScope:
    case PrimType::NullPtr:
    case PrimType::EmptyArray:
    case PrimType::Module:
    case PrimType::Interface:
    case PrimType::Block:
    case PrimType::OptBlock:
    case PrimType::RepBlock: UNREACHABLE();
  }
}

void Array::EmitRepr(ir::Val const &val, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"arg", nullptr}});

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

// TODO print something friendlier
void Pointer::EmitRepr(ir::Val const &val, Context *ctx) const {
  ir::Print(val.reg_or<ir::Addr>());
}
void Enum::EmitRepr(ir::Val const &val, Context *ctx) const {
  ir::Print(val.reg_or<ir::EnumVal>(), this);
}
void Flags::EmitRepr(ir::Val const &val, Context *ctx) const {
  ir::Print(val.reg_or<ir::FlagsVal>(), this);
}
void Variant::EmitRepr(ir::Val const &id_val, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"arg", nullptr}});
    CURRENT_FUNC(repr_func_) {
      ir::BasicBlock::Current = repr_func_->entry();
      auto landing            = ir::Func::Current->AddBlock();
      auto type               = ir::Load<type::Type const *>(
          ir::VariantType(repr_func_->Argument(0)));

      for (const Type *v : variants_) {
        auto old_block   = ir::BasicBlock::Current;
        auto found_block = ir::Func::Current->AddBlock();

        ir::BasicBlock::Current = found_block;
        v->EmitRepr(
            ir::Val::Reg(
                ir::PtrFix(ir::VariantValue(v, repr_func_->Argument(0)), v), v),
            ctx);
        ir::UncondJump(landing);

        ir::BasicBlock::Current = old_block;
        ir::BasicBlock::Current = ir::EarlyExitOn<true>(
            found_block, ir::Eq(ir::RegisterOr<type::Type const *>(type), v));
      }

      ir::UncondJump(landing);
      ir::BasicBlock::Current = landing;
      ir::ReturnJump();
    }
  }

  ir::Arguments call_args;
  call_args.append(id_val);
  call_args.type_ = repr_func_->type_;
  ir::Call(ir::AnyFunc{repr_func_}, std::move(call_args));
}

void Function::EmitRepr(ir::Val const &, Context *ctx) const { UNREACHABLE(); }
void Struct::EmitRepr(ir::Val const &val, Context *ctx) const { UNREACHABLE(); }
void GenericStruct::EmitRepr(ir::Val const &val, Context *ctx) const { UNREACHABLE(); }

void CharBuffer::EmitRepr(ir::Val const &val, Context *ctx) const {
  ir::Print(val.reg_or<std::string_view>());
}
}  // namespace type
