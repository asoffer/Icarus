#include "type/all.h"

#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

namespace type {
void Primitive::EmitRepr(IR::Val const &val, Context *ctx) const {
  switch (type_) {
    case PrimType::Char: {
      std::unique_lock lock(mtx_);
      if (!repr_func_) {
        repr_func_ = ctx->mod_->AddFunc(
            Func({this}, {}),
            base::vector<std::pair<std::string, AST::Expression *>>{
                {"arg", nullptr}});

        CURRENT_FUNC(repr_func_) {
          IR::BasicBlock::Current = repr_func_->entry();

          IR::PrintChar('`');

          for (auto[c, rep] : {std::pair('\a', 'a'), std::pair('\b', 'b'),
                               std::pair('\n', 'n'), std::pair('\r', 'r'),
                               std::pair('\t', 't'), std::pair('\v', 'v')}) {
            auto special_block = repr_func_->AddBlock();
            auto next_block    = repr_func_->AddBlock();

            IR::CondJump(IR::EqChar(repr_func_->Argument(0), c), special_block,
                         next_block);

            IR::BasicBlock::Current = special_block;
            IR::PrintChar('\\');
            IR::PrintChar(rep);
            IR::ReturnJump();

            IR::BasicBlock::Current = next_block;
          }

          IR::PrintChar(repr_func_->Argument(0));
          IR::ReturnJump();
        }
      }

      IR::LongArgs call_args;
      call_args.append(val);
      call_args.type_ = repr_func_->type_;
      IR::Call(IR::AnyFunc{repr_func_}, std::move(call_args));
    } break;

    case PrimType::Bool: IR::PrintBool(val.reg_or<bool>()); break;
    case PrimType::Int: IR::PrintInt(val.reg_or<int>()); break;
    case PrimType::Real: IR::PrintReal(val.reg_or<double>()); break;
    case PrimType::Type: IR::PrintType(val.reg_or<type::Type const *>()); break;
    case PrimType::Code: NOT_YET();
    case PrimType::NullPtr:
    case PrimType::EmptyArray:
    case PrimType::Generic:
    case PrimType::Module:
    case PrimType::Interface:
    case PrimType::Block:
    case PrimType::OptBlock:
    case PrimType::Err: NOT_YET();
  }
}

void Array::EmitRepr(IR::Val const &val, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(repr_func_) {
      IR::BasicBlock::Current = repr_func_->entry();

      auto exit_block = repr_func_->AddBlock();

      IR::PrintChar('[');

      auto length_var = [&]() -> IR::RegisterOr<i32> {
        if (fixed_length) { return static_cast<i32>(len); }
        return IR::LoadInt(IR::ArrayLength(repr_func_->Argument(0)));
      }();
      IR::BasicBlock::Current =
          IR::EarlyExitOn<true>(exit_block, IR::EqInt(length_var, 0));
      auto ptr = IR::Index(type::Ptr(this), repr_func_->Argument(0), 0);

      data_type->EmitRepr(
          PtrCallFix(IR::Val::Reg(ptr, type::Ptr(this->data_type))), ctx);

      using tup = std::tuple<IR::RegisterOr<IR::Addr>, IR::RegisterOr<i32>>;
      IR::CreateLoop(
          [&](tup const &phis) { return IR::EqInt(std::get<1>(phis), 0); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            auto elem_ptr = IR::PtrIncr(std::get<0>(phis).reg_, 1,
                                        type::Ptr(this->data_type));

            IR::PrintChar(',');
            IR::PrintChar(' ');
            data_type->EmitRepr(
                PtrCallFix(IR::Val::Reg(elem_ptr, type::Ptr(this->data_type))),
                ctx);

            return std::make_tuple(elem_ptr, IR::SubInt(std::get<1>(phis), 1));
          },
          std::tuple<type::Type const *, type::Type const *>{
              type::Ptr(this->data_type), type::Int},
          tup{ptr, IR::SubInt(length_var, 1)});
      IR::UncondJump(exit_block);

      IR::BasicBlock::Current = exit_block;
      IR::PrintChar(']');
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(val);
  call_args.type_ = repr_func_->type_;
  IR::Call(IR::AnyFunc{repr_func_}, std::move(call_args));
}

// TODO print something friendlier
void Pointer::EmitRepr(IR::Val const &val, Context *ctx) const {
  IR::PrintAddr(val.reg_or<IR::Addr>());
}
void Enum::EmitRepr(IR::Val const &val, Context *ctx) const {
  IR::PrintEnum(val.reg_or<IR::EnumVal>(), this);
}
void Flags::EmitRepr(IR::Val const &val, Context *ctx) const {
  IR::PrintFlags(val.reg_or<IR::FlagsVal>(), this);
}
void Scope::EmitRepr(IR::Val const &, Context *ctx) const { NOT_YET(); }
void Variant::EmitRepr(IR::Val const &id_val, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  std::unique_lock lock(mtx_);
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});
    CURRENT_FUNC(repr_func_) {
      IR::BasicBlock::Current = repr_func_->entry();
      auto landing            = IR::Func::Current->AddBlock();
      auto type = IR::LoadType(IR::VariantType(repr_func_->Argument(0)));

      for (const Type *v : variants_) {
        auto old_block   = IR::BasicBlock::Current;
        auto found_block = IR::Func::Current->AddBlock();

        IR::BasicBlock::Current = found_block;
        v->EmitRepr(
            PtrCallFix(IR::Val::Reg(
                IR::VariantValue(v, repr_func_->Argument(0)), type::Ptr(v))),
            ctx);
        IR::UncondJump(landing);

        IR::BasicBlock::Current = old_block;
        IR::BasicBlock::Current =
            IR::EarlyExitOn<true>(found_block, IR::EqType(type, v));
      }

      IR::UncondJump(landing);
      IR::BasicBlock::Current = landing;
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(id_val);
  call_args.type_ = repr_func_->type_;
  IR::Call(IR::AnyFunc{repr_func_}, std::move(call_args));
}

void Function::EmitRepr(IR::Val const &, Context *ctx) const { UNREACHABLE(); }
void Struct::EmitRepr(IR::Val const &val, Context *ctx) const { UNREACHABLE(); }

void CharBuffer::EmitRepr(IR::Val const &val, Context *ctx) const {
  IR::PrintCharBuffer(val.reg_or<std::string_view>());
}
}  // namespace type
