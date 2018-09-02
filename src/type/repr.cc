#include "type/all.h"

#include "context.h"
#include "ir/func.h"
#include "module.h"

namespace type {
void Primitive::EmitRepr(IR::Val val, Context *ctx) const {
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

            IR::CondJump(IR::Eq(repr_func_->Argument(0), IR::Val::Char(c)),
                         special_block, next_block);

            IR::BasicBlock::Current = special_block;
            IR::PrintChar('\\');
            IR::PrintChar(rep);
            IR::ReturnJump();

            IR::BasicBlock::Current = next_block;
          }

          IR::Print(repr_func_->Argument(0));
          IR::ReturnJump();
        }
      }

      IR::LongArgs call_args;
      call_args.append(val);
      IR::Call(IR::Val::Func(repr_func_), std::move(call_args));
    } break;

    case PrimType::Bool:
    case PrimType::Int:
    case PrimType::Real:
    case PrimType::Type:
    case PrimType::Code: IR::Print(val); break;
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

void Array::EmitRepr(IR::Val val, Context *ctx) const {
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
        return IR::LoadInt(IR::ArrayLength(
            std::get<IR::Register>(repr_func_->Argument(0).value)));
      }();
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          exit_block, IR::ValFrom(IR::EqInt(length_var, 0)));
      auto ptr = IR::Index(repr_func_->Argument(0), IR::Val::Int(0));

      data_type->EmitRepr(PtrCallFix(ptr), ctx);

      CreateLoop({ptr, IR::ValFrom(IR::SubInt(length_var, 1))},
                 [&](const base::vector<IR::Val> &phis) {
                   return IR::Eq(phis[1], IR::Val::Int(0));
                 },
                 [&](const base::vector<IR::Val> &phis) {
                   auto elem_ptr = IR::PtrIncr(phis[0], IR::Val::Int(1));

                   IR::PrintChar(',');
                   IR::PrintChar(' ');
                   data_type->EmitRepr(PtrCallFix(elem_ptr), ctx);

                   return base::vector<IR::Val>{
                       elem_ptr, IR::Sub(phis[1], IR::Val::Int(1))};
                 });
      IR::UncondJump(exit_block);

      IR::BasicBlock::Current = exit_block;
      IR::PrintChar(']');
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(val);
  IR::Call(IR::Val::Func(repr_func_), std::move(call_args));
}

// TODO print something friendlier
void Pointer::EmitRepr(IR::Val val, Context *ctx) const {
  IR::PrintAddr(val.reg_or<IR::Addr>());
}
void Enum::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(val); }
void Flags::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(val); }
void Scope::EmitRepr(IR::Val, Context *ctx) const { NOT_YET(); }
void Variant::EmitRepr(IR::Val id_val, Context *ctx) const {
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
      auto type               = IR::LoadType(IR::VariantType(
          std::get<IR::Register>(repr_func_->Argument(0).value)));

      for (const Type *v : variants_) {
        auto old_block   = IR::BasicBlock::Current;
        auto found_block = IR::Func::Current->AddBlock();

        IR::BasicBlock::Current = found_block;
        v->EmitRepr(PtrCallFix(IR::Val::Reg(
                        IR::VariantValue(v, std::get<IR::Register>(
                                                repr_func_->Argument(0).value)),
                        type::Ptr(v))),
                    ctx);
        IR::UncondJump(landing);

        IR::BasicBlock::Current = old_block;
        IR::BasicBlock::Current = IR::EarlyExitOn<true>(
            found_block, IR::ValFrom(IR::EqType(type, v)));
      }

      IR::UncondJump(landing);
      IR::BasicBlock::Current = landing;
      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(id_val);
  IR::Call(IR::Val::Func(repr_func_), std::move(call_args));
}

void Function::EmitRepr(IR::Val, Context *ctx) const { UNREACHABLE(); }
void Struct::EmitRepr(IR::Val val, Context *ctx) const { UNREACHABLE(); }

void CharBuffer::EmitRepr(IR::Val val, Context *ctx) const {
  IR::Print(std::move(val));
}
}  // namespace type
