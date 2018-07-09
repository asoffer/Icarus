#include "all.h"

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

        IR::Print(IR::Val::Char('`'));

        for (auto[c, rep] : {std::pair('\a', 'a'), std::pair('\b', 'b'),
                             std::pair('\n', 'n'), std::pair('\r', 'r'),
                             std::pair('\t', 't'), std::pair('\v', 'v')}) {
          auto special_block = repr_func_->AddBlock();
          auto next_block    = repr_func_->AddBlock();

          IR::CondJump(IR::Eq(repr_func_->Argument(0), IR::Val::Char(c)),
                       special_block, next_block);

          IR::BasicBlock::Current = special_block;
          IR::Print(IR::Val::Char('\\'));
          IR::Print(IR::Val::Char(rep));
          IR::ReturnJump();

          IR::BasicBlock::Current = next_block;
        }

        IR::Print(repr_func_->Argument(0));
        IR::ReturnJump();
      }
    }

    IR::Call(IR::Val::Func(repr_func_), base::vector<IR::Val>{val}, {});
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

      IR::Print(IR::Val::Char('['));

      auto length_var = fixed_length
                            ? IR::Val::Int(static_cast<i32>(len))
                            : IR::Load(IR::ArrayLength(repr_func_->Argument(0)));
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          exit_block, IR::Eq(length_var, IR::Val::Int(0)));
      auto ptr = IR::Index(repr_func_->Argument(0), IR::Val::Int(0));

      data_type->EmitRepr(PtrCallFix(ptr), ctx);

      CreateLoop({ptr, IR::Sub(length_var, IR::Val::Int(1))},
                 [&](const base::vector<IR::Val> &phis) {
                   return IR::Eq(phis[1], IR::Val::Int(0));
                 },
                 [&](const base::vector<IR::Val> &phis) {
                   auto elem_ptr = IR::PtrIncr(phis[0], IR::Val::Int(1));

                   IR::Print(IR::Val::Char(','));
                   IR::Print(IR::Val::Char(' '));
                   data_type->EmitRepr(PtrCallFix(elem_ptr), ctx);

                   return base::vector<IR::Val>{
                       elem_ptr, IR::Sub(phis[1], IR::Val::Int(1))};
                 });
      IR::UncondJump(exit_block);

      IR::BasicBlock::Current = exit_block;
      IR::Print(IR::Val::Char(']'));
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(repr_func_), base::vector<IR::Val>{val}, {});
}

// TODO print something friendlier
void Pointer::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(val); }
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
      auto landing       = IR::Func::Current->AddBlock();
      auto type          = IR::Load(IR::VariantType(repr_func_->Argument(0)));

      for (const Type *v : variants_) {
        auto old_block   = IR::BasicBlock::Current;
        auto found_block = IR::Func::Current->AddBlock();

        IR::BasicBlock::Current = found_block;
        v->EmitRepr(PtrCallFix(IR::VariantValue(v, repr_func_->Argument(0))),
                    ctx);
        IR::UncondJump(landing);

        IR::BasicBlock::Current = old_block;
        IR::BasicBlock::Current =
            IR::EarlyExitOn<true>(found_block, IR::Eq(type, IR::Val::Type(v)));
      }

      IR::UncondJump(landing);
      IR::BasicBlock::Current = landing;
      IR::ReturnJump();
    }
  }

  IR::Call(IR::Val::Func(repr_func_), base::vector<IR::Val>{id_val}, {});
}

void Function::EmitRepr(IR::Val, Context *ctx) const { UNREACHABLE(); }
void Struct::EmitRepr(IR::Val val, Context *ctx) const { UNREACHABLE(); }

void CharBuffer::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(std::move(val)); }
} // namespace type
