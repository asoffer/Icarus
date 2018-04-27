#include "all.h"

#include "../context.h"
#include "../ir/func.h"

namespace type {
void Primitive::EmitRepr(IR::Val val, Context *ctx) const {
  switch (type_) {
  case PrimType::Char: {
    if (!repr_func_) {
      repr_func_ = ctx->mod_->AddFunc(
          Func({this}, {}),
          std::vector<std::pair<std::string, AST::Expression *>>{
              {"arg", nullptr}});

      CURRENT_FUNC(repr_func_) {
        IR::Block::Current = repr_func_->entry();

        IR::Print(IR::Val::Char('`'));

        for (auto[c, rep] : {std::pair('\a', 'a'), std::pair('\b', 'b'),
                             std::pair('\n', 'n'), std::pair('\r', 'r'),
                             std::pair('\t', 't'), std::pair('\v', 'v')}) {
          auto special_block = repr_func_->AddBlock();
          auto next_block    = repr_func_->AddBlock();

          IR::CondJump(IR::Eq(repr_func_->Argument(0), IR::Val::Char(c)),
                       special_block, next_block);

          IR::Block::Current = special_block;
          IR::Print(IR::Val::Char('\\'));
          IR::Print(IR::Val::Char(rep));
          IR::ReturnJump();

          IR::Block::Current = next_block;
        }

        IR::Print(repr_func_->Argument(0));
        IR::ReturnJump();
      }
    }

    IR::Call(IR::Val::Func(repr_func_), std::vector<IR::Val>{val}, {});
  } break;

  case PrimType::Bool:
  case PrimType::Int:
  case PrimType::Real:
  case PrimType::Type:
  case PrimType::String:
  case PrimType::Code: IR::Print(val); break;
  case PrimType::Void:
  case PrimType::NullPtr:
  case PrimType::EmptyArray:
  case PrimType::Generic:
  case PrimType::Module:
  case PrimType::Err: NOT_YET();
  }
}

void Array::EmitRepr(IR::Val val, Context *ctx) const {
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(repr_func_) {
      IR::Block::Current = repr_func_->entry();

      auto init_block = repr_func_->AddBlock();
      auto exit_block = repr_func_->AddBlock();

      IR::Print(IR::Val::Char('['));

      auto length_var = fixed_length
                            ? IR::Val::Int(static_cast<i32>(len))
                            : IR::Load(IR::ArrayLength(repr_func_->Argument(0)));
      IR::CondJump(IR::Eq(length_var, IR::Val::Int(0)), exit_block,
                   init_block);

      IR::Block::Current = init_block;
      auto ptr           = IR::Index(repr_func_->Argument(0), IR::Val::Int(0));
      auto end_ptr       = IR::PtrIncr(ptr, length_var);

      auto loop_phi  = repr_func_->AddBlock();
      auto loop_body = repr_func_->AddBlock();

      data_type->EmitRepr(PtrCallFix(ptr), ctx);
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      auto elem_ptr      = IR::PtrIncr(phi_reg, IR::Val::Int(1));
      IR::CondJump(IR::Eq(elem_ptr, end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      IR::Print(IR::Val::Char(','));
      IR::Print(IR::Val::Char(' '));
      data_type->EmitRepr(PtrCallFix(elem_ptr), ctx);
      IR::UncondJump(loop_phi);

      IR::Func::Current->SetArgs(phi, {IR::Val::Block(init_block), ptr,
                                       IR::Val::Block(IR::Block::Current),
                                       elem_ptr});

      IR::Block::Current = exit_block;
      IR::Print(IR::Val::Char(']'));
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(repr_func_), std::vector<IR::Val>{val}, {});
}

// TODO print something friendlier
void Pointer::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(val); }
void Enum::EmitRepr(IR::Val val, Context *ctx) const { IR::Print(val); }
void Range::EmitRepr(IR::Val, Context *ctx) const { NOT_YET(); }
void Slice::EmitRepr(IR::Val, Context *ctx) const { NOT_YET(); }
void Scope::EmitRepr(IR::Val, Context *ctx) const { NOT_YET(); }
void Variant::EmitRepr(IR::Val id_val, Context *ctx) const {
  // TODO design and build a jump table?
  // TODO repr_func_
  // TODO remove these casts in favor of something easier to track properties on

  auto landing = IR::Func::Current->AddBlock();
  auto type    = IR::Load(IR::VariantType(id_val));
  for (const Type *v : variants_) {
    auto old_block   = IR::Block::Current;
    auto found_block = IR::Func::Current->AddBlock();

    IR::Block::Current = found_block;
    v->EmitRepr(PtrCallFix(IR::VariantValue(v, id_val)),ctx);
    IR::UncondJump(landing);

    IR::Block::Current = old_block;
    IR::Block::Current =
        IR::EarlyExitOn<true>(found_block, IR::Eq(type, IR::Val::Type(v)));
  }
  IR::UncondJump(landing);
  IR::Block::Current = landing;
}
void Function::EmitRepr(IR::Val, Context *ctx) const {
  IR::Print(IR::Val::Char('{'));
  IR::Print(IR::Val::Type(this));
  IR::Print(IR::Val::Char('}'));
}
void Struct::EmitRepr(IR::Val val, Context *ctx) const {
  if (!repr_func_) {
    repr_func_ = ctx->mod_->AddFunc(
        Func({this}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(repr_func_) {
      IR::Block::Current = repr_func_->entry();

      IR::Print(IR::Val::Char('{'));
      IR::Print(IR::Val::Char(' '));

      for (size_t i = 0; i < fields_.size(); ++i) {
        fields_[i].type->EmitRepr(
            PtrCallFix(IR::Field(repr_func_->Argument(0), i)), ctx);
        IR::Print(IR::Val::Char(' '));
      }
      IR::Print(IR::Val::Char('}'));
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(repr_func_), std::vector<IR::Val>{val}, {});
}

} // namespace type
