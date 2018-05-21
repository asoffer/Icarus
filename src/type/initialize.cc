#include "type/all.h"

#include "architecture.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"

namespace AST {
struct Expression;
}  // namespace AST

namespace type {
void Array::EmitInit(IR::Val id_val, Context *ctx) const {
  if (!fixed_length) {
    IR::Store(IR::Val::Int(0), IR::ArrayLength(id_val));
    IR::Store(IR::Malloc(data_type, IR::Val::Int(0)), IR::ArrayData(id_val));
    return;
  }

  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::BasicBlock::Current = init_func_->entry();

      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      auto exit_block = IR::Func::Current->AddBlock();

      auto ptr        = IR::Index(init_func_->Argument(0), IR::Val::Int(0));
      auto length_var = IR::Val::Int(static_cast<i32>(len));
      auto end_ptr    = IR::PtrIncr(ptr, length_var);
      IR::UncondJump(loop_phi);

      IR::BasicBlock::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      IR::CondJump(IR::Eq(phi_reg, end_ptr), exit_block, loop_body);

      IR::BasicBlock::Current = loop_body;
      data_type->EmitInit(phi_reg, ctx);
      auto incr = IR::PtrIncr(phi_reg, IR::Val::Int(1));
      IR::UncondJump(loop_phi);

      IR::BasicBlock::Current = exit_block;
      IR::ReturnJump();

      IR::Func::Current->SetArgs(phi, {IR::Val::BasicBlock(init_func_->entry()), ptr,
                                       IR::Val::BasicBlock(loop_body), incr});
    }
  }

  IR::Call(IR::Val::Func(init_func_), std::vector<IR::Val>{id_val}, {});
}

void Primitive::EmitInit(IR::Val id_val, Context *ctx) const {
  switch (type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type: IR::Store(IR::Val::Type(Void()), id_val); break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Code: {
      AST::CodeBlock block;
      block.type     = Code;
      block.content_ = AST::Statements{};
      return IR::Store(IR::Val::CodeBlock(std::move(block)), id_val);
    }
    case PrimType::Bool: IR::Store(IR::Val::Bool(false), id_val); break;
    case PrimType::Char: IR::Store(IR::Val::Char('\0'), id_val); break;
    case PrimType::Int: IR::Store(IR::Val::Int(0l), id_val); break;
    case PrimType::Real: IR::Store(IR::Val::Real(0.0), id_val); break;
    case PrimType::String: IR::Store(IR::Val::StrLit(""), id_val); break;
    default: UNREACHABLE();
  }
}

void Enum::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(IR::Val::Enum(this, 0), id_val);
}

void Flags::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(IR::Val::Flags(this, 0), id_val);
}

void Variant::EmitInit(IR::Val, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Pointer::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(IR::Val::Null(pointee), id_val);
}

void Function::EmitInit(IR::Val id_val, Context *ctx) const { UNREACHABLE(); }

void Scope::EmitInit(IR::Val, Context *ctx) const { UNREACHABLE(); }

void Struct::EmitInit(IR::Val id_val, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::BasicBlock::Current = init_func_->entry();

      // TODO init expressions? Do these need to be verfied too?
      for (size_t i = 0; i < fields_.size(); ++i) {
        if (fields_[i].init_val != IR::Val::None()) {
          EmitCopyInit(
              /* from_type = */ fields_[i].type,
              /*   to_type = */ fields_[i].type,
              /*  from_val = */ fields_[i].init_val,
              /*    to_var = */ IR::Field(init_func_->Argument(0), i), ctx);
        } else {
          fields_[i].type->EmitInit(IR::Field(init_func_->Argument(0), i), ctx);
        }
      }

      IR::ReturnJump();
    }
  }

  IR::Call(IR::Val::Func(init_func_), {id_val}, {});
}
} // namespace type
