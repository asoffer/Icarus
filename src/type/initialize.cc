#include "type/all.h"

#include "architecture.h"
#include "ast/codeblock.h"
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
  }

  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::BasicBlock::Current = init_func_->entry();

      auto ptr        = IR::Index(init_func_->Argument(0), IR::Val::Int(0));
      auto length_var = IR::Val::Int(static_cast<i32>(len));
      auto end_ptr    = IR::PtrIncr(ptr, length_var);

      CreateLoop({ptr},
                 [&](const base::vector<IR::Val> &phis) {
                   return IR::Eq(phis[0], end_ptr);
                 },
                 [&](const base::vector<IR::Val> &phis) {
                   data_type->EmitInit(phis[0], ctx);
                   return base::vector<IR::Val>{
                       IR::PtrIncr(phis[0], IR::Val::Int(1))};
                 });

      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(id_val);
  IR::Call(IR::Val::Func(init_func_), std::move(call_args));
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

void CharBuffer::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(IR::Val::CharBuf(""), id_val);
}

void Struct::EmitInit(IR::Val id_val, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
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

  IR::LongArgs call_args;
  call_args.append(id_val);
  IR::Call(IR::Val::Func(init_func_), std::move(call_args));
}
} // namespace type
