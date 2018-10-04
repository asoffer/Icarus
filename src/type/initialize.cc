#include "type/all.h"

#include "architecture.h"
#include "ast/codeblock.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

namespace AST {
struct Expression;
}  // namespace AST

namespace type {
void Array::EmitInit(IR::Register id_reg, Context *ctx) const {
  if (!fixed_length) {
    IR::StoreInt(0, IR::ArrayLength(id_reg));
    IR::StoreAddr(IR::Malloc(data_type, 0), IR::ArrayData(id_reg, this));
  }

  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        base::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::BasicBlock::Current = init_func_->entry();

      auto ptr = IR::Index(type::Ptr(this), init_func_->Argument(0), 0);
      auto end_ptr =
          IR::PtrIncr(ptr, static_cast<i32>(len), type::Ptr(data_type));

      using tup = std::tuple<IR::RegisterOr<IR::Addr>>;
      IR::CreateLoop(
          [&](tup const &phis) {
            return IR::EqAddr(std::get<0>(phis), end_ptr);
          },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitInit(std::get<0>(phis).reg_, ctx);
            return tup{
                IR::PtrIncr(std::get<0>(phis).reg_, 1, type::Ptr(data_type))};
          },
          std::tuple<type::Type const *>{type::Ptr(data_type)}, tup{ptr});

      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  IR::Call(IR::AnyFunc{init_func_}, std::move(call_args));
}

void Primitive::EmitInit(IR::Register id_reg, Context *ctx) const {
  switch (type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type: IR::StoreType(type::Void(), id_reg); break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Code: {
      AST::CodeBlock block;
      block.content_ = AST::Statements{};
      NOT_YET();
    }
    case PrimType::Bool: IR::StoreBool(false, id_reg); break;
    case PrimType::Char: IR::StoreChar('\0', id_reg); break;
    case PrimType::Int: IR::StoreInt(0, id_reg); break;
    case PrimType::Real: IR::StoreReal(0.0, id_reg); break;
    default: UNREACHABLE();
  }
}

void Enum::EmitInit(IR::Register id_reg, Context *ctx) const {
  IR::StoreEnum(IR::EnumVal{0}, id_reg);
}

void Flags::EmitInit(IR::Register id_reg, Context *ctx) const {
  IR::StoreFlags(IR::FlagsVal{0}, id_reg);
}

void Variant::EmitInit(IR::Register, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Pointer::EmitInit(IR::Register id_reg, Context *ctx) const {
  IR::StoreAddr(IR::Addr::Null(), id_reg);
}

void Function::EmitInit(IR::Register id_reg, Context *ctx) const { UNREACHABLE(); }

void Scope::EmitInit(IR::Register, Context *ctx) const { UNREACHABLE(); }

void CharBuffer::EmitInit(IR::Register id_reg, Context *ctx) const {
  NOT_YET();
}

void Struct::EmitInit(IR::Register id_reg, Context *ctx) const {
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
              /*    to_var = */
              IR::Field(init_func_->Argument(0), this, i), ctx);
        } else {
          fields_.at(i).type->EmitInit(
              IR::Field(init_func_->Argument(0), this, i), ctx);
        }
      }

      IR::ReturnJump();
    }
  }

  IR::LongArgs call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  IR::Call(IR::AnyFunc{init_func_}, std::move(call_args));
}
} // namespace type
