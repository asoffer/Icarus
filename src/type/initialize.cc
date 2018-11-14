#include "type/all.h"

#include "architecture.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"

namespace ast {
struct Expression;
}  // namespace ast

namespace type {
void Array::EmitInit(ir::Register id_reg, Context *ctx) const {
  if (!fixed_length) {
    ir::Store(0, ir::ArrayLength(id_reg));
    ir::Store(ir::Malloc(data_type, 0), ir::ArrayData(id_reg, this));
  }

  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      ir::BasicBlock::Current = init_func_->entry();

      auto ptr = ir::Index(type::Ptr(this), init_func_->Argument(0), 0);
      auto end_ptr =
          ir::PtrIncr(ptr, static_cast<i32>(len), type::Ptr(data_type));

      using tup = std::tuple<ir::RegisterOr<ir::Addr>>;
      ir::CreateLoop(
          [&](tup const &phis) { return ir::Eq(std::get<0>(phis), end_ptr); },
          [&](tup const &phis) {
            ASSERT(std::get<0>(phis).is_reg_);
            data_type->EmitInit(std::get<0>(phis).reg_, ctx);
            return tup{
                ir::PtrIncr(std::get<0>(phis).reg_, 1, type::Ptr(data_type))};
          },
          std::tuple<type::Type const *>{type::Ptr(data_type)}, tup{ptr});

      ir::ReturnJump();
    }
  }

  ir::LongArgs call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}

void Primitive::EmitInit(ir::Register id_reg, Context *ctx) const {
  switch (type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type_: ir::Store(type::Void(), id_reg); break;
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Bool: ir::Store(false, id_reg); break;
    case PrimType::Char: ir::Store('\0', id_reg); break;
    case PrimType::Int: ir::Store(0, id_reg); break;
    case PrimType::Float32: ir::Store(0.0f, id_reg); break;
    case PrimType::Float64: ir::Store(0.0, id_reg); break;
    default: UNREACHABLE();
  }
}

void Enum::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::EnumVal{0}, id_reg);
}

void Flags::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::FlagsVal{0}, id_reg);
}

void Variant::EmitInit(ir::Register, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Pointer::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::Addr::Null(), id_reg);
}

void Function::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE();
}

void CharBuffer::EmitInit(ir::Register id_reg, Context *ctx) const {
  NOT_YET();
}

void Struct::EmitInit(ir::Register id_reg, Context *ctx) const {
  std::unique_lock lock(mtx_);
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      ir::BasicBlock::Current = init_func_->entry();

      // TODO init expressions? Do these need to be verfied too?
      for (size_t i = 0; i < fields_.size(); ++i) {
        if (fields_[i].init_val != ir::Val::None()) {
          EmitCopyInit(
              /* from_type = */ fields_[i].type,
              /*   to_type = */ fields_[i].type,
              /*  from_val = */ fields_[i].init_val,
              /*    to_var = */
              ir::Field(init_func_->Argument(0), this, i), ctx);
        } else {
          fields_.at(i).type->EmitInit(
              ir::Field(init_func_->Argument(0), this, i), ctx);
        }
      }

      ir::ReturnJump();
    }
  }

  ir::LongArgs call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}
}  // namespace type
