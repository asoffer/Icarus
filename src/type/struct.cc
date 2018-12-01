#include "type/struct.h"

#include "architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace type {
void Struct::EmitAssign(Type const *from_type, ir::Val const &from,
                        ir::Register to, Context *ctx) const {
  std::unique_lock lock(mtx_);
  ASSERT(this == from_type);
  if (!assign_func_) {
    assign_func_ = ctx->mod_->AddFunc(
        type::Func({from_type, type::Ptr(this)}, {}),
        base::vector<std::pair<std::string, ast::Expression *>>{
            {"from", nullptr}, {"to", nullptr}});

    CURRENT_FUNC(assign_func_) {
      ir::BasicBlock::Current = assign_func_->entry();
      auto val                = assign_func_->Argument(0);
      auto var                = assign_func_->Argument(1);

      for (size_t i = 0; i < fields_.size(); ++i) {
        auto *field_type = from_type->as<type::Struct>().fields_.at(i).type;
        fields_[i].type->EmitAssign(
            fields_[i].type,
            ir::Val::Reg(ir::PtrFix(ir::Field(val, this, i), field_type),
                         field_type),
            ir::Field(var, this, i), ctx);
      }

      ir::ReturnJump();
    }
  }
  ASSERT(assign_func_ != nullptr);
  ir::Arguments call_args;
  call_args.append(from);
  call_args.append(to);
  call_args.type_ = assign_func_->type_;
  ir::Call(ir::AnyFunc{assign_func_}, std::move(call_args));
}

Struct *Struct::Make(ast::StructLiteral *lit) {
  auto *s             = new Struct;
  s->to_be_completed_ = lit;
  return s;
}

size_t Struct::offset(size_t field_num, Architecture const &arch) const {
  size_t offset = 0;
  for (size_t i = 0; i < field_num; ++i) {
    offset += arch.bytes(fields_.at(i).type);
    offset = arch.MoveForwardToAlignment(fields_.at(i + 1).type, offset);
  }
  return offset;
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

  ir::Arguments call_args;
  call_args.append(id_reg);
  call_args.type_ = init_func_->type_;
  ir::Call(ir::AnyFunc{init_func_}, std::move(call_args));
}

}  // namespace type
