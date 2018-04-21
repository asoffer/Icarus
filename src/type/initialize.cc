#include "all.h"

#include "../architecture.h"
#include "../ast/ast.h"
#include "../context.h"
#include "../ir/func.h"

namespace type {
void Array::EmitInit(IR::Val id_val, Context *ctx) const {
  if (!fixed_length) {
    IR::Store(IR::Val::Int(0), IR::ArrayLength(id_val));
    IR::Store(IR::Malloc(data_type, IR::Val::Int(0)), IR::ArrayData(id_val));
    return;
  }

  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::Block::Current = init_func_->entry();

      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      auto exit_block = IR::Func::Current->AddBlock();

      auto ptr        = IR::Index(init_func_->Argument(0), IR::Val::Int(0));
      auto length_var = IR::Val::Int(static_cast<i32>(len));
      auto end_ptr    = IR::PtrIncr(ptr, length_var);
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      IR::CondJump(IR::Eq(phi_reg, end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      data_type->EmitInit(phi_reg, ctx);
      auto incr = IR::PtrIncr(phi_reg, IR::Val::Int(1));
      IR::UncondJump(loop_phi);

      IR::Block::Current = exit_block;
      IR::ReturnJump();

      IR::Func::Current->SetArgs(phi, {IR::Val::Block(init_func_->entry()), ptr,
                                       IR::Val::Block(loop_body), incr});
    }
  }

  IR::Call(IR::Val::Func(init_func_), std::vector<IR::Val>{id_val}, {});
}

void Primitive::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(EmitInitialValue(ctx), id_val);
}

void Enum::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(EmitInitialValue(ctx), id_val);
}


void Variant::EmitInit(IR::Val, Context *ctx) const {
  UNREACHABLE("Variants must be initialized.");
}

void Pointer::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(EmitInitialValue(ctx), id_val);
}

void Function::EmitInit(IR::Val id_val, Context *ctx) const {
  IR::Store(EmitInitialValue(ctx), id_val);
}

void Range::EmitInit(IR::Val, Context *ctx) const { UNREACHABLE(); }
void Slice::EmitInit(IR::Val, Context *ctx) const { UNREACHABLE(); }
void Scope::EmitInit(IR::Val, Context *ctx) const { UNREACHABLE(); }

void Struct::EmitInit(IR::Val id_val, Context *ctx) const {
  if (!init_func_) {
    init_func_ = ctx->mod_->AddFunc(
        Func({Ptr(this)}, {}),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}});

    CURRENT_FUNC(init_func_) {
      IR::Block::Current = init_func_->entry();

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
