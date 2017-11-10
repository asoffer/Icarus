#include "type.h"

#include "../architecture.h"
#include "../ir/ir.h"
#include "../scope.h"

extern IR::Val PtrCallFix(IR::Val v);

void Type::CallAssignment(Scope *scope, Type *from_type, Type *to_type,
                          IR::Val from_val, IR::Val to_var) {

  ASSERT(scope, "");
  if (from_type->is<Primitive>() || from_type->is<Pointer>() ||
      from_type->is<Function>()) {

    ASSERT_EQ(from_type, to_type);
    IR::Store(from_val, to_var);

  } else if (from_type->is<Enum>()) {
    ASSERT_EQ(from_type, to_type);
    IR::Store(from_val, to_var);

  } else if (to_type->is<Array>()) {
    ASSERT_TYPE(Array, from_type);
    auto *from_array_type = ptr_cast<Array>(from_type);
    auto *to_array_type   = ptr_cast<Array>(to_type);

    IR::Func::All.push_back(
        std::make_unique<IR::Func>(Func({Ptr(from_type), Ptr(to_type)}, Void)));
    auto *assign_func = IR::Func::All.back().get(); // TODO cache this
    assign_func->name = "assign." + Mangle(from_type) + Mangle(to_type);

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();
      auto val           = assign_func->Argument(0);
      auto var           = assign_func->Argument(1);
      IR::Val len        = from_array_type->fixed_length
                        ? IR::Val::Uint(from_array_type->len)
                        : IR::Load(IR::ArrayLength(val));
      IR::Val from_ptr     = IR::Index(val, IR::Val::Uint(0));
      IR::Val from_end_ptr = IR::PtrIncr(from_ptr, len);

      if (!to_array_type->fixed_length) {
        to_array_type->EmitDestroy(var);
        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            len, to_array_type->data_type);
        auto ptr = IR::Malloc(to_array_type->data_type, to_bytes);
        IR::Store(len, IR::ArrayLength(var));
        IR::Store(ptr, IR::ArrayData(var));
      }

      IR::Val to_ptr = IR::Index(var, IR::Val::Uint(0));

      auto init_block = IR::Block::Current;
      auto exit_block = IR::Func::Current->AddBlock();
      auto loop_phi   = IR::Func::Current->AddBlock();
      auto loop_body  = IR::Func::Current->AddBlock();
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto from_phi      = IR::Phi(Ptr(from_array_type->data_type));
      auto to_phi        = IR::Phi(Ptr(to_array_type->data_type));
      auto from_phi_reg  = IR::Func::Current->Command(from_phi).reg();
      auto to_phi_reg    = IR::Func::Current->Command(to_phi).reg();

      IR::CondJump(IR::Eq(from_phi_reg, from_end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      EmitCopyInit(from_array_type->data_type, to_array_type->data_type,
                   PtrCallFix(from_phi_reg), to_phi_reg);

      IR::UncondJump(loop_phi);

      IR::Func::Current->SetArgs(
          from_phi, {IR::Val::Block(init_block), from_ptr,
                     IR::Val::Block(IR::Block::Current),
                     IR::PtrIncr(from_phi_reg, IR::Val::Uint(1ul))});
      IR::Func::Current->SetArgs(to_phi,
                                 {IR::Val::Block(init_block), to_ptr,
                                  IR::Val::Block(IR::Block::Current),
                                  IR::PtrIncr(to_phi_reg, IR::Val::Uint(1ul))});

      IR::Block::Current = exit_block;
      IR::ReturnJump();
    }
    IR::Call(IR::Val::Func(assign_func), {from_val, to_var});

  } else if (from_type->is<Scope_Type>() && to_type->is<Scope_Type>()) {
    ASSERT_EQ(from_type, to_type);
    IR::Store(from_val, to_var);

  } else {
    auto fn = scope->FuncHereOrNull("__assign__",
                                    Func(Tup({Ptr(to_type), from_type}), Void));
    if (fn != IR::Val::None()) {
      IR::Call(fn, {to_var, from_val});
    } else {
      ptr_cast<Struct>(from_type)->EmitDefaultAssign(to_var, from_val);
    }
  }
}

void Struct::EmitDefaultAssign(IR::Val to_var, IR::Val from_val) {
  CompleteDefinition();
  if (!assign_func) {
    IR::Func::All.push_back(
        std::make_unique<IR::Func>(Func({this, Ptr(this)}, Void)));
    assign_func       = IR::Func::All.back().get();
    assign_func->name = "assign." + Mangle(this);

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();
      auto val           = assign_func->Argument(0);
      auto var           = assign_func->Argument(1);

      for (size_t i = 0; i < field_type.size(); ++i) {
        auto the_field_type = field_type AT(i);
        // TODO is that the right scope?
        Type::CallAssignment(type_scope, the_field_type, the_field_type,
                             PtrCallFix(IR::Field(val, i)), IR::Field(var, i));
      }

      IR::ReturnJump();
    }
  }
  ASSERT(assign_func, "");

  IR::Call(IR::Val::Func(assign_func), {to_var, from_val});
}
