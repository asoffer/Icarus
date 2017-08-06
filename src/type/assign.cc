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
    ASSERT(from_type->is<Array>(), "");
    auto *from_array_type = ptr_cast<Array>(from_type);
    auto *to_array_type   = ptr_cast<Array>(to_type);

    IR::Val from_ptr = IR::Index(from_val, IR::Val::Uint(0));
    IR::Val len      = from_array_type->fixed_length
                      ? IR::Val::Uint(from_array_type->len)
                      : IR::Load(IR::ArrayLength(from_val));
    IR::Val from_end_ptr = IR::PtrIncr(from_ptr, len);

    if (!to_array_type->fixed_length) {
      // TODO delete first time. currently just delete
      // TODO Architecture dependence?
      auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
          len, from_array_type->data_type);
      auto ptr        = IR::Malloc(from_array_type->data_type, to_bytes);
      auto array_data = IR::ArrayData(to_var);
      IR::Free(IR::Load(array_data));
      IR::Store(len, IR::ArrayLength(to_var));
      IR::Store(ptr, array_data);
    }

    IR::Val to_ptr = IR::Index(to_var, IR::Val::Uint(0));

    auto init_block = IR::Block::Current;
    auto loop_phi   = IR::Func::Current->AddBlock();
    auto loop_body  = IR::Func::Current->AddBlock();
    auto land       = IR::Func::Current->AddBlock();
    IR::Jump::Unconditional(loop_phi);

    IR::Block::Current = loop_phi;
    auto from_phi      = IR::Phi(Ptr(from_array_type->data_type));
    auto to_phi        = IR::Phi(Ptr(to_array_type->data_type));
    IR::Jump::Conditional(IR::Eq(from_phi, from_end_ptr), land, loop_body);

    IR::Block::Current = loop_body;
    // TODO Are these the right types?
    CallAssignment(scope, from_array_type->data_type, to_array_type->data_type,
                   PtrCallFix(from_phi), to_phi);

    IR::Func::Current->SetArgs(from_phi.as_reg,
                               {IR::Val::Block(init_block), from_ptr,
                                IR::Val::Block(IR::Block::Current),
                                IR::PtrIncr(from_phi, IR::Val::Uint(1ul))});
    IR::Func::Current->SetArgs(to_phi.as_reg,
                               {IR::Val::Block(init_block), to_ptr,
                                IR::Val::Block(IR::Block::Current),
                                IR::PtrIncr(to_phi, IR::Val::Uint(1ul))});
    IR::Jump::Unconditional(loop_phi);

    IR::Block::Current = land;
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
    assign_func       = new IR::Func(Func({this, Ptr(this)}, Void));
    assign_func->name = "assign." + Mangle(this);

    CURRENT_FUNC(assign_func) {
      IR::Block::Current = assign_func->entry();

      auto var = IR::Val::Arg(Ptr(this), 0);
      auto val = IR::Val::Arg(Ptr(this), 1);

      for (size_t i = 0; i < field_type.size(); ++i) {
        auto the_field_type = field_type AT(i);
        auto field_val      = IR::Field(val, i);
        auto field_var      = IR::Field(var, i);

        // TODO ptr call fix?
        if (!the_field_type->is_big()) { field_val = IR::Load(field_val); }

        // TODO is that the right scope?
        Type::CallAssignment(type_scope, the_field_type, the_field_type,
                             field_val, field_var);
      }

      IR::Jump::Return();
    }
  }
  ASSERT(assign_func, "");

  IR::Call(IR::Val::Func(assign_func), {to_var, from_val});
}
