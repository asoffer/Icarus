#include "type.h"

#include "../ir/ir.h"
#include "../scope.h"

extern IR::Val PtrCallFix(IR::Val v);

void Type::CallAssignment(Scope *scope, Type *lhs_type, Type *rhs_type,
                          IR::Val from_val, IR::Val to_var) {
  ASSERT(scope, "");
  if (lhs_type->has_vars() || rhs_type->has_vars()) {
    return;
  } else if (lhs_type->is_primitive() || lhs_type->is_pointer() ||
             lhs_type->is_function()) {
    ASSERT(lhs_type == rhs_type, "");

    IR::Store(from_val, to_var);
  } else if (lhs_type->is_enum()) {
    ASSERT(lhs_type == rhs_type, "");
    IR::Store(from_val, to_var);

  } else if (lhs_type->is_array()) {
    ASSERT(rhs_type->is_array(), "");
    auto lhs_array_type = (Array *)lhs_type;
    auto rhs_array_type = (Array *)rhs_type;

    IR::Val lhs_ptr = IR::Val::None();
    IR::Val rhs_ptr = IR::Val::None();
    IR::Val rhs_len = IR::Val::None();
    IR::Val rhs_end_ptr = IR::Val::None();

    if (rhs_array_type->fixed_length) {
      rhs_ptr = IR::Access(IR::Val::Uint(0ul), from_val);
      rhs_len = IR::Val::Uint(rhs_array_type->len);
    } else {
      rhs_ptr = IR::Load(IR::ArrayData(from_val));
      rhs_len = IR::Load(IR::ArrayLength(from_val));
    }
    rhs_end_ptr = IR::PtrIncr(rhs_ptr, rhs_len);

    if (lhs_array_type->fixed_length) {
      lhs_ptr =
          IR::Access(IR::Val::Uint(0ul), to_var);
    } else {
      // TODO delete first time. currently just delete
      auto rhs_bytes =
          IR::Mul(rhs_len,
                  IR::Val::Uint(lhs_array_type->data_type
                                    ->bytes())); // TODO round up for alignment?
      auto ptr = IR::Malloc(lhs_array_type->data_type, rhs_bytes);
      auto array_data = IR::ArrayData(to_var);
      IR::Store(ptr, array_data);
      lhs_ptr = IR::Load(array_data);

      IR::Store(rhs_len, IR::ArrayLength(to_var));
    }

    auto init_block = IR::Block::Current;
    auto loop_phi = IR::Func::Current->AddBlock();
    auto loop_body = IR::Func::Current->AddBlock();
    auto land = IR::Func::Current->AddBlock();
    IR::Jump::Unconditional(loop_phi);

    IR::Block::Current = loop_phi;
    auto lhs_phi = IR::Phi(Ptr(lhs_array_type->data_type));
    auto rhs_phi = IR::Phi(Ptr(rhs_array_type->data_type));
    IR::Jump::Conditional(IR::Eq(rhs_phi, rhs_end_ptr), land, loop_body);

    IR::Block::Current = loop_body;
    // TODO Are these the right types?
    CallAssignment(scope, lhs_array_type->data_type, lhs_array_type->data_type,
                   PtrCallFix(rhs_phi), lhs_phi);
    auto next_lhs = IR::PtrIncr(lhs_phi, IR::Val::Uint(1ul));
    auto next_rhs = IR::PtrIncr(rhs_phi, IR::Val::Uint(1ul));

    // TODO FIXME XXX THIS IS HACKY!
    IR::Func::Current->blocks_[lhs_phi.as_reg.block_index.value]
        .cmds_[lhs_phi.as_reg.instr_index]
        .args = {IR::Val::Block(init_block), lhs_ptr,
                 IR::Val::Block(IR::Block::Current), next_lhs};
    IR::Func::Current->blocks_[rhs_phi.as_reg.block_index.value]
        .cmds_[rhs_phi.as_reg.instr_index]
        .args = {IR::Val::Block(init_block), rhs_ptr,
                 IR::Val::Block(IR::Block::Current), next_rhs};
    IR::Jump::Unconditional(loop_phi);

    IR::Block::Current = land;
  } else if (lhs_type->is_scope_type() && rhs_type->is_scope_type()) {
    ASSERT(lhs_type == rhs_type, "");
    IR::Store(from_val, to_var);

  } else {
    auto fn = scope->FuncHereOrNull("__assign__",
                                    Func(Tup({Ptr(lhs_type), rhs_type}), Void));
    if (fn != IR::Val::None()) {
      IR::Call(fn, {to_var, from_val});
    } else {
      ((Struct *)lhs_type)->EmitDefaultAssign(to_var, from_val);
    }
  }
}

void Struct::EmitDefaultAssign(IR::Val to_var, IR::Val from_val) {
  if (!assign_func) {
    assign_func = new IR::Func(Func({Ptr(this), Ptr(this)}, Void));
    assign_func->name = "assign." + Mangle(this);

    CURRENT_FUNC(assign_func) {

      auto var = IR::Val::Arg(this, 0);
      auto val = IR::Val::Arg(this, 1);

      for (size_t i = 0; i < field_type.size(); ++i) {
        auto the_field_type = field_type AT(i);
        auto field_val = IR::Field(val, i);
        auto field_var = IR::Field(var, i);

        // TODO ptr call fix?
        if (!the_field_type->is_big()) {
          field_val = IR::Load(field_val);
        }

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
