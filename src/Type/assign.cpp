#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

extern IR::Func *GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                                     Function *fn_type);

extern IR::Value PtrCallFix(Type *t, IR::Value v);


void Type::CallAssignment(Scope *scope, Type *lhs_type, Type *rhs_type,
                             IR::Value from_val, IR::Value to_var) {
  assert(scope);
  if (lhs_type->is_primitive() || lhs_type->is_pointer() ||
      lhs_type->is_enum() || lhs_type->is_function()) {
    assert(lhs_type == rhs_type);
    IR::Store(rhs_type, from_val, to_var);

  } else if (lhs_type->is_array()) {
    assert(rhs_type->is_array());
    auto lhs_array_type = (Array *)lhs_type;
    auto rhs_array_type = (Array *)rhs_type;

    IR::Value lhs_ptr, rhs_ptr, rhs_len, rhs_end_ptr;
    if (rhs_array_type->fixed_length) {
      rhs_ptr = IR::Access(rhs_array_type->data_type, IR::Value(0ul), from_val);
      rhs_len = IR::Value(rhs_array_type->len);
    } else {
      rhs_ptr = IR::Load(rhs_array_type->data_type,
                         IR::ArrayData(rhs_array_type, from_val));
      rhs_len = IR::Load(Uint, IR::ArrayLength(from_val));
    }
    rhs_end_ptr = IR::PtrIncr(Ptr(rhs_array_type->data_type), rhs_ptr, rhs_len);

    if (lhs_array_type->fixed_length) {
      lhs_ptr = IR::Access(lhs_array_type->data_type, IR::Value(0ul), to_var);
    } else {
      // TODO delete first time. currently just delete
      auto rhs_bytes =
          IR::UMul(rhs_len,
                   IR::Value(lhs_array_type->data_type
                                 ->bytes())); // TODO round up for alignment?
      auto ptr        = IR::Malloc(lhs_array_type->data_type, rhs_bytes);
      auto array_data = IR::ArrayData(lhs_array_type, to_var);
      IR::Store(Ptr(lhs_array_type->data_type), ptr, array_data);
      lhs_ptr = IR::Load(Ptr(lhs_array_type->data_type), array_data);

      IR::Store(Uint, rhs_len, IR::ArrayLength(to_var));
    }

    auto init_block   = IR::Block::Current;
    auto loop_phi     = IR::Func::Current->AddBlock("loop-phi");
    auto loop_cond    = IR::Func::Current->AddBlock("loop-cond");
    auto loop_body    = IR::Func::Current->AddBlock("loop-body");
    auto land         = IR::Func::Current->AddBlock("land");
    IR::Block::Current->exit.SetUnconditional(loop_phi);
    IR::Block::Current = loop_phi;

    auto lhs_phi = IR::Phi(Ptr(lhs_array_type->data_type));
    lhs_phi.args.emplace_back(init_block);
    lhs_phi.args.emplace_back(lhs_ptr);
    auto lhs_phi_reg = IR::Value::Reg(lhs_phi.result.reg);

    auto rhs_phi = IR::Phi(Ptr(rhs_array_type->data_type));
    rhs_phi.args.emplace_back(init_block);
    rhs_phi.args.emplace_back(rhs_ptr);
    auto rhs_phi_reg = IR::Value::Reg(rhs_phi.result.reg);

    loop_phi->exit.SetUnconditional(loop_cond);
    IR::Block::Current = loop_cond;

    auto cond = IR::PtrEQ(rhs_phi_reg, rhs_end_ptr);
    IR::Block::Current->exit.SetConditional(cond, land, loop_body);
    IR::Block::Current = loop_body;

    // TODO Are these the right types?
    CallAssignment(scope, lhs_array_type->data_type, lhs_array_type->data_type,
                   PtrCallFix(rhs_array_type->data_type, rhs_phi_reg),
                   lhs_phi_reg);
    auto next_lhs =
        IR::PtrIncr(Ptr(lhs_array_type->data_type), lhs_phi_reg, IR::Value(1ul));
    auto next_rhs =
        IR::PtrIncr(Ptr(rhs_array_type->data_type), rhs_phi_reg, IR::Value(1ul));

    lhs_phi.args.emplace_back(IR::Block::Current);
    lhs_phi.args.emplace_back(next_lhs);
    rhs_phi.args.emplace_back(IR::Block::Current);
    rhs_phi.args.emplace_back(next_rhs);

    IR::Block::Current->exit.SetUnconditional(loop_phi);

    loop_phi->push(lhs_phi);
    loop_phi->push(rhs_phi);

    IR::Block::Current = land;

  } else {
    auto fn = GetFuncReferencedIn(scope, "__assign__",
                                  Func(Tup({Ptr(lhs_type), rhs_type}), Void));
    if (fn) {
      IR::Call(Void, IR::Value(fn), {to_var, from_val});
    } else {
      ((Structure *)lhs_type)->EmitDefaultAssign(to_var, from_val);
    }
  }
}

void Structure::EmitDefaultAssign(IR::Value to_var, IR::Value from_val) {
  if (!assign_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    assign_func        = new IR::Func(Func({Ptr(this), Ptr(this)}, Void));
    assign_func->name  = "assign." + Mangle(this);

    IR::Func::Current  = assign_func;
    IR::Block::Current = assign_func->entry();

    auto var = IR::Value::Arg(0);
    auto val = IR::Value::Arg(1);

    for (size_t i = 0; i < field_type.size(); ++i) {
      auto the_field_type = field_type AT(i);
      auto field_val = IR::Field(this, val, i);
      auto field_var = IR::Field(this, var, i);

      // TODO ptr call fix?
      if (!the_field_type->is_big()) {
        field_val = IR::Load(the_field_type, field_val);
      }

      Type::CallAssignment(ast_expression->scope_, the_field_type,
                           the_field_type, field_val, field_var);
    }

    IR::Block::Current->exit.SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->exit.SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(assign_func);

  IR::Call(Void, IR::Value(assign_func), {to_var, from_val});
}
