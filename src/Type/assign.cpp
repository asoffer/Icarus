#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

extern std::vector<IR::Func *> implicit_functions;

namespace data {
extern llvm::Value *global_string(const std::string &s);
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

void Structure::EmitDefaultAssign(IR::Value to_var, IR::Value from_val) {
  if (!assign_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    assign_func        = new IR::Func(Func({Ptr(this), Ptr(this)}, Void));
    assign_func->name  = "assign." + Mangle(this);
    implicit_functions.push_back(assign_func);

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
