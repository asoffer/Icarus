#include "type.h"

#include "../ast/ast.h"
#include "../ir/ir.h"
#include "scope.h"

void Primitive::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}
void Enum::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}
void Function::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}

void Array::EmitInit(IR::Val id_val) {
  if (!init_func) {
    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->name = "init." + Mangle(this);
    implicit_functions.push_back(init_func);
    CURRENT_FUNC(init_func) {
      IR::Block::Current = init_func->entry();

      auto ptr = IR::Val::None();
      auto length_var = IR::Val::None();
      if (fixed_length) {
        ptr = IR::Index(IR::Val::Arg(Ptr(this), 0), IR::Val::Uint(0));
        length_var     = IR::Val::Uint(len);
        auto end_ptr   = IR::PtrIncr(ptr, length_var);

        auto loop_phi  = IR::Func::Current->AddBlock();
        auto loop_body = IR::Func::Current->AddBlock();

        IR::Jump::Unconditional(loop_phi);

        IR::Block::Current = loop_phi;
        auto phi           = IR::Phi(Ptr(data_type));
        IR::Jump::Conditional(IR::Eq(phi, end_ptr), IR::Func::Current->exit(),
                              loop_body);

        IR::Block::Current = loop_body;
        data_type->EmitInit(phi);
        auto incr = IR::PtrIncr(phi, IR::Val::Uint(1));
        IR::Jump::Unconditional(loop_phi);

        IR::Block::Current = IR::Func::Current->exit();
        IR::Jump::Return();

        IR::Func::Current->SetArgs(phi.as_reg,
                                   {IR::Val::Block(init_func->entry()), ptr,
                                    IR::Val::Block(loop_body), incr});

      } else {
        IR::Store(IR::Val::Uint(0),
                  IR::ArrayLength(IR::Val::Arg(Ptr(this), 0)));
        IR::Store(IR::Malloc(data_type, IR::Val::Uint(0ul)),
                 IR::ArrayData(IR::Val::Arg(Ptr(this), 0)));
        IR::Jump::Return();
      }
    }
  }

  IR::Call(IR::Val::Func(init_func), std::vector<IR::Val>{id_val});
}

void Pointer::EmitInit(IR::Val id_val) {
  IR::Store(IR::Val::Null(this), id_val);
}

void Struct::EmitInit(IR::Val id_val) {
  CompleteDefinition();

  if (!init_func) {
    init_func       = new IR::Func(Func(Ptr(this), Void));
    init_func->name = "init." + Mangle(this);
    implicit_functions.push_back(init_func);

    CURRENT_FUNC(init_func) {
      IR::Block::Current = init_func->entry();

      // TODO init expressions? Do these need to be verfied too?
      for (size_t i = 0; i < field_type.size(); ++i) {
        if (init_values[i]) {
          if (init_values[i]->is_hole()) { continue; }
          Type::CallAssignment(init_values[i]->scope_, field_type[i],
                               init_values[i]->type,
                               init_values[i]->EmitIR(),
                               IR::Field(IR::Val::Arg(Ptr(this), 0), i));
        } else {
          field_type[i]->EmitInit(IR::Field(IR::Val::Arg(Ptr(this), 0), i));
        }
      }

      IR::Jump::Unconditional(IR::Func::Current->exit());
      IR::Block::Current = IR::Func::Current->exit();
      IR::Jump::Return();
    }
  }

  IR::Call(IR::Val::Func(init_func), {id_val});
}

void Tuple::EmitInit(IR::Val) { NOT_YET(); }
void RangeType::EmitInit(IR::Val) { UNREACHABLE(); }
void SliceType::EmitInit(IR::Val) { UNREACHABLE(); }
void Scope_Type::EmitInit(IR::Val) { UNREACHABLE(); }
