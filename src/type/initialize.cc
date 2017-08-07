#include "type.h"

#include "../architecture.h"
#include "../ast/ast.h"
#include "../ir/ir.h"
#include "scope.h"

extern IR::Val PtrCallFix(IR::Val v);

void Primitive::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}
void Enum::EmitInit(IR::Val id_val) { IR::Store(EmitInitialValue(), id_val); }
void Function::EmitInit(IR::Val id_val) {
  IR::Store(EmitInitialValue(), id_val);
}

void Array::EmitInit(IR::Val id_val) {
  if (!fixed_length) {
    IR::Store(IR::Val::Uint(0), IR::ArrayLength(id_val));
    IR::Store(IR::Malloc(data_type, IR::Val::Uint(0)), IR::ArrayData(id_val));
    return;
  }

  if (!init_func) {
    init_func       = new IR::Func(Func(Ptr(this), Void));
    init_func->name = "init." + Mangle(this);
    implicit_functions.push_back(init_func);
    CURRENT_FUNC(init_func) {
      IR::Block::Current = init_func->entry();

      auto loop_phi  = IR::Func::Current->AddBlock();
      auto loop_body = IR::Func::Current->AddBlock();

      auto ptr        = IR::Index(IR::Val::Arg(Ptr(this), 0), IR::Val::Uint(0));
      auto length_var = IR::Val::Uint(len);
      auto end_ptr    = IR::PtrIncr(ptr, length_var);
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
          EmitCopyInit(
              /* from_type = */ init_values[i]->type,
              /*   to_type = */ field_type[i],
              /*  from_val = */ init_values[i]->EmitIR(),
              /*    to_var = */ IR::Field(IR::Val::Arg(Ptr(this), 0), i));
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

using InitFnType = void (*)(Type *, Type *, IR::Val, IR::Val);
template <InitFnType InitFn>
static IR::Val ArrayInitializationWith(Array *from_type, Array *to_type) {
  static std::unordered_map<Array *, std::unordered_map<Array *, IR::Func>>
      init_fns;

  auto insertion = init_fns[to_type].emplace(
      from_type, IR::Func(Func({from_type, Ptr(to_type)}, Void)));
  IR::Func *fn = &insertion.first->second;
  if (insertion.second) {
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      auto from_arg      = IR::Val::Arg(Ptr(from_type), 0);
      auto to_arg        = IR::Val::Arg(Ptr(to_type), 1);
      auto phi_block     = IR::Func::Current->AddBlock();
      auto body_block    = IR::Func::Current->AddBlock();

      auto from_len = from_type->fixed_length
                          ? IR::Val::Uint(from_type->len)
                          : IR::Load(IR::ArrayLength(from_arg));

      if (!to_type->fixed_length) {
        // TODO Architecture dependence?
        auto to_bytes = Architecture::InterprettingMachine().ComputeArrayLength(
            from_len, from_type->data_type);

        IR::Store(from_len, IR::ArrayLength(to_arg));
        IR::Store(IR::Malloc(from_type->data_type, to_bytes),
                  IR::ArrayData(to_arg));
      }

      auto from_start = IR::Index(from_arg, IR::Val::Uint(0));
      auto to_start   = IR::Index(to_arg, IR::Val::Uint(0));
      auto from_end   = IR::PtrIncr(from_start, from_len);
      IR::Jump::Unconditional(phi_block);

      IR::Block::Current = phi_block;
      auto from_phi      = IR::Phi(Ptr(from_type->data_type));
      auto to_phi        = IR::Phi(Ptr(to_type->data_type));
      IR::Jump::Conditional(IR::Ne(from_phi, from_end), body_block, fn->exit());

      IR::Block::Current = body_block;
      InitFn(from_type->data_type, to_type->data_type, PtrCallFix(from_phi),
             to_phi);
      auto from_incr = IR::PtrIncr(from_phi, IR::Val::Uint(1));
      auto to_incr   = IR::PtrIncr(to_phi, IR::Val::Uint(1));
      IR::Jump::Unconditional(phi_block);

      fn->SetArgs(from_phi.as_reg, {IR::Val::Block(fn->entry()), from_start,
                                    IR::Val::Block(body_block), from_incr});
      fn->SetArgs(to_phi.as_reg, {IR::Val::Block(fn->entry()), to_start,
                                  IR::Val::Block(body_block), to_incr});

      IR::Block::Current = IR::Func::Current->exit();
      IR::Jump::Return();
    }
  }
  return IR::Val::Func(fn);
}

template <InitFnType InitFn>
static IR::Val StructInitializationWith(Struct *struct_type) {
  static std::unordered_map<Struct *, IR::Func> struct_init_fns;
  auto insertion = struct_init_fns.emplace(
      struct_type, IR::Func(Func({struct_type, Ptr(struct_type)}, Void)));
  IR::Func *fn = &insertion.first->second;
  if (insertion.second) {
    CURRENT_FUNC(fn) {
      for (size_t i = 0; i < struct_type->field_type.size(); ++i) {
        InitFn(struct_type->field_type[i], struct_type->field_type[i],
               PtrCallFix(IR::Field(IR::Val::Arg(Ptr(struct_type), 0), i)),
               IR::Field(IR::Val::Arg(Ptr(struct_type), 1), i));
      }

      IR::Block::Current = IR::Func::Current->exit();
      IR::Jump::Return();
    }
  }
  return IR::Val::Func(fn);
}

void Type::EmitMoveInit(Type *from_type, Type *to_type, IR::Val from_val,
                        IR::Val to_var) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() ||
      to_type->is<Pointer>()) {
    ASSERT_EQ(to_type, from_type);
    IR::Store(from_val, to_var);

  } else if (to_type->is<Array>()) {
    auto *to_array_type   = ptr_cast<Array>(to_type);
    auto *from_array_type = ptr_cast<Array>(from_type);

    if (to_array_type->fixed_length || from_array_type->fixed_length) {
      IR::Call(
          ArrayInitializationWith<EmitMoveInit>(from_array_type, to_array_type),
          {from_val, to_var});

    } else {
      IR::Store(IR::Load(IR::ArrayLength(from_val)), IR::ArrayLength(to_var));
      IR::Store(IR::Load(IR::ArrayData(from_val)), IR::ArrayData(to_var));
      // TODO if this move is to be destructive, this assignment to array
      // length is not necessary.
      IR::Store(IR::Val::Uint(0), IR::ArrayLength(from_val));
      IR::Store(IR::Malloc(from_array_type->data_type, IR::Val::Uint(0)),
                IR::ArrayData(from_val));
    }
  } else if (to_type->is<Struct>()) {
    ASSERT_EQ(to_type, from_type);
    IR::Call(StructInitializationWith<EmitMoveInit>(ptr_cast<Struct>(to_type)),
             {from_val, to_var});

  } else if (to_type->is<Function>()) {
    NOT_YET();
  } else if (to_type->is<Tuple>()) {
    NOT_YET();
  } else if (to_type->is<RangeType>()) {
    NOT_YET();
  } else if (to_type->is<SliceType>()) {
    NOT_YET();
  } else if (to_type->is<Scope_Type>()) {
    NOT_YET();
  }
}

void Type::EmitCopyInit(Type *from_type, Type *to_type, IR::Val from_val,
                        IR::Val to_var) {
  if (to_type->is<Primitive>() || to_type->is<Enum>() ||
      to_type->is<Pointer>()) {
    ASSERT_EQ(to_type, from_type);
    IR::Store(from_val, to_var);
  } else if (to_type->is<Array>()) {
    IR::Call(ArrayInitializationWith<EmitCopyInit>(ptr_cast<Array>(from_type),
                                                   ptr_cast<Array>(to_type)),
             {from_val, to_var});

  } else if (to_type->is<Struct>()) {
    ASSERT_EQ(to_type, from_type);
    IR::Call(StructInitializationWith<EmitCopyInit>(ptr_cast<Struct>(to_type)),
             {from_val, to_var});

  } else if (to_type->is<Function>()) {
    NOT_YET();
  } else if (to_type->is<Tuple>()) {
    NOT_YET();
  } else if (to_type->is<RangeType>()) {
    NOT_YET();
  } else if (to_type->is<SliceType>()) {
    NOT_YET();
  } else if (to_type->is<Scope_Type>()) {
    NOT_YET();
  }
}
