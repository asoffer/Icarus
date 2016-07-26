#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

/*
// TODO rename? This isn't really about init-ing literals. it's more about allocating
llvm::Value *Array::initialize_literal(llvm::Value *alloc, llvm::Value *len) {
  auto use_calloc         = data_type->is_primitive();
  llvm::Value *alloc_call = nullptr;

  if (use_calloc) {
    alloc_call = builder.CreateBitCast(
        builder.CreateCall(cstdlib::calloc(),
                           {len, data::const_uint(data_type->bytes())}),
        *Ptr(data_type));

  } else {
    auto bytes_to_alloc =
        builder.CreateMul(len, data::const_uint(data_type->bytes()));

    alloc_call = builder.CreateBitCast(
        builder.CreateCall(cstdlib::malloc(), {bytes_to_alloc}),
        *Ptr(data_type));

  }

  // TODO allocate this in the right place
  builder.CreateStore(len, builder.CreateGEP(alloc, {data::const_uint(0),
                                                     data::const_uint(0)}));

  builder.CreateStore(
      alloc_call,
      builder.CreateGEP(alloc, {data::const_uint(0), data::const_uint(1)}));

  return alloc;
}
*/

void Primitive::EmitInit(IR::Value id_val) {
  IR::Store(this, EmitInitialValue(), id_val);
}
void Enum::EmitInit(IR::Value id_val) {
  IR::Store(this, EmitInitialValue(), id_val);
}

void Array::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->SetName("init." + Mangle(this));

    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    IR::Value val;
    if (fixed_length) {
      val = IR::Value::Arg(0);
    } else {
      auto len_ptr = IR::ArrayLength(IR::Value::Arg(0));
      IR::Store(Uint, IR::Value(0ul), len_ptr);
      auto ptr = IR::ArrayData(this, IR::Value::Arg(0));
      IR::Store(Ptr(data_type), IR::Malloc(data_type, IR::Value(0ul)), ptr);
      val = IR::Load(Ptr(data_type), ptr);
    }

    for (size_t i = 0; i < len; ++i) {
      data_type->EmitInit(IR::Access(data_type, IR::Value(i), val));
    }

    IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  IR::Call(Void, IR::Value(init_func), {id_val});
}

void Pointer::EmitInit(IR::Value id_val) {
  IR::Store(this, IR::Value::Null(this), id_val);
}

void Structure::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func = new IR::Func(Func(Ptr(this), Void));
    init_func->SetName("init." + Mangle(this));

    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    // TODO init expressions?

    for (size_t i = 0; i < field_type.size(); ++i) {
      if (init_values[i]) {
        if (init_values[i]->is_hole()) { continue; }
        Type::CallAssignment(init_values[i]->scope_, field_type[i],
                             init_values[i]->type, init_values[i]->EmitIR(),
                             IR::Field(this, IR::Value::Arg(0), i));
      } else {
        field_type[i]->EmitInit(IR::Field(this, IR::Value::Arg(0), i));
      }
    }

    IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
    IR::Block::Current = IR::Func::Current->exit();
    IR::Block::Current->SetReturnVoid();

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  IR::Call(Void, IR::Value(init_func), {id_val});
}

void Tuple::EmitInit(IR::Value id_val) { NOT_YET; }
void Function::EmitInit(IR::Value id_val) { /* Intentionally do nothing */ }
void RangeType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void SliceType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void TypeVariable::EmitInit(IR::Value id_val) { UNREACHABLE; }
void ParametricStructure::EmitInit(IR::Value id_val) { UNREACHABLE; }
