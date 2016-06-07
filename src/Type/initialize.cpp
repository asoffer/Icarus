#ifndef ICARUS_UNITY
#include "Type.h"
#include "Scope.h"
#endif

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

namespace cstdlib {
extern llvm::Constant *calloc();
extern llvm::Constant *malloc();
} // namespace cstdlib

namespace data {
extern llvm::Value *global_string(const std::string &s);
extern llvm::Constant *null_pointer(Type *t);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
extern llvm::ConstantInt *const_true();
extern llvm::ConstantInt *const_false();
} // namespace data

extern llvm::Module *global_module;

void Primitive::call_init(llvm::Value *var) {
  switch (type_) {
  case TypeEnum::Error: assert(false && "Constructor called for type Error");
  case TypeEnum::Unknown:
    assert(false && "Constructor called for unknown type");
  case TypeEnum::Type: assert(false && "Constructor called for type");
  case TypeEnum::Void: assert(false && "Constructor called for void type");
  case TypeEnum::NullPtr: assert(false && "Constructor called for raw nullptr");
  case TypeEnum::Bool: builder.CreateStore(data::const_false(), var); return;
  case TypeEnum::Char: builder.CreateStore(data::const_char('\0'), var); return;
  case TypeEnum::Int: builder.CreateStore(data::const_int(0), var); return;
  case TypeEnum::Real: builder.CreateStore(data::const_real(0), var); return;
  case TypeEnum::Uint: builder.CreateStore(data::const_uint(0), var); return;
  }
}

// TODO Change this is array size is known at compile time
void Array::call_init(llvm::Value *var) {
  if (init_fn_ == nullptr) {
    init_fn_ = llvm::Function::Create(*Func(Ptr(this), Void),
                                      llvm::Function::ExternalLinkage,
                                      "init." + Mangle(this), global_module);

    auto ip = builder.saveIP();

    auto block = make_block("entry", init_fn_);
    builder.SetInsertPoint(block);
    auto arg = init_fn_->args().begin();

    if (fixed_length) {
      // TODO This seems to be wasteful. Shouldn't we share?
      // TODO out of laziness now i will just generate something correct rather
      // than something fast/compact.

      // Repeatedly call_init on the entries. Probably a fancy loop makes more sense.
      for (size_t i = 0; i < len; ++i) {
        data_type->call_init(
            builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(i)}));
      }

    } else {
      builder.CreateStore(
          data::const_uint(0),
          builder.CreateGEP(arg, {data::const_uint(0), data::const_uint(0)}));
      auto ptr = builder.CreateBitCast(
          builder.CreateCall(cstdlib::malloc(), {data::const_uint(0)}),
          *Ptr(data_type));

      builder.CreateStore(ptr, builder.CreateGEP(arg, {data::const_uint(0),
                                                       data::const_uint(1)}));
    }

    builder.CreateRetVoid();
    builder.restoreIP(ip);
  }

  builder.CreateCall(init_fn_, {var});
}

void Tuple::call_init(llvm::Value *var) { NOT_YET; }

void Pointer::call_init(llvm::Value *var) {
  builder.CreateStore(data::null_pointer(pointee), var);
}

void Function::call_init(llvm::Value *var) {}

void Enumeration::call_init(llvm::Value *var) {
  builder.CreateStore(data::const_uint(0), var);
}

void RangeType::call_init(llvm::Value *) { UNREACHABLE; }
void SliceType::call_init(llvm::Value *) { UNREACHABLE; }
void TypeVariable::call_init(llvm::Value *) { UNREACHABLE; }
void ParametricStructure::call_init(llvm::Value *) { UNREACHABLE; }

void Structure::call_init(llvm::Value *var) {
  if (init_fn_ == nullptr) {
    init_fn_ = llvm::Function::Create(*Func(Ptr(this), Void),
                                      llvm::Function::ExternalLinkage,
                                      "init." + Mangle(this), global_module);

    auto ip = builder.saveIP();

    auto block = make_block("entry", init_fn_);
    builder.SetInsertPoint(block);

    // initialize all fields
    for (const auto &kv : field_num_to_llvm_num) {
      auto init_expr = init_values AT(kv.first);
      if (init_expr && init_expr->is_hole()) { continue; }

      auto the_field_type = field_type AT(kv.first);
      auto arg =
          builder.CreateGEP(init_fn_->args().begin(),
                            {data::const_uint(0), data::const_uint(kv.second)});
      if (init_expr) {
        auto init_val = init_expr->generate_code();

        Type::CallAssignment(ast_expression->scope_, the_field_type,
                             the_field_type, arg, init_val);
      } else {
        the_field_type->call_init(arg);
      }
    }

    builder.CreateRetVoid();
    builder.restoreIP(ip);
  }

  builder.CreateCall(init_fn_, {var});
}

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


void Primitive::EmitInit(IR::Value id_val) {
  switch (type_) {
  case TypeEnum::Error: UNREACHABLE;
  case TypeEnum::Unknown: UNREACHABLE;
  case TypeEnum::Type: UNREACHABLE;
  case TypeEnum::Void: UNREACHABLE;
  case TypeEnum::NullPtr: UNREACHABLE;
  case TypeEnum::Bool: IR::Store(this, IR::Value(false), id_val); return;
  case TypeEnum::Char: IR::Store(this, IR::Value((char)0), id_val); return;
  case TypeEnum::Int: IR::Store(this, IR::Value((int)0), id_val); return;
  case TypeEnum::Real: IR::Store(this, IR::Value(0.0), id_val); return;
  case TypeEnum::Uint: IR::Store(this, IR::Value((size_t)0), id_val); return;
  }
}

void Array::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func          = new IR::Func;
    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    if (fixed_length) {
      for (size_t i = 0; i < len; ++i) {
        auto gep = IR::GEP(this, IR::Value::Arg(0), {0, (int)i});
        IR::Block::Current->push(gep);
        data_type->EmitInit(gep);
      }
    } else {
      NOT_YET;
    }

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  auto call = IR::CallCmd(IR::Value(init_func));
  call.args.push_back(id_val);
  IR::Block::Current->cmds.push_back(call);
}

void Pointer::EmitInit(IR::Value id_val) {
  IR::Store(this, IR::Value(nullptr), id_val);
}

void Structure::EmitInit(IR::Value id_val) {
  if (!init_func) {
    auto saved_func  = IR::Func::Current;
    auto saved_block = IR::Block::Current;

    init_func          = new IR::Func;
    IR::Func::Current  = init_func;
    IR::Block::Current = init_func->entry();

    // TODO init expressions?
    for (size_t i = 0; i < field_type.size(); ++i) {
      auto gep = IR::GEP(this, IR::Value::Arg(0), {0, (int)i});
      IR::Block::Current->push(gep);
      field_type[i]->EmitInit(gep);
    }

    IR::Func::Current  = saved_func;
    IR::Block::Current = saved_block;
  }
  assert(init_func);

  auto call = IR::CallCmd(IR::Value(init_func));
  call.args.push_back(id_val);
  IR::Block::Current->cmds.push_back(call);
}

void Tuple::EmitInit(IR::Value id_val) { NOT_YET; }
void Enumeration::EmitInit(IR::Value id_val) { NOT_YET; }
void Function::EmitInit(IR::Value id_val) {}
void RangeType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void SliceType::EmitInit(IR::Value id_val) { UNREACHABLE; }
void TypeVariable::EmitInit(IR::Value id_val) { UNREACHABLE; }
void ParametricStructure::EmitInit(IR::Value id_val) { UNREACHABLE; }
