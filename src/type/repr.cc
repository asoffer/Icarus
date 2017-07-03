#include "type.h"
#include "../ir/ir.h"
#include "../scope.h"

extern IR::Val PtrCallFix(IR::Val v);

void Primitive::EmitRepr(IR::Val val) {
  switch (type_) {
  case PrimType::Char: {
    if (!repr_func) {
      repr_func = new IR::Func(Func(this, Void));
      implicit_functions.push_back(repr_func);

      CURRENT_FUNC(repr_func) {
        IR::Block::Current = repr_func->entry();
        repr_func->name    = "repr." + Mangle(this);

        IR::Print(IR::Val::Char('`'));

        for (auto pair :
             {std::make_pair('\a', 'a'), std::make_pair('\b', 'b'),
              std::make_pair('\n', 'n'), std::make_pair('\r', 'r'),
              std::make_pair('\t', 't'), std::make_pair('\v', 'v')}) {
          auto special_block = repr_func->AddBlock();
          auto next_block    = repr_func->AddBlock();

          IR::Jump::Conditional(
              IR::Eq(IR::Val::Arg(Char, 0), IR::Val::Char(pair.first)),
              special_block, next_block);

          IR::Block::Current = special_block;
          IR::Print(IR::Val::Char('\\'));
          IR::Print(IR::Val::Char(pair.second));
          IR::Jump::Unconditional(repr_func->exit());

          IR::Block::Current = next_block;
        }

        IR::Print(IR::Val::Arg(Char, 0));
        IR::Jump::Unconditional(repr_func->exit());

        IR::Block::Current = repr_func->exit();
        IR::Jump::Return();
      }
    }

    IR::Call(IR::Val::Func(repr_func), std::vector<IR::Val>{val});
  } break;

  case PrimType::Bool:
  case PrimType::Int:
  case PrimType::Uint:
  case PrimType::Real:
  case PrimType::Type:
  case PrimType::Code: {
    IR::Print(val);
  } break;
  case PrimType::Void:
  case PrimType::NullPtr:
  case PrimType::Err:
  case PrimType::String: {
    NOT_YET;
  } break;
  case PrimType::Unknown:
    UNREACHABLE;
  }
}

void Function::EmitRepr(IR::Val) {
  IR::Print(IR::Val::Char('{'));
  IR::Print(IR::Val::Type(this));
  IR::Print(IR::Val::Char('}'));
}

// TODO print something friendlier
void Enum::EmitRepr(IR::Val val) { IR::Print(val); }

void Pointer::EmitRepr(IR::Val) { UNREACHABLE; }

void Array::EmitRepr(IR::Val val) {
  if (fixed_length) {
    IR::Print(IR::Val::Char('['));
    if (len >= 1) {
      data_type->EmitRepr(PtrCallFix(IR::Access(IR::Val::Uint(0), val)));
    }

    // TODO This might generate way too much code. Loop in generated code
    // instead.
    for (decltype(len) i = 1; i < len; ++i) {
      IR::Print(IR::Val::Char(','));
      IR::Print(IR::Val::Char(' '));
      data_type->EmitRepr(PtrCallFix(IR::Access(IR::Val::Uint(i), val)));
    }

    IR::Print(IR::Val::Char(']'));
    return;
  }

  if (!repr_func) {
    repr_func = new IR::Func(Func(this, Void));
    repr_func->name = "repr." + Mangle(this);
    implicit_functions.push_back(repr_func);

    CURRENT_FUNC(repr_func) {
      IR::Block::Current = repr_func->entry();

      auto init_block = repr_func->AddBlock();

      IR::Print(IR::Val::Char('['));

      IR::Val ptr = IR::Val::None();
      IR::Val length_var = IR::Val::None();
      if (fixed_length) {
        // Can assume length is not zero or one because these are handled above.
        length_var = IR::Val::Uint(len);
        ptr = IR::Access(IR::Val::Uint(0), IR::Val::Arg(this, 0));
        IR::Jump::Unconditional(init_block);
      } else {
        length_var = IR::Load(IR::ArrayLength(IR::Val::Arg(Uint, 0)));
        ptr = IR::Load(IR::ArrayData(IR::Val::Arg(this, 0)));
        IR::Jump::Conditional(IR::Eq(length_var, IR::Val::Uint(0)),
                              repr_func->exit(), init_block);
      }

      auto end_ptr = IR::PtrIncr(ptr, length_var);

      auto loop_phi = repr_func->AddBlock();
      auto loop_body = repr_func->AddBlock();

      data_type->EmitRepr(PtrCallFix(ptr));
      IR::PtrIncr(ptr, length_var);
      IR::Jump::Unconditional(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi = IR::Phi(Ptr(data_type));
      auto elem_ptr = IR::PtrIncr(phi, IR::Val::Uint(1));
      IR::Jump::Conditional(IR::Eq(elem_ptr, end_ptr), repr_func->exit(),
                            loop_body);

      IR::Block::Current = loop_body;
      IR::Print(IR::Val::Char(','));
      IR::Print(IR::Val::Char(' '));
      data_type->EmitRepr(PtrCallFix(elem_ptr));
      IR::Jump::Unconditional(loop_phi);

      IR::Func::Current->SetArgs(
          phi.as_reg, {IR::Val::Block(init_block), ptr,
                       IR::Val::Block(IR::Block::Current), elem_ptr});

      IR::Block::Current = repr_func->exit();
      IR::Print(IR::Val::Char(']'));
      IR::Jump::Return();
    }
  }
  IR::Call(IR::Val::Func(repr_func), std::vector<IR::Val>{val});
}

void Struct::EmitRepr(IR::Val val) {
  IR::Print(IR::Val::Char('{'));
  IR::Print(IR::Val::Char(' '));
  for (size_t i = 0; i < field_type.size(); ++i) {
    field_type AT(i)->EmitRepr(PtrCallFix(IR::Field(val, i)));
    IR::Print(IR::Val::Char(' '));
  }
  IR::Print(IR::Val::Char('}'));
}

void Tuple::EmitRepr(IR::Val) { NOT_YET; }
void RangeType::EmitRepr(IR::Val) { NOT_YET; }
void SliceType::EmitRepr(IR::Val) { NOT_YET; }
void Scope_Type::EmitRepr(IR::Val) { NOT_YET; }
