#include "type.h"

#include "../ir/ir.h"
#include "../scope.h"

extern IR::Val PtrCallFix(IR::Val v);

void Primitive::EmitRepr(IR::Val val) {
  switch (type_) {
  case PrimType::Char: {
    if (!repr_func) {
      IR::Func::All.push_back(std::make_unique<IR::Func>(
          Func(this, Void),
          std::vector<std::pair<std::string, AST::Expression *>>{
              {"arg", nullptr}}));
      repr_func = IR::Func::All.back().get();

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

          IR::CondJump(
              IR::Eq(repr_func->Argument(0), IR::Val::Char(pair.first)),
              special_block, next_block);

          IR::Block::Current = special_block;
          IR::Print(IR::Val::Char('\\'));
          IR::Print(IR::Val::Char(pair.second));
          IR::ReturnJump();

          IR::Block::Current = next_block;
        }

        IR::Print(repr_func->Argument(0));
        IR::ReturnJump();
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
  case PrimType::EmptyArray:
  case PrimType::Err:
  case PrimType::String: {
    NOT_YET();
  } break;
  case PrimType::Unknown: UNREACHABLE();
  }
}

void Function::EmitRepr(IR::Val) {
  IR::Print(IR::Val::Char('{'));
  IR::Print(IR::Val::Type(this));
  IR::Print(IR::Val::Char('}'));
}

void Enum::EmitRepr(IR::Val val) { IR::Print(val); }

// TODO print something friendlier
void Pointer::EmitRepr(IR::Val val) { IR::Print(val); }

void Array::EmitRepr(IR::Val val) {
  if (!repr_func) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(this, Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}}));
    repr_func       = IR::Func::All.back().get();
    repr_func->name = "repr." + Mangle(this);

    CURRENT_FUNC(repr_func) {
      IR::Block::Current = repr_func->entry();

      auto init_block = repr_func->AddBlock();
      auto exit_block = repr_func->AddBlock();

      IR::Print(IR::Val::Char('['));

      auto length_var = fixed_length
                            ? IR::Val::Uint(len)
                            : IR::Load(IR::ArrayLength(repr_func->Argument(0)));
      IR::CondJump(IR::Eq(length_var, IR::Val::Uint(0)), exit_block,
                   init_block);

      IR::Block::Current = init_block;
      auto ptr           = IR::Index(repr_func->Argument(0), IR::Val::Uint(0));
      auto end_ptr       = IR::PtrIncr(ptr, length_var);

      auto loop_phi  = repr_func->AddBlock();
      auto loop_body = repr_func->AddBlock();

      data_type->EmitRepr(PtrCallFix(ptr));
      IR::PtrIncr(ptr, length_var);
      IR::UncondJump(loop_phi);

      IR::Block::Current = loop_phi;
      auto phi           = IR::Phi(Ptr(data_type));
      auto phi_reg       = IR::Func::Current->Command(phi).reg();
      auto elem_ptr      = IR::PtrIncr(phi_reg, IR::Val::Uint(1));
      IR::CondJump(IR::Eq(elem_ptr, end_ptr), exit_block, loop_body);

      IR::Block::Current = loop_body;
      IR::Print(IR::Val::Char(','));
      IR::Print(IR::Val::Char(' '));
      data_type->EmitRepr(PtrCallFix(elem_ptr));
      IR::UncondJump(loop_phi);

      IR::Func::Current->SetArgs(phi, {IR::Val::Block(init_block), ptr,
                                       IR::Val::Block(IR::Block::Current),
                                       elem_ptr});

      IR::Block::Current = exit_block;
      IR::Print(IR::Val::Char(']'));
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(repr_func), std::vector<IR::Val>{val});
}

void Struct::EmitRepr(IR::Val val) {
  CompleteDefinition();
  if (!repr_func) {
    IR::Func::All.push_back(std::make_unique<IR::Func>(
        Func(this, Void),
        std::vector<std::pair<std::string, AST::Expression *>>{
            {"arg", nullptr}}));
    repr_func       = IR::Func::All.back().get();
    repr_func->name = "repr." + Mangle(this);

    CURRENT_FUNC(repr_func) {
      IR::Block::Current = repr_func->entry();

      IR::Print(IR::Val::Char('{'));
      IR::Print(IR::Val::Char(' '));
      for (size_t i = 0; i < field_type.size(); ++i) {
        field_type AT(i)->EmitRepr(
            PtrCallFix(IR::Field(repr_func->Argument(0), i)));
        IR::Print(IR::Val::Char(' '));
      }
      IR::Print(IR::Val::Char('}'));
      IR::ReturnJump();
    }
  }
  IR::Call(IR::Val::Func(repr_func), std::vector<IR::Val>{val});
}

void Tuple::EmitRepr(IR::Val) { NOT_YET(); }
void RangeType::EmitRepr(IR::Val) { NOT_YET(); }
void SliceType::EmitRepr(IR::Val) { NOT_YET(); }
void Scope_Type::EmitRepr(IR::Val) { NOT_YET(); }
void Variant::EmitRepr(IR::Val) { NOT_YET(); }
