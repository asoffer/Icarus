#include "all.h"

#include "../ast/ast.h"
#include "../ir/func.h"

namespace type {
IR::Val Array::EmitInitialValue() const {
  auto current_block   = IR::Block::Current;
  IR::Block::Current   = IR::Func::Current->entry();
  auto temp_allocation = IR::Alloca(this);
  IR::Block::Current   = current_block;

  // TODO must remember to destroy
  EmitInit(temp_allocation);
  return temp_allocation;
}

IR::Val Tuple::EmitInitialValue() const { NOT_YET(); }

IR::Val Pointer::EmitInitialValue() const { return IR::Val::Null(this); }
IR::Val Function::EmitInitialValue() const { return IR::Val::Func(nullptr); }
IR::Val Range::EmitInitialValue() const { NOT_YET(); }
IR::Val Slice::EmitInitialValue() const { NOT_YET(); }
IR::Val Scope::EmitInitialValue() const { NOT_YET(); }
IR::Val Variant::EmitInitialValue() const { NOT_YET(); }

IR::Val Primitive::EmitInitialValue() const {
  switch (type_) {
  case PrimType::Err: UNREACHABLE(this, ": Err");
  case PrimType::Unknown: UNREACHABLE(this, ": Unknown");
  case PrimType::Type: return IR::Val::Type(Void);
  case PrimType::Void: UNREACHABLE();
  case PrimType::NullPtr: UNREACHABLE();
  case PrimType::EmptyArray: UNREACHABLE();
  case PrimType::Code: {
    AST::CodeBlock block;
    block.type     = Code;
    block.content_ = AST::Statements{};
    return IR::Val::CodeBlock(std::move(block));
  }
  case PrimType::Bool: return IR::Val::Bool(false);
  case PrimType::Char: return IR::Val::Char('\0');
  case PrimType::Int: return IR::Val::Int(0l);
  case PrimType::Real: return IR::Val::Real(0.0);
  case PrimType::String: return IR::Val::StrLit("");
  default: UNREACHABLE();
  }
}


IR::Val Struct::EmitInitialValue() const {
  auto current_block   = IR::Block::Current;
  IR::Block::Current   = IR::Func::Current->entry();
  auto temp_allocation = IR::Alloca(this);
  IR::Block::Current   = current_block;

  // TODO must remember to destroy
  EmitInit(temp_allocation);
  return temp_allocation;
}
} // namespace type
