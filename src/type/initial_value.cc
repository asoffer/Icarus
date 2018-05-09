#include "type/all.h"

#include "ast/codeblock.h"
#include "ast/statements.h"
#include "ir/func.h"
// TODO deprecate these.
namespace type {
IR::Val Array::EmitInitialValue(Context* ctx) const {
  auto current_block = IR::BasicBlock::Current;
  IR::BasicBlock::Current = IR::Func::Current->entry();
  auto temp_allocation = IR::Alloca(this);
  IR::BasicBlock::Current = current_block;

  // TODO must remember to destroy
  EmitInit(temp_allocation, ctx);
  return temp_allocation;
}

IR::Val Pointer::EmitInitialValue(Context* ctx) const {
  return IR::Val::Null(pointee);
}
IR::Val Function::EmitInitialValue(Context* ctx) const {
  return IR::Val::Func(nullptr);
}
IR::Val Scope::EmitInitialValue(Context* ctx) const { NOT_YET(); }
IR::Val Variant::EmitInitialValue(Context* ctx) const { NOT_YET(); }

IR::Val Primitive::EmitInitialValue(Context* ctx) const {
  switch (type_) {
    case PrimType::Err: UNREACHABLE(this, ": Err");
    case PrimType::Type: return IR::Val::Type(Void);
    case PrimType::Void: UNREACHABLE();
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Code: {
      AST::CodeBlock block;
      block.type = Code;
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

IR::Val Struct::EmitInitialValue(Context* ctx) const {
  auto current_block = IR::BasicBlock::Current;
  IR::BasicBlock::Current = IR::Func::Current->entry();
  auto temp_allocation = IR::Alloca(this);
  IR::BasicBlock::Current = current_block;

  // TODO must remember to destroy
  EmitInit(temp_allocation, ctx);
  return temp_allocation;
}
}  // namespace type
