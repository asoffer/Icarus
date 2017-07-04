#include "ir.h"

#include "../ast/ast.h"
#include "../type/type.h"

namespace IR {
#define MAKE_AND_RETURN(k, t, name, v)                                         \
  Val val = IR::Val::None();                                                   \
  val.kind = (k);                                                              \
  val.type = (t);                                                              \
  val.name = (v);                                                              \
  return val

Val Val::Arg(::Type *t, u64 n) { MAKE_AND_RETURN(Kind::Arg, t, as_arg, n); }
Val Val::Reg(RegIndex r, ::Type *t) {
  MAKE_AND_RETURN(Kind::Reg, t, as_reg, r);
}
Val Val::StackAddr(u64 addr, ::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), as_addr,
                  (IR::Addr{Addr::Kind::Stack, addr}));
}
Val Val::GlobalAddr(u64 addr, ::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), as_addr,
                  (IR::Addr{Addr::Kind::Global, addr}));
}
Val Val::Addr(IR::Addr addr, ::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), as_addr, addr);
}

Val Val::Bool(bool b) { MAKE_AND_RETURN(Kind::Const, ::Bool, as_bool, b); }
Val Val::Char(char c) { MAKE_AND_RETURN(Kind::Const, ::Char, as_char, c); }
Val Val::Real(double r) { MAKE_AND_RETURN(Kind::Const, ::Real, as_real, r); }
Val Val::Uint(u64 n) { MAKE_AND_RETURN(Kind::Const, ::Uint, as_uint, n); }
Val Val::Int(i64 n) { MAKE_AND_RETURN(Kind::Const, ::Int, as_int, n); }
Val Val::Type(::Type *t) { MAKE_AND_RETURN(Kind::Const, ::Type_, as_type, t); }
Val Val::CodeBlock(AST::CodeBlock *block) {
  MAKE_AND_RETURN(Kind::Const, ::Code, as_code, block);
}
Val Val::Scope(AST::ScopeLiteral *scope_lit) {
  MAKE_AND_RETURN(Kind::Const, scope_lit->type, as_scope, scope_lit);
}
Val Val::Enum(::Enum *enum_type, size_t integral_val) {
  MAKE_AND_RETURN(Kind::Const, enum_type, as_enum, integral_val);
}

Val Val::Func(::IR::Func *fn) {
  MAKE_AND_RETURN(Kind::Const, fn->type, as_func, fn);
}
// Using 'as_bool' for convenience. That field should never be used.
Val Val::Void() { MAKE_AND_RETURN(Kind::Const, ::Void, as_bool, false); }

// Using the nonsense type Ptr(Void) to indicate that this is a block.
// TODO FIXME
Val Val::Block(BlockIndex bi) {
  MAKE_AND_RETURN(Kind::Const, Ptr(::Void), as_block, bi);
}

Val Val::Null(::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), as_addr,
                  (IR::Addr{Addr::Kind::Null, 0}));
}
#undef MAKE_AND_RETURN

std::string Val::to_string() const {
  switch (kind) {
  case Kind::Arg:
    return type->to_string() + " a." + std::to_string(as_arg);
  case Kind::Reg:
    return type->to_string() + " r." + std::to_string(as_reg.index);
  case Kind::Const:
    if (type == nullptr) {
      return "--";
    } else if (type == ::Bool) {
      return as_bool ? "true" : "false";
    } else if (type == ::Char) {
      // TODO print the actual character if that's useful.
      return std::to_string(static_cast<i32>(as_char)) + "_c";
    } else if (type == ::Int) {
      return std::to_string(as_int);
    } else if (type == ::Uint) {
      return std::to_string(as_uint) + "_u";
    } else if (type == ::Real) {
      return std::to_string(as_real) + "_r";
    } else if (type == ::Type_) {
      return as_type->to_string();
    } else if (type == ::Void) {
      return "<void>";
    } else if (type == Ptr(::Void)) {
      return "block #" + std::to_string(as_block.value);
    } else if (type->is<Function>()) {
      std::stringstream ss;
      ss << "fn." << as_func;
      return ss.str();
    } else if (type->is<Pointer>()) {
      return as_addr.to_string();
    } else if (type->is<::Enum>()) {
      return as_enum >= ptr_cast<::Enum>(type)->members.size()
                 ? ptr_cast<::Enum>(type)->to_string() + ":END"
                 : ptr_cast<::Enum>(type)->members.at(as_enum);
    } else {
      std::cerr << *type << std::endl;
      UNREACHABLE;
    }
    return "";
  case Kind::None:
    return "---";
  }
  std::cerr << "Kind had value of " << static_cast<int>(kind) << std::endl;
  UNREACHABLE;
}

void Jump::Conditional(Val cond, BlockIndex true_index,
                       BlockIndex false_index) {
  Jump &jmp = IR::Func::Current->blocks_[IR::Block::Current.value].jmp_;
  if (cond.kind == Val::Kind::Const) {
    jmp.type = Type::Uncond;
    ASSERT(cond.type == Bool, "");
    jmp.block_index = (cond.as_bool ? true_index : false_index);
  } else {
    jmp.type = Type::Cond;
    jmp.cond_data.cond = cond;
    jmp.cond_data.true_block = true_index;
    jmp.cond_data.false_block = false_index;
  }
}

void Jump::Return() {
  Jump &jmp = IR::Func::Current->blocks_[IR::Block::Current.value].jmp_;
  jmp.type = Type::Ret;
}

void Jump::Unconditional(BlockIndex index) {
  Jump& jmp = IR::Func::Current->blocks_[IR::Block::Current.value].jmp_;
  jmp.block_index = index;
  jmp.type = Type::Uncond;
}

std::string Addr::to_string() const {
  switch (kind) {
  case Addr::Kind::Null: return "null";
  case Kind::Global: return "g." + std::to_string(as_global);
  case Kind::Stack: return "s." + std::to_string(as_stack);
  }
  UNREACHABLE;
}

bool operator==(Addr lhs, Addr rhs) {
  if (lhs.kind != rhs.kind) { return false; }
  switch (lhs.kind) {
  case Addr::Kind::Null: return true;
  case Addr::Kind::Stack: return lhs.as_stack == rhs.as_stack;
  case Addr::Kind::Global: return lhs.as_global == rhs.as_global;
  }
  UNREACHABLE;
}

} // namespace IR
