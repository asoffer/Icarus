#include "ir.h"

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
Val Val::FrameAddr(u64 n, ::Type *t) {
  MAKE_AND_RETURN(Kind::Frame, t, as_frame_addr, n);
}
Val Val::HeapAddr(u64 n, ::Type *t) {
  MAKE_AND_RETURN(Kind::Heap, t, as_heap_addr, n);
}
Val Val::GlobalAddr(u64 n, ::Type *t) {
  MAKE_AND_RETURN(Kind::Global, t, as_global_addr, n);
}
Val Val::Bool(bool b) { MAKE_AND_RETURN(Kind::Const, ::Bool, as_bool, b); }
Val Val::Char(char c) { MAKE_AND_RETURN(Kind::Const, ::Char, as_char, c); }
Val Val::Real(double r) { MAKE_AND_RETURN(Kind::Const, ::Real, as_real, r); }
Val Val::U16(u16 n) { MAKE_AND_RETURN(Kind::Const, ::U16, as_u16, n); }
Val Val::U32(u32 n) { MAKE_AND_RETURN(Kind::Const, ::U32, as_u32, n); }
Val Val::Uint(u64 n) { MAKE_AND_RETURN(Kind::Const, ::Uint, as_uint, n); }
Val Val::Int(i64 n) { MAKE_AND_RETURN(Kind::Const, ::Int, as_int, n); }
Val Val::Type(::Type *t) { MAKE_AND_RETURN(Kind::Const, ::Type_, as_type, t); }
Val Val::Func(::IR::Func *fn) {
  MAKE_AND_RETURN(Kind::Const, fn->type, as_func, fn);
}
// Using 'as_bool' for convenience. That field should never be used.
Val Val::Void() { MAKE_AND_RETURN(Kind::Const, ::Void, as_bool, false); }

// Type here is sort of nonsense but that doesn't matter
Val Val::Block(BlockIndex bi) {
  MAKE_AND_RETURN(Kind::Const, ::Uint, as_block, bi);
}

// Using 'as_bool' for convenience. That field should never be used.
Val Val::Null(::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), as_bool, false);
}

std::string Val::to_string() const {
  std::stringstream ss;
  switch (kind) {
  case Kind::Arg:
    return type->to_string() + " a." + std::to_string(as_arg);
  case Kind::Reg:
    return type->to_string() + " r." +
           std::to_string(as_reg.block_index.value) + "." +
           std::to_string(as_reg.instr_index);
  case Kind::Frame:
    return type->to_string() + " f." + std::to_string(as_frame_addr);
  case Kind::Global:
    return type->to_string() + " g." + std::to_string(as_global_addr);
  case Kind::Heap:
    return type->to_string() + " h." + std::to_string(as_heap_addr);
  case Kind::Const:
    if (type == nullptr) {
      return "--";
    } else if (type == ::Bool) {
      return as_bool ? "true" : "false";
    } else if (type == ::Char) {
      // TODO print the actual character if that's useful.
      return std::to_string(static_cast<i32>(as_char)) + "_c";
    } else if (type == ::U16) {
      return std::to_string(as_u16) + "_u16";
    } else if (type == ::U32) {
      return std::to_string(as_u32) + "_u32";
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
    } else if (type->is_function()) {
      std::stringstream ss;
      ss << "fn." << as_func;
      return ss.str();
    } else {
      UNREACHABLE;
    }
  case Kind::None:
    return "---";
  }
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
} // namespace IR
