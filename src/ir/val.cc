#include "ir.h"

#include "../ast/ast.h"
#include "../type/type.h"
#include <sstream>

namespace IR {
#define MAKE_AND_RETURN(k, t, v)                                               \
  Val val   = IR::Val::None();                                                 \
  val.kind  = (k);                                                             \
  val.type  = (t);                                                             \
  val.value = (v);                                                             \
  return val

Val Val::StackAddr(u64 addr, ::Type *t) {
  IR::Addr a;
  a.kind     = Addr::Kind::Stack;
  a.as_stack = addr;
  return Val(Kind::Const, Ptr(t), a);
}

Val Val::HeapAddr(void *addr, ::Type *t) {
  IR::Addr a;
  a.kind    = Addr::Kind::Heap;
  a.as_heap = addr;
  return Val(Kind::Const, Ptr(t), a);
}

Val Val::GlobalAddr(u64 addr, ::Type *t) {
  IR::Addr a;
  a.kind      = Addr::Kind::Global;
  a.as_global = addr;
  return Val(Kind::Const, Ptr(t), a);
}

Val Val::Ref(AST::Expression *expr) {
  MAKE_AND_RETURN(Kind::Const, expr->type,  expr);
}

Val Val::Scope(AST::ScopeLiteral *scope_lit) {
  MAKE_AND_RETURN(Kind::Const, scope_lit->type, scope_lit);
}
Val Val::Enum(const ::Enum *enum_type, size_t integral_val) {
  MAKE_AND_RETURN(Kind::Const, const_cast<::Enum *>(enum_type),
                  EnumVal{integral_val});
}

// TODO FIXME Make this a real type?
Val Val::Block(BlockIndex bi) { MAKE_AND_RETURN(Kind::Const, Ptr(::Void), bi); }
Val Val::Func(::IR::Func *fn) { MAKE_AND_RETURN(Kind::Const, fn->type, fn); }
Val Val::Void() { MAKE_AND_RETURN(Kind::Const, ::Void, false); }

Val Val::Null(::Type *t) {
  MAKE_AND_RETURN(Kind::Const, Ptr(t), (IR::Addr{Addr::Kind::Null, 0}));
}
#undef MAKE_AND_RETURN

std::string Val::to_string() const {
  switch (kind) {
  case Kind::Arg:
    return type->to_string() + " a." + std::to_string(value.as<Argument>().value);
  case Kind::Reg:
    return type->to_string() + " r." + std::to_string(value.as<RegIndex>().index);
  case Kind::Const:
    if (type == nullptr) {
      return "--";
    } else if (type == ::Bool) {
      return value.as<bool>() ? "true" : "false";
    } else if (type == ::Char) {
      // TODO print the actual character if that's useful.
      return std::to_string(static_cast<i32>(value.as<char>())) + "_c";
    } else if (type == ::Int) {
      return std::to_string(value.as<i64>());
    } else if (type == ::Uint) {
      return std::to_string(value.as<u64>()) + "_u";
    } else if (type == ::Real) {
      return std::to_string(value.as<double>()) + "_r";
    } else if (type == ::Type_) {
      return value.as<::Type *>()->to_string();
    } else if (type == ::Void) {
      return "<void>";
    } else if (type == Ptr(::Void)) {
      return "block #" + std::to_string(value.as<IR::BlockIndex>().value);
    } else if (type == ::String) {
      return "string \"" + value.as<std::string>() + "\"";
    } else if (type == ::Code) {
      return "<...>";
    } else if (type->is<Function>()) {
      std::stringstream ss;
      ss << "fn." << value.as<IR::Func *>()->name;
      return ss.str();
    } else if (type->is<Pointer>()) {
      return value.as<::IR::Addr>().to_string();
    } else if (type->is<::Enum>()) {
      return value.as<EnumVal>().value >= ptr_cast<::Enum>(type)->members.size()
                 ? ptr_cast<::Enum>(type)->to_string() + ":END"
                 : ptr_cast<::Enum>(type)->members.at(
                       value.as<EnumVal>().value);
    } else {
      UNREACHABLE(*type);
    }
    return "";
  case Kind::None:
    return "---";
  }

  UNREACHABLE("Kind had value of ", static_cast<int>(kind));
}

void Jump::Conditional(Val cond, BlockIndex true_index,
                       BlockIndex false_index) {
  Jump &jmp = Jump::Current();
  if (cond.kind == Val::Kind::Const) {
    ASSERT_EQ(cond.type, Bool);
    jmp.type = Type::Uncond;
    jmp.block_index = (cond.value.as<bool>() ? true_index : false_index);
  } else {
    jmp.type = Type::Cond;
    jmp.cond_data.cond = cond;
    jmp.cond_data.true_block = true_index;
    jmp.cond_data.false_block = false_index;
  }
}

Jump &Jump::Current() { return Func::Current->block(IR::Block::Current).jmp_; }

std::string Addr::to_string() const {
  std::stringstream ss;
  switch (kind) {
  case Kind::Null: ss << "null"; break;
  case Kind::Global: ss << "g." << as_global; break;
  case Kind::Stack: ss << "s." << as_stack; break;
  case Kind::Heap: ss << "h." << as_heap; break;
  }
  return ss.str();
}

bool operator==(Addr lhs, Addr rhs) {
  if (lhs.kind != rhs.kind) { return false; }
  switch (lhs.kind) {
  case Addr::Kind::Null: return true;
  case Addr::Kind::Stack: return lhs.as_stack == rhs.as_stack;
  case Addr::Kind::Global: return lhs.as_global == rhs.as_global;
  case Addr::Kind::Heap: return lhs.as_heap == rhs.as_heap;
  }
  UNREACHABLE();
}

Block &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

} // namespace IR
