#include "ir.h"

#include "../ast/ast.h"
#include "../type/type.h"
#include <sstream>

namespace IR {
Val Val::StackAddr(u64 addr, ::Type *t) {
  IR::Addr a;
  a.kind     = Addr::Kind::Stack;
  a.as_stack = addr;
  return Val(Ptr(t), a);
}

Val Val::HeapAddr(void *addr, ::Type *t) {
  IR::Addr a;
  a.kind    = Addr::Kind::Heap;
  a.as_heap = addr;
  return Val(Ptr(t), a);
}

Val Val::GlobalAddr(u64 addr, ::Type *t) {
  IR::Addr a;
  a.kind      = Addr::Kind::Global;
  a.as_global = addr;
  return Val(Ptr(t), a);
}

Val Val::Ref(AST::Expression *expr) { return Val(expr->type, expr); }

Val Val::Scope(AST::ScopeLiteral *scope_lit) {
  return Val(scope_lit->type, scope_lit);
}
Val Val::Enum(const ::Enum *enum_type, size_t integral_val) {
  return Val(const_cast<::Enum *>(enum_type), EnumVal{integral_val});
}

Val Val::Func(::IR::Func *fn) { return Val(fn->type, fn); }
Val Val::Null(::Type *t) { return Val(Ptr(t), IR::Addr{Addr::Kind::Null, 0}); }

std::string Val::to_string() const {
  // TODO switch on the variant kind flag?
  if (value.is<Register>()) {
    return type->to_string() + " r." + std::to_string(value.as<Register>());
  } else if (value.is<ReturnValue>()) {
    return type->to_string() + " ret." +
           std::to_string(value.as<ReturnValue>());
  } else if (value.is<::IR::Addr>()) {
    return value.as<::IR::Addr>().to_string();
  } else if (value.is<bool>()) {
    return value.as<bool>() ? "true" : "false";
  } else if (value.is<char>()) {
    return std::to_string(static_cast<i32>(value.as<char>())) + "_c";
  } else if (value.is<double>()) {
    return std::to_string(value.as<double>()) + "_r";
  } else if (value.is<i32>()) {
    return std::to_string(value.as<i32>());
  } else if (value.is<u64>()) {
    return std::to_string(value.as<u64>()) + "_u";
  } else if (value.is<EnumVal>()) {
    return value.as<EnumVal>().value >= ptr_cast<::Enum>(type)->members.size()
               ? ptr_cast<::Enum>(type)->to_string() + ":END"
               : ptr_cast<::Enum>(type)->members.at(value.as<EnumVal>().value);
  } else if (value.is<::Type *>()) {
    return value.as<::Type *>()->to_string();
  } else if (value.is<::IR::Func *>()) {
    return "fn." + value.as<IR::Func *>()->name;
  } else if (value.is<AST::ScopeLiteral *>()) {
    NOT_YET();
  } else if (value.is<AST::CodeBlock *>()) {
    return "<...>";
  } else if (value.is<AST::Expression *>()) {
    NOT_YET();
  } else if (value.is<BlockIndex>()) {
    return "block #" + std::to_string(value.as<IR::BlockIndex>());
  } else if (value.is<std::string>()) {
    return "string \"" + value.as<std::string>() + "\"";
  } else {
    UNREACHABLE();
  }
}

void Jump::Conditional(Val cond, BlockIndex true_index,
                       BlockIndex false_index) {
  Jump &jmp = Jump::Current();
  if (cond.value.is<bool>()) {
    jmp.type        = Type::Uncond;
    jmp.block_index = (cond.value.as<bool>() ? true_index : false_index);
  } else {
    jmp.type                  = Type::Cond;
    jmp.cond_data.cond        = cond;
    jmp.cond_data.true_block  = true_index;
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

Val Func::Argument(u32 n) {
  Type *arg_type = nullptr;
  if (type->input->is<Tuple>()) {
    arg_type = ptr_cast<Tuple>(type->input)->entries AT(n);
    if (arg_type->is_big()) { arg_type = Ptr(arg_type); }
  } else {
    ASSERT_EQ(n, 0);
    arg_type = type->input;
  }
  return Val::Reg(Register(n), arg_type);
}

Func::Func(::Function *fn_type)
    : type(fn_type),
      num_regs_(fn_type->is<Tuple>()
                    ? static_cast<i32>(ptr_cast<Tuple>(fn_type)->entries.size())
                    : 1),
      blocks_(2, Block(this)) {}

std::vector<std::unique_ptr<Func>> Func::All;
} // namespace IR
