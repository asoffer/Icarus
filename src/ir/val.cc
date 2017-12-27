#include "ir.h"

#include <sstream>

#include "../ast/ast.h"
#include "../type/type.h"

namespace IR {
Val Val::Addr(IR::Addr addr, ::Type *t) { return Val(Ptr(t), addr); }

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
Val Val::NullPtr() { return Val(::NullPtr, IR::Addr{Addr::Kind::Null, 0}); }

std::string Val::to_string() const {
  return std::visit(
      base::overloaded{
          [this](Register reg) -> std::string {
            std::stringstream ss;
            ss << reg;
            return this->type->to_string() + " r." + ss.str();
          },
          [this](ReturnValue ret) -> std::string {
            return this->type->to_string() + " ret." +
                   std::to_string(ret.value);
          },
          [](IR::Addr addr) -> std::string { return addr.to_string(); },
          [](bool b) -> std::string { return b ? "true" : "false"; },
          [](char c) -> std::string {
            return std::to_string(static_cast<i32>(c)) + "_c";
          },
          [](double d) -> std::string { return std::to_string(d) + "_r"; },
          [](i32 n) -> std::string { return std::to_string(n); },
          [](u64 n) -> std::string { return std::to_string(n) + "_u"; },
          [this](EnumVal e) -> std::string {
            return e.value >= this->type->as<::Enum>().members.size()
                       ? this->type->as<::Enum>().to_string() + ":END"
                       : this->type->as<::Enum>().members AT(e.value);
          },
          [](::Type *t) -> std::string { return t->to_string(); },
          [](IR::Func *f) -> std::string {
            return "fn." +
                   (f->name == ""
                        ? std::to_string(reinterpret_cast<uintptr_t>(f)) + "-" +
                              f->type->to_string()
                        : f->name);
          },
          [](AST::ScopeLiteral *) -> std::string { NOT_YET(); },
          [](AST::CodeBlock *) -> std::string { return "<...>"; },
          [](AST::Expression *) -> std::string { NOT_YET(); },
          [](BlockIndex b) -> std::string {
            return "block #" + std::to_string(b);
          },
          [](const std::string &s) -> std::string {
            return "string \"" + s + "\"";
          },
      },
      value);
}

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
    arg_type = type->input->as<Tuple>().entries AT(n);
    if (arg_type->is_big()) { arg_type = Ptr(arg_type); }
  } else {
    ASSERT_EQ(n, 0);
    arg_type = type->input;
  }
  return Val::Reg(Register(n), arg_type);
}

Func::Func(::Function *fn_type,
           std::vector<std::pair<std::string, AST::Expression *>> args)
    : type(fn_type), args_(std::move(args)),
      num_regs_(static_cast<i32>(fn_type->num_inputs())) {
  blocks_.push_back(std::move(Block(this)));
  i32 num_args = static_cast<i32>(args_.size());
  for (i32 i = 0; i < num_args; ++i) {
    reg_map_[Register(static_cast<i32>(i))] = CmdIndex{0, i - num_args};
  }
}

bool Register::is_arg(const Func &fn) const {
  return value_ >= 0 && value_ < static_cast<decltype(value_)>(fn.args_.size());
}

std::vector<std::unique_ptr<Func>> Func::All;
} // namespace IR
