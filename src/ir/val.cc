#include "val.h"

#include <sstream>

#include "../ast/ast.h"
#include "../type/enum.h"
#include "../type/function.h"
#include "../type/pointer.h"
#include "../type/struct.h"
#include "func.h"

extern type::Type *NullPtr;

namespace IR {
Val Val::CodeBlock(AST::CodeBlock block) {
  return Val(type::Code, std::move(block));
}

Val Val::Struct() { return Val(type::Type_, new type::Struct); }

Val Val::Addr(IR::Addr addr, const type::Type *t) { return Val(Ptr(t), addr); }

Val Val::StackAddr(u64 addr, const type::Type *t) {
  IR::Addr a;
  a.kind     = Addr::Kind::Stack;
  a.as_stack = addr;
  return Val(Ptr(t), a);
}

Val Val::HeapAddr(void *addr, const type::Type *t) {
  IR::Addr a;
  a.kind    = Addr::Kind::Heap;
  a.as_heap = addr;
  return Val(Ptr(t), a);
}

Val Val::GlobalAddr(u64 addr, const type::Type *t) {
  IR::Addr a;
  a.kind      = Addr::Kind::Global;
  a.as_global = addr;
  return Val(Ptr(t), a);
}

Val Val::Ref(AST::Expression *expr) { return Val(expr->type, expr); }

Val Val::Scope(AST::ScopeLiteral *scope_lit) {
  return Val(scope_lit->type, scope_lit);
}
Val Val::Enum(const type::Enum *enum_type, size_t integral_val) {
  return Val(enum_type, EnumVal{integral_val});
}

Val Val::FnLit(AST::FunctionLiteral *fn) { return Val(fn->type, fn); }
Val Val::GenFnLit(AST::GenericFunctionLiteral *fn) { return Val(fn->type, fn); }
Val Val::Func(IR::Func *fn) { return Val(fn->ir_type, fn); }
Val Val::Null(const type::Type *t) {
  return Val(Ptr(t), IR::Addr{Addr::Kind::Null, 0});
}
Val Val::NullPtr() { return Val(type::NullPtr, IR::Addr{Addr::Kind::Null, 0}); }

static std::string Escaped(const std::string& s) {
  std::stringstream ss;
  for (char c : s) {
    switch (c) {
    case '\a': ss << R"(\a)"; break;
    case '\b': ss << R"(\b)"; break;
    case '\n': ss << R"(\n)"; break;
    case '\r': ss << R"(\r)"; break;
    case '\t': ss << R"(\t)"; break;
    case '\v': ss << R"(\r)"; break;
    case '"': ss << R"(\")"; break;
    default: ss << c; break;
    }
  }
  return ss.str();
}
std::string Val::to_string() const {
  return std::visit(
      base::overloaded{
          [this](Register reg) -> std::string {
            return this->type->to_string() + " r." + std::to_string(reg);
          },
          [this](ReturnValue ret) -> std::string {
            return this->type->to_string() + " ret." +
                   std::to_string(ret.value);
          },
          [](IR::Addr addr) -> std::string { return addr.to_string(); },
          [this](bool b) -> std::string {
            // type::Bool is used to represent -- if the type is missing.
            return type ? (b ? "true" : "false") : "--";
          },
          [](char c) -> std::string {
            return std::to_string(static_cast<i32>(c)) + "_c";
          },
          [](double d) -> std::string { return std::to_string(d) + "_r"; },
          [](i32 n) -> std::string { return std::to_string(n); },
          [this](EnumVal e) -> std::string {
            // TODO this is wrong now that we have enum flags
            return e.value >= this->type->as<type::Enum>().members_.size()
                       ? this->type->as<type::Enum>().to_string() + ":END"
                       : this->type->as<type::Enum>().members_ AT(e.value);
          },
          [](const type::Type *t) -> std::string { return t->to_string(); },
          [](IR::Func *f) -> std::string {
            ASSERT_NE(f, nullptr);
            ASSERT_NE(f->type_, nullptr);
            return "fn." +
                   (f->name == ""
                        ? std::to_string(reinterpret_cast<uintptr_t>(f)) + "-" +
                              f->type_->to_string()
                        : f->name);
          },
          [](AST::ScopeLiteral *s) -> std::string {
            return "scope(" + std::to_string(reinterpret_cast<uintptr_t>(s)) +
                   ")";
          },
          [](const AST::CodeBlock &c) -> std::string { return c.to_string(); },
          [](AST::Expression *) -> std::string { return "<expr>"; },
          [](BlockIndex b) -> std::string {
            return "block #" + std::to_string(b);
          },

          [](const std::string &s) -> std::string {
            return "string \"" + Escaped(s) + "\"";
          },
          [](AST::FunctionLiteral *fn) -> std::string {
            return fn->to_string();
          }},
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
} // namespace IR

IR::Val PtrCallFix(IR::Val v) {
  return !v.type->is<type::Pointer>() ||
                 v.type->as<type::Pointer>().pointee->is_big()
             ? v
             : IR::Load(v);
}
