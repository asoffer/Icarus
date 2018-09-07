#include "ir/val.h"

#include <sstream>
#include <unordered_set>

#include "ast/block_literal.h"
#include "ast/function_literal.h"
#include "ast/scope_literal.h"
#include "base/guarded.h"
#include "ir/func.h"
#include "type/char_buffer.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"

namespace IR {
Val Val::CodeBlock(AST::CodeBlock block) { NOT_YET(); }

// TODO this stores way more than is needed. It'd be nice to have a way to say
// when you're done with these. this could be done by code manually
// incrementing/decrementing counters because this is compile-time only.
static base::guarded<std::set<base::vector<AST::BlockLiteral *>>> seqs;
Val Val::Block(AST::BlockLiteral *b) {
  auto handle = seqs.lock();
  auto[iter, success] = handle->insert(base::vector<AST::BlockLiteral *>{b});
  return Val::BlockSeq(BlockSequence{&*iter});
}

BlockSequence MakeBlockSeq(const base::vector<IR::BlockSequence> &blocks) {
  base::vector<AST::BlockLiteral *> seq;
  for (const auto &bseq : blocks) {
    seq.insert(seq.end(), bseq.seq_->begin(), bseq.seq_->end());
  }

  auto handle         = seqs.lock();
  auto[iter, success] = handle->insert(std::move(seq));
  return BlockSequence{&*iter};
}

static base::guarded<std::unordered_set<std::string>> GlobalStringSet;
std::string_view SaveStringGlobally(std::string const &str) {
  auto handle         = GlobalStringSet.lock();
  auto[iter, success] = handle->insert(str);
  return std::string_view(*iter);
}

Val Val::CharBuf(const std::string &str) {
  auto sv = SaveStringGlobally(str);
  return Val(type::CharBuf(sv.size()), sv);
}

Val Val::Struct() { return Val(type::Type_, new type::Struct); }

Val Val::BlockSeq(BlockSequence b) {
  ASSERT(b.seq_->size() != 0u);
  auto *t = (b.seq_->back() == nullptr) ? type::Block : b.seq_->back()->type;
  return Val(t, b);
}

Val Val::Interface(IR::Interface ifc) {
  return Val(type::Interface, std::move(ifc));
}

Val Val::Ref(AST::Expression *expr) { return Val(expr->type, expr); }

Val::Val(AST::ScopeLiteral *scope_lit) : Val(scope_lit->type, scope_lit) {}

Val Val::Enum(const type::Enum *enum_type, size_t integral_val) {
  return Val(enum_type, EnumVal{integral_val});
}

Val Val::Flags(const type::Flags *flags_type, FlagsVal val) {
  return Val(flags_type, val);
}

Val Val::Func(AST::Function *fn) { return Val(fn->type, fn); }
Val Val::Func(IR::Func *fn) { return Val(fn->type_, fn); }

Val Val::Null(const type::Type *t) {
  return Val(Ptr(t), IR::Addr{Addr::Kind::Null, 0});
}

static std::string Escaped(std::string_view sv) {
  std::stringstream ss;
  for (char c : sv) {
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
            return this->type->to_string() + " " + reg.to_string();
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
            return e.value >= this->type->as<type::Enum>().members_.size()
                       ? this->type->as<type::Enum>().to_string() + ":END"
                       : this->type->as<type::Enum>().members_.at(e.value);
          },
          [this](FlagsVal f) -> std::string {
            return f.value >=
                           (1u << this->type->as<type::Flags>().members_.size())
                       ? this->type->as<type::Flags>().to_string() + ":END"
                       : this->type->as<type::Flags>().members_.at(f.value);
          },
          [](const type::Type *t) -> std::string { return t->to_string(); },
          [](IR::Func *f) -> std::string {
            ASSERT(f != nullptr);
            ASSERT(f->type_ != nullptr);
            return "fn." + f->name() + "-" + f->type_->to_string();
          },
          [](AST::ScopeLiteral *s) -> std::string {
            return "scope(" + std::to_string(reinterpret_cast<uintptr_t>(s)) +
                   ")";
          },
          [](AST::Expression *) -> std::string { return "<expr>"; },
          [](BlockIndex b) -> std::string { return b.to_string(); },
          [](std::string_view sv) -> std::string {
            return "\"" + Escaped(sv) + "\"";
          },
          [](const Module *module) -> std::string {
            // TODO
            return "module";
          },
          [](BlockSequence bs) -> std::string {
            // TODO
            return "bs." + std::to_string(reinterpret_cast<uintptr_t>(bs.seq_));
          },
          [](const IR::Interface &ifc) -> std::string {
            // TODO
            return "Interface";
          },
          [](IR::BuiltinGenericIndex n) -> std::string {
            return "builtin(" + n.to_string() + ")";
          },
          [](ForeignFn f) -> std::string {
            return "foreign(" + std::string(f.name_) + ")";
          }},
      value);
}

std::string Addr::to_string() const {
  std::stringstream ss;
  switch (kind) {
  case Kind::Null: ss << "null"; break;
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
  case Addr::Kind::Heap: return lhs.as_heap == rhs.as_heap;
  }
  UNREACHABLE();
}

bool operator<(const ::IR::Val &lhs, const ::IR::Val &rhs) {
  auto lhs_index = lhs.value.index();
  auto rhs_index = rhs.value.index();
  if (lhs_index < rhs_index) { return true; }
  if (lhs_index > rhs_index) { return false; }
  const auto &rhs_val = rhs.value;
  return std::visit(
      [&rhs_val](const auto &l) -> bool {
        return l < std::get<std::decay_t<decltype(l)>>(rhs_val);
      },
      lhs.value);
}

Val ValFrom(RegisterOr<FlagsVal> r, type::Flags const *t) {
  return r.is_reg_ ? Val::Reg(r.reg_, t) : Val::Flags(t, r.val_);
}

Val ValFrom(RegisterOr<IR::Addr> r, type::Pointer const *ptr_type) {
  return r.is_reg_ ? Val::Reg(r.reg_, ptr_type) : Val(ptr_type, r.val_);
}

}  // namespace IR

IR::Val PtrCallFix(const IR::Val &v) {
  return !v.type->is<type::Pointer>() ||
                 v.type->as<type::Pointer>().pointee->is_big()
             ? v
             : IR::Val::Reg(IR::Load(std::get<IR::Register>(v.value),
                                     v.type->as<type::Pointer>().pointee),
                            v.type->as<type::Pointer>().pointee);
}
