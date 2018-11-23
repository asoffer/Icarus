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

namespace ir {

// TODO this stores way more than is needed. It'd be nice to have a way to say
// when you're done with these. this could be done by code manually
// incrementing/decrementing counters because this is compile-time only.
static base::guarded<std::set<base::vector<ast::BlockLiteral *>>> seqs;
Val Val::Block(ast::BlockLiteral *b) {
  auto handle         = seqs.lock();
  auto[iter, success] = handle->insert(base::vector<ast::BlockLiteral *>{b});
  return Val::BlockSeq(BlockSequence{&*iter});
}

BlockSequence MakeBlockSeq(const base::vector<ir::BlockSequence> &blocks) {
  base::vector<ast::BlockLiteral *> seq;
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

Val Val::BlockSeq(BlockSequence b) {
  ASSERT(b.seq_->size() != 0u);
  auto *t = (b.seq_->back() == nullptr ||
             b.seq_->back() == reinterpret_cast<ast::BlockLiteral *>(0x1))
                ? type::Block
                : b.seq_->back()->required_ ? type::Block : type::OptBlock;
  return Val(t, b);
}

Val Val::Interface(ir::Interface ifc) {
  return Val(type::Interface, std::move(ifc));
}

Val Val::Ref(ast::Expression *expr) { NOT_YET(); }

Val::Val(ast::ScopeLiteral *scope_lit) : Val(type::Scope, scope_lit) {}

Val Val::Enum(const type::Enum *enum_type, size_t integral_val) {
  return Val(enum_type, EnumVal{integral_val});
}

Val Val::Flags(const type::Flags *flags_type, FlagsVal val) {
  return Val(flags_type, val);
}

Val Val::Func(ast::FunctionLiteral *fn) { return Val(type::Generic, fn); }

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
          [](ir::Addr addr) -> std::string { return addr.to_string(); },
          [this](bool b) -> std::string {
            // type::Bool is used to represent -- if the type is missing.
            return type ? (b ? "true" : "false") : "--";
          },
          [](char c) -> std::string {
            return std::to_string(static_cast<i32>(c)) + "_c";
          },
          [](float f) -> std::string { return std::to_string(f) + "_f32"; },
          [](double d) -> std::string { return std::to_string(d) + "_f64"; },
          [](i8 n) -> std::string { return std::to_string(n); },
          [](i16 n) -> std::string { return std::to_string(n); },
          [](i32 n) -> std::string { return std::to_string(n); },
          [](i64 n) -> std::string { return std::to_string(n); },
          [](u8 n) -> std::string { return std::to_string(n); },
          [](u16 n) -> std::string { return std::to_string(n); },
          [](u32 n) -> std::string { return std::to_string(n); },
          [](u64 n) -> std::string { return std::to_string(n); },
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
          [](AnyFunc a) -> std::string {
            std::stringstream ss;
            ss << a;
            return ss.str();
          },
          [](ast::ScopeLiteral *s) -> std::string {
            return "scope(" + std::to_string(reinterpret_cast<uintptr_t>(s)) +
                   ")";
          },
          [](ast::Expression *) -> std::string { return "<expr>"; },
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
          [](const ir::Interface &ifc) -> std::string {
            // TODO
            return "Interface";
          },
          [](ir::BuiltinGenericIndex n) -> std::string {
            return "builtin(" + n.to_string() + ")";
          },
          [](ForeignFn f) -> std::string {
            return "foreign(" + std::string(f.name()) + ")";
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

bool operator<(const ::ir::Val &lhs, const ::ir::Val &rhs) {
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

Val ValFrom(RegisterOr<ir::Addr> r, type::Pointer const *ptr_type) {
  return r.is_reg_ ? Val::Reg(r.reg_, ptr_type) : Val(ptr_type, r.val_);
}

}  // namespace ir
