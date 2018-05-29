#ifndef ICARUS_IR_VAL_H
#define ICARUS_IR_VAL_H

#include <functional>
#include <iostream>
#include <memory>
#include <variant>

#include "ast/codeblock.h"
#include "base/strong_types.h"
#include "base/types.h"
#include "ir/block_sequence.h"

struct Module;

namespace type {
struct Enum;
struct Flags;
struct Pointer;
struct Struct;
struct Type;

const Type *Void();
extern Type *Bool, *Char, *Real, *Int, *Type_, *String, *Module;
} // namespace type


namespace AST {
struct Expression;
struct ScopeLiteral;
struct BlockLiteral;
struct Function;
} // namespace AST

namespace IR {
DEFINE_STRONG_INT(BlockIndex, i32, -1);
DEFINE_STRONG_INT(EnumVal, size_t, 0);
DEFINE_STRONG_INT(FlagsVal, size_t, 0);
DEFINE_STRONG_INT(Register, i32, std::numeric_limits<i32>::lowest());

struct CmdIndex {
  BlockIndex block;
  i32 cmd;
};

inline bool operator==(CmdIndex lhs, CmdIndex rhs) {
  return lhs.block == rhs.block && lhs.cmd == rhs.cmd;
}
inline bool operator<(CmdIndex lhs, CmdIndex rhs) {
  if (lhs.block.value < rhs.block.value) return true;
  if (lhs.block.value > rhs.block.value) return false;
  return lhs.cmd < rhs.cmd;
}

struct Addr {
  enum class Kind : u8 { Null, Global, Stack, Heap } kind;
  union {
    u64 as_global;
    u64 as_stack;
    void *as_heap;
  };

  std::string to_string() const;
};

bool operator==(Addr lhs, Addr rhs);
inline bool operator!=(Addr lhs, Addr rhs) { return !(lhs == rhs); }
inline bool operator<(Addr lhs, Addr rhs) {
  u8 lhs_kind = static_cast<u8>(lhs.kind);
  u8 rhs_kind = static_cast<u8>(rhs.kind);
  if (lhs_kind < rhs_kind) { return true; }
  if (lhs_kind > rhs_kind) { return false; }
  switch (lhs.kind) {
  case Addr::Kind::Null: return false;
  case Addr::Kind::Global: return lhs.as_global < rhs.as_global;
  case Addr::Kind::Stack: return lhs.as_stack < rhs.as_stack;
  case Addr::Kind::Heap: return lhs.as_heap < rhs.as_heap;
  }
  UNREACHABLE();
}
inline bool operator<=(Addr lhs, Addr rhs) { return !(rhs < lhs); }
inline bool operator>(Addr lhs, Addr rhs) { return rhs < lhs; }
inline bool operator>=(Addr lhs, Addr rhs) { return !(lhs < rhs); }

struct Func;
} // namespace IR

DEFINE_STRONG_HASH(IR::BlockIndex);
DEFINE_STRONG_HASH(IR::Register);

namespace std {
template <> struct hash<IR::CmdIndex> {
  size_t operator()(const IR::CmdIndex &cmd_index) const noexcept {
    u64 num = (static_cast<u64>(static_cast<u32>(cmd_index.block.value)) << 32);
    num |= static_cast<u32>(cmd_index.cmd);
    return hash<u64>()(num);
  }
};
} // namespace std

namespace IR {
struct Val {
  const type::Type *type = nullptr;
  std::variant<
      Register, IR::Addr, bool, char, double, i32, EnumVal, FlagsVal,
      const type::Type *, type::Struct *, IR::Func *, AST::Function *,
      AST::ScopeLiteral *, AST::CodeBlock, AST::Expression *, BlockIndex,
      std::string, const Module *,
      AST::BlockLiteral *,  // TODO no longer necessary with blocksequence?
      std::vector<Val>, BlockSequence>
      value{false};

  static Val Reg(Register r, const type::Type *t) { return Val(t, r); }
  static Val Addr(Addr addr, const type::Type *t);
  static Val GlobalAddr(u64 addr, const type::Type *t);
  static Val HeapAddr(void *addr, const type::Type *t);
  static Val StackAddr(u64 addr, const type::Type *t);
  static Val Bool(bool b) { return Val(type::Bool, b); }
  static Val Char(char c) { return Val(type::Char, c); }
  static Val Real(double r) { return Val(type::Real, r); }
  static Val Int(i32 n) { return Val(type::Int, n); }
  static Val Enum(const type::Enum *enum_type, size_t integral_val);
  static Val Flags(const type::Flags *flags_type, size_t integral_val);
  static Val Type(const type::Type *t) { return Val(type::Type_, t); }
  static Val CodeBlock(AST::CodeBlock block);
  static Val Func(IR::Func *fn); // TODO deprecate?
  static Val Func(AST::Function* fn);
  static Val BasicBlock(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Block(AST::BlockLiteral *b);
  static Val BlockSeq(BlockSequence b);
  static Val Void() { return Val(type::Void(), false); }
  static Val Mod(const Module *mod) { return Val(type::Module, mod); }
  static Val Null(const type::Type *t);
  static Val NullPtr();
  static Val Many(std::vector<IR::Val> vals) {
    return Val(nullptr, std::move(vals));
  }

  static Val StrLit(std::string str) { return Val(type::String, std::move(str)); }
  static Val Ref(AST::Expression *expr);
  static Val None() { return Val(); }
  static Val Scope(AST::ScopeLiteral *scope_lit);
  static Val Struct();

  std::string to_string() const;

  Val()                = default;
  ~Val() noexcept      = default;
  Val(const Val &)     = default;
  Val(Val &&) noexcept = default;
  Val &operator=(const Val &) = default;
  Val &operator=(Val &&) noexcept = default;

private:
  template <typename T>
  Val(const type::Type *t, T &&val) : type(t), value(std::forward<T>(val)) {}
};

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }
bool operator<(const ::IR::Val &lhs, const ::IR::Val &rhs);
} // namespace IR

#endif // ICARUS_IR_VAL_H
