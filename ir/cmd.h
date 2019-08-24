#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "absl/container/flat_hash_map.h"
#include "ast/hashtag.h"
#include "base/untyped_buffer.h"
#include "ir/block.h"
#include "ir/reg.h"
#include "ir/register.h"
#include "ir/results.h"
#include "misc/context.h"
#include "type/util.h"

namespace core {
struct Scope;
}  // namespace core

namespace type {
struct Function;
struct GenericStruct;
struct Pointer;
struct Struct;
struct Tuple;
}  // namespace type

namespace ast {
struct StructLiteral;
struct ScopeNode;
struct Function;
}  // namespace ast

namespace ir {
struct Arguments;
struct OutParams;

enum class Op : uint16_t {
#define OP_MACRO(op, ...) op,
#include "ir/op.xmacro.h"
#undef OP_MACRO
};
char const *OpCodeStr(Op op);

struct Cmd {
  struct Call {
    Call(RegOr<AnyFunc> f, Arguments *args, OutParams *outs)
        : fn_(f), arguments_(args), outs_(outs) {}
    RegOr<AnyFunc> fn_;
    Arguments *arguments_;
    OutParams *outs_;
  };

#define OP_MACRO(op, tag, ...) struct tag##Tag;
#include "ir/op.xmacro.h"
#undef OP_MACRO
  template <typename Tag, typename T>
  static constexpr Op OpCode() {
#define OP_MACRO(op, tag, type, field)                                         \
  if constexpr (std::is_same_v<Tag, tag##Tag> && std::is_same_v<T, type>) {    \
    return Op::op;                                                             \
  }
#include "ir/op.xmacro.h"
#undef OP_MACRO
    UNREACHABLE();
  }

  template <typename Tag, typename T>
  constexpr auto &get() {
    static_assert(!std::is_same_v<T, void>);
#define OP_MACRO(op, tag, type, field)                                         \
  if constexpr (std::is_same_v<Tag, tag##Tag> && std::is_same_v<T, type>) {    \
    return field;                                                              \
  }
#include "ir/op.xmacro.h"
#undef OP_MACRO
    UNREACHABLE();
  }

  template <typename Tag, typename T, typename... Args>
  void set(Args &&... args) {
    auto &cmd   = this->template get<Tag, T>();
    using cmd_t = std::decay_t<decltype(cmd)>;
    cmd         = cmd_t{std::forward<Args>(args)...};
  }

  Cmd(type::Type const *t, Op op);
  Op op_code_;

  template <typename T>
  struct Args {
    std::array<RegOr<T>, 2> args_;
  };

  struct Empty {
    inline friend std::ostream &operator<<(std::ostream &os, Empty) {
      return os;
    }
  };

  struct AddScopeDefInit {
    Reg reg_;
    RegOr<AnyFunc> f_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddScopeDefInit a) {
      return os << stringify(a.reg_) << " " << a.f_;
    }
  };

  struct AddScopeDefDone {
    Reg reg_;
    RegOr<AnyFunc> f_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddScopeDefDone a) {
      return os << stringify(a.reg_) << " " << a.f_;
    }
  };

  template <size_t N>
  struct SpecialMember {
    type::Type const *type_;
    std::array<RegOr<Addr>, N> regs_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           SpecialMember<N> const &sm) {
      os << sm.type_->to_string();
      for (size_t i = 0; i < N; ++i) { os << " " << sm.regs_[i]; }
      return os;
    }
  };

  struct CreateScopeDef {
    ::Module *mod_;
    ScopeDef *scope_def_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateScopeDef const &c) {
      return os << c.mod_ << " " << c.scope_def_;
    }
  };

  union {
    Empty empty_;
    size_t get_ret_;
    BlockDef const *block_def_;
    ast::BlockLiteral const *block_lit_;
    CreateScopeDef create_scope_def_;
    AddScopeDefInit add_scope_def_init_;
    AddScopeDefDone add_scope_def_done_;

    BlockIndex block_index_;
    Call call_;
    ::Module *mod_;
    core::Scope const *scope_;

    RegOr<std::string_view> byte_view_arg_;

    RegOr<AnyFunc> any_fn_;
  };

  Reg result;
};

Reg Reserve(core::Bytes b, core::Alignment a);
Reg Reserve(type::Type const *);

// Type repreesents the type of `ptr`
Cmd &MakeCmd(type::Type const *t, Op op);

void Call(RegOr<AnyFunc> const &f, Arguments arguments);
void Call(RegOr<AnyFunc> const &f, Arguments arguments, OutParams outs);
std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const &block_map);

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegOr<int64_t> offset);
TypedRegister<Addr> Alloca(type::Type const *t);
TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx);

TypedRegister<Addr> GetRet(size_t n, type::Type const *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);

void JumpPlaceholder(BlockDef const *block_def);

Reg CreateScopeDef(::Module const *mod, ScopeDef *scope_def);
void AddScopeDefInit(Reg reg, RegOr<AnyFunc> f);
void AddScopeDefDone(Reg reg,  RegOr<AnyFunc> f);
void FinishScopeDef();

Reg CreateBlockDef(ast::BlockLiteral const *parent);
void AddBlockDefBefore(RegOr<AnyFunc> f);
void AddBlockDefAfter(RegOr<AnyFunc> f);
void FinishBlockDef(std::string_view name);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
