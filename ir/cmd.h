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
struct Pointer;
}  // namespace type

namespace ir {
struct Arguments;

enum class Op : uint16_t {
#define OP_MACRO(op, ...) op,
#include "ir/op.xmacro.h"
#undef OP_MACRO
};

struct Cmd {
  Cmd(type::Type const *t, Op op);
  Op op_code_;

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

  struct CreateScopeDef {
    ::Module *mod_;
    ScopeDef *scope_def_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateScopeDef const &c) {
      return os << c.mod_ << " " << c.scope_def_;
    }
  };

  union {
    size_t get_ret_;
    ast::BlockLiteral const *block_lit_;
    CreateScopeDef create_scope_def_;
    AddScopeDefInit add_scope_def_init_;
    AddScopeDefDone add_scope_def_done_;

    BlockIndex block_index_;
    ::Module *mod_;
    core::Scope const *scope_;

    RegOr<std::string_view> byte_view_arg_;
    RegOr<AnyFunc> any_fn_;
  };

  Reg result;
};

Reg Reserve(core::Bytes b, core::Alignment a);
Reg Reserve(type::Type const *);

std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const &block_map);

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegOr<int64_t> offset);
TypedRegister<Addr> Alloca(type::Type const *t);
TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx);

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
