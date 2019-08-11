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
struct Enum;
struct Flags;
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

struct GenericPhiArgs : public base::Cast<GenericPhiArgs> {
  virtual ~GenericPhiArgs() {}
  virtual std::ostream &print(std::ostream &os) const = 0;
};
template <typename T>
struct PhiArgs : GenericPhiArgs {
  ~PhiArgs() override {}
  absl::flat_hash_map<BlockIndex, RegOr<T>> map_;
  virtual std::ostream &print(std::ostream &os) const {
    return os << base::stringify(map_);
  }
};

struct Cmd {
  template <typename T>
  struct Store {
    RegOr<Addr> addr_;
    RegOr<T> val_;
  };

  struct Array {
    RegOr<int64_t> len_;
    RegOr<type::Type const *> type_;
  };

  struct PtrIncr {
    // TODO maybe store the type here rather than on the cmd because most cmds
    // don't need it.
    RegOr<Addr> ptr_;
    type::Type const *pointee_type_;
    RegOr<int64_t> incr_;
  };
  struct Field {
    RegOr<Addr> ptr_;
    type::Type const *type_;  // Struct or Tuple
    size_t num_;
  };

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

  struct CreateStruct {
    core::Scope const *scope_;
    ast::StructLiteral const *parent_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateStruct const &c) {
      return os << c.scope_ << " " << c.parent_;
    }
  };

  struct CreateStructField {
    Reg struct_;
    RegOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateStructField const &c) {
      if (c.type_.is_reg_) {
        return os << stringify(c.struct_) << " " << stringify(c.type_.reg_);
      } else {
        return os << stringify(c.struct_) << " " << c.type_.val_->to_string();
      }
    }
  };

  struct SetStructFieldName {
    // Implicitly the last element.
    Reg struct_;
    std::string_view name_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           SetStructFieldName const &s) {
      return os << stringify(s.struct_) << " " << s.name_;
    };
  };

  struct AddHashtag {
    Reg struct_;
    ast::Hashtag hashtag_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddHashtag const &a) {
      return os << stringify(a.struct_) << " "
                << static_cast<int>(a.hashtag_.kind_);
    }
  };

  struct LoadSymbol {
    std::string_view name_;
    type::Type const *type_;
  };

  struct AddEnumerator {
    Reg enum_;
    std::string_view name_;
  };

  struct SetEnumerator {
    Reg enum_;
    RegOr<int32_t> val_;
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

  struct AstData {
    ast::Node const *node_;
    Reg ctx_;

    inline friend std::ostream &operator<<(std::ostream &os, AstData ast) {
      return os << ast.node_ << " ctx=" << stringify(ast.ctx_);
    }
  };

  struct AddBc {
    Reg ctx_;
    ast::Declaration const *decl_;
    RegOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os, AddBc const &a) {
      return os << stringify(a.ctx_) << " " << a.decl_ << " " << a.type_;
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
    Reg reg_;
    size_t get_ret_;
    type::Type const *type_;
    ast::StructLiteral const *sl_;
    LoadSymbol load_sym_;
    BlockDef const *block_def_;
    ast::BlockLiteral const *block_lit_;
    CreateScopeDef create_scope_def_;
    AddScopeDefInit add_scope_def_init_;
    AddScopeDefDone add_scope_def_done_;

    CreateStruct create_struct_;
    CreateStructField create_struct_field_;
    SetStructFieldName set_struct_field_name_;
    AddHashtag add_hashtag_;
    AddEnumerator add_enumerator_;
    SetEnumerator set_enumerator_;
    BlockIndex block_index_;
    Call call_;
    PtrIncr ptr_incr_;
    Cmd::Array array_;
    Field field_;
    ::Module *mod_;
    core::Scope const *scope_;

    AddBc add_bc_;

    // TODO names of these are easily mis-spellable and would lead to UB.
    RegOr<bool> bool_arg_;
    RegOr<int8_t> i8_arg_;
    RegOr<int16_t> i16_arg_;
    RegOr<int32_t> i32_arg_;
    RegOr<int64_t> i64_arg_;
    RegOr<uint8_t> u8_arg_;
    RegOr<uint16_t> u16_arg_;
    RegOr<uint32_t> u32_arg_;
    RegOr<uint64_t> u64_arg_;
    RegOr<float> float32_arg_;
    RegOr<double> float64_arg_;
    RegOr<EnumVal> enum_arg_;
    RegOr<FlagsVal> flags_arg_;
    RegOr<type::Type const *> type_arg_;
    RegOr<std::string_view> byte_view_arg_;
    RegOr<Addr> addr_arg_;

    SpecialMember<1> special1_;
    SpecialMember<2> special2_;
    Args<FlagsVal> flags_args_;

    AstData ast_;

    Store<type::Type const *> store_type_;

    PhiArgs<bool> *phi_bool_;
    PhiArgs<int8_t> *phi_i8_;
    PhiArgs<int16_t> *phi_i16_;
    PhiArgs<int32_t> *phi_i32_;
    PhiArgs<int64_t> *phi_i64_;
    PhiArgs<uint8_t> *phi_u8_;
    PhiArgs<uint16_t> *phi_u16_;
    PhiArgs<uint32_t> *phi_u32_;
    PhiArgs<uint64_t> *phi_u64_;
    PhiArgs<float> *phi_float32_;
    PhiArgs<double> *phi_float64_;
    PhiArgs<type::Type const *> *phi_type_;
    PhiArgs<BlockDef *> *phi_block_;
    PhiArgs<ir::Addr> *phi_addr_;
    PhiArgs<ir::EnumVal> *phi_enum_;
    PhiArgs<ir::FlagsVal> *phi_flags_;
    PhiArgs<ir::AnyFunc> *phi_func_;

    RegOr<AnyFunc> any_fn_;
  };

  Reg result;
};

RegOr<int64_t> Bytes(RegOr<type::Type const *> r);
RegOr<int64_t> Align(RegOr<type::Type const *> r);
void DebugIr();

RegOr<type::Type const *> Array(RegOr<int64_t> len,
                                     RegOr<type::Type const *> data_type);
Reg VariantType(RegOr<Addr> r);
Reg VariantValue(const type::Type *t, RegOr<Addr> r);
// Type repreesents the type of `ptr`
TypedRegister<Addr> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                            type::Pointer const *t);
type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const *t,
                            size_t n);
type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const *t, size_t n);

Cmd &MakeCmd(type::Type const *t, Op op);

void Call(RegOr<AnyFunc> const &f, Arguments arguments);
void Call(RegOr<AnyFunc> const &f, Arguments arguments, OutParams outs);
std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const &block_map);

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod);

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegOr<int64_t> offset);
TypedRegister<Addr> Alloca(type::Type const *t);
TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx);

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type);

TypedRegister<Addr> GetRet(size_t n, type::Type const *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);

void Move(type::Type const *t, Reg from, RegOr<Addr> to);
void Copy(type::Type const *t, Reg from, RegOr<Addr> to);
void Destroy(type::Type const *t, Reg r);
void Init(type::Type const *t, Reg r);

void VerifyType(ast::Node const *node, Reg ctx);
Reg EvaluateAsType(ast::Node const *node, Reg ctx);

Reg CreateContext(Module *mod);
void AddBoundConstant(Reg ctx, ast::Declaration const *decl,
                      RegOr<type::Type const *> type);
void DestroyContext(Reg r);
void JumpPlaceholder(BlockDef const *block_def);

Reg ArgumentCache(ast::StructLiteral const *sl);

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
