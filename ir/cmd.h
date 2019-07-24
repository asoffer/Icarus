#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "absl/container/flat_hash_map.h"
#include "ast/hashtag.h"
#include "base/untyped_buffer.h"
#include "ir/block.h"
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
struct Variant;
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
  absl::flat_hash_map<BlockIndex, RegisterOr<T>> map_;
  virtual std::ostream &print(std::ostream &os) const {
    return os << base::stringify(map_);
  }
};

struct Cmd {
  template <typename T>
  struct Store {
    RegisterOr<Addr> addr_;
    RegisterOr<T> val_;
  };

  template <typename T>
  struct SetRet {
    size_t ret_num_;
    RegisterOr<T> val_;
  };

  struct PrintEnum {
    RegisterOr<EnumVal> arg_;
    type::Enum const *enum_type_;
  };

  struct PrintFlags {
    RegisterOr<FlagsVal> arg_;
    type::Flags const *flags_type_;
  };

  struct Array {
    RegisterOr<int64_t> len_;
    RegisterOr<type::Type const *> type_;
  };

  struct PtrIncr {
    // TODO maybe store the type here rather than on the cmd because most cmds
    // don't need it.
    RegisterOr<Addr> ptr_;
    type::Type const *pointee_type_;
    RegisterOr<int64_t> incr_;
  };
  struct Field {
    RegisterOr<Addr> ptr_;
    type::Type const *type_;  // Struct or Tuple
    size_t num_;
  };

  struct Call {
    Call(RegisterOr<AnyFunc> f, Arguments *args, OutParams *outs)
        : fn_(f), arguments_(args), outs_(outs) {}
    RegisterOr<AnyFunc> fn_;
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
    std::array<RegisterOr<T>, 2> args_;
  };

  struct Empty {
    inline friend std::ostream &operator<<(std::ostream &os, Empty) {
      return os;
    }
  };

  struct AddScopeDefInit {
    Reg reg_;
    RegisterOr<AnyFunc> f_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddScopeDefInit a) {
      return os << a.reg_ << " " << a.f_;
    }
  };

  struct AddScopeDefDone {
    Reg reg_;
    RegisterOr<AnyFunc> f_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddScopeDefDone a) {
      return os << a.reg_ << " " << a.f_;
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
    RegisterOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateStructField const &c) {
      if (c.type_.is_reg_) {
        return os << c.struct_ << " " << c.type_.reg_;
      } else {
        return os << c.struct_ << " " << c.type_.val_->to_string();
      }
    }
  };

  struct SetStructFieldName {
    // Implicitly the last element.
    Reg struct_;
    std::string_view name_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           SetStructFieldName const &s) {
      return os << s.struct_ << " " << s.name_;
    };
  };

  struct AddHashtag {
    Reg struct_;
    ast::Hashtag hashtag_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddHashtag const &a) {
      return os << a.struct_ << " " << static_cast<int>(a.hashtag_.kind_);
    }
  };

  struct CondJump {
    Reg cond_;
    BlockIndex blocks_[2];
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
    RegisterOr<int32_t> val_;
  };

  template <size_t N>
  struct SpecialMember {
    type::Type const *type_;
    std::array<RegisterOr<Addr>, N> regs_;

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
      return os << ast.node_ << " ctx=" << ast.ctx_;
    }
  };

  struct AddBc {
    Reg ctx_;
    ast::Declaration const *decl_;
    RegisterOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os, AddBc const &a) {
      return os << a.ctx_ << " " << a.decl_ << " " << a.type_;
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
    CondJump cond_jump_;
    BlockIndex block_index_;
    Call call_;
    PtrIncr ptr_incr_;
    Cmd::Array array_;
    Field field_;
    ::Module *mod_;
    core::Scope const *scope_;

    AddBc add_bc_;

    // TODO names of these are easily mis-spellable and would lead to UB.
    RegisterOr<bool> bool_arg_;
    RegisterOr<int8_t> i8_arg_;
    RegisterOr<int16_t> i16_arg_;
    RegisterOr<int32_t> i32_arg_;
    RegisterOr<int64_t> i64_arg_;
    RegisterOr<uint8_t> u8_arg_;
    RegisterOr<uint16_t> u16_arg_;
    RegisterOr<uint32_t> u32_arg_;
    RegisterOr<uint64_t> u64_arg_;
    RegisterOr<float> float32_arg_;
    RegisterOr<double> float64_arg_;
    RegisterOr<EnumVal> enum_arg_;
    RegisterOr<FlagsVal> flags_arg_;
    RegisterOr<type::Type const *> type_arg_;
    RegisterOr<std::string_view> byte_view_arg_;
    RegisterOr<Addr> addr_arg_;

    SpecialMember<1> special1_;
    SpecialMember<2> special2_;
    Args<FlagsVal> flags_args_;
    Args<type::Type const *> type_args_;

    AstData ast_;

    // TODO rename these since some of them are used for things other than
    // storage (e.g., block appending).
    Store<bool> store_bool_;
    Store<int8_t> store_i8_;
    Store<int16_t> store_i16_;
    Store<int32_t> store_i32_;
    Store<int64_t> store_i64_;
    Store<uint8_t> store_u8_;
    Store<uint16_t> store_u16_;
    Store<uint32_t> store_u32_;
    Store<uint64_t> store_u64_;
    Store<float> store_float32_;
    Store<double> store_float64_;
    Store<type::Type const *> store_type_;
    Store<EnumVal> store_enum_;
    Store<AnyFunc> store_func_;
    Store<FlagsVal> store_flags_;
    Store<Addr> store_addr_;

    SetRet<bool> set_ret_bool_;
    SetRet<int8_t> set_ret_i8_;
    SetRet<int16_t> set_ret_i16_;
    SetRet<int32_t> set_ret_i32_;
    SetRet<int64_t> set_ret_i64_;
    SetRet<uint8_t> set_ret_u8_;
    SetRet<uint16_t> set_ret_u16_;
    SetRet<uint32_t> set_ret_u32_;
    SetRet<uint64_t> set_ret_u64_;
    SetRet<float> set_ret_float32_;
    SetRet<double> set_ret_float64_;
    SetRet<type::Type const *> set_ret_type_;
    SetRet<EnumVal> set_ret_enum_;
    SetRet<AnyFunc> set_ret_func_;
    SetRet<FlagsVal> set_ret_flags_;
    SetRet<Addr> set_ret_addr_;
    SetRet<std::string_view> set_ret_byte_view_;
    SetRet<ScopeDef *> set_ret_scope_;
    SetRet<ast::FunctionLiteral *> set_ret_generic_;
    SetRet<Module *> set_ret_module_;
    SetRet<BlockDef *> set_ret_block_;

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

    PrintEnum print_enum_;
    PrintFlags print_flags_;

    RegisterOr<AnyFunc> any_fn_;
    type::Typed<Reg> typed_reg_;
#define OP_MACRO(...)
#include "ir/op.xmacro.h"
#undef OP_MACRO
  };

  Reg result;
};

RegisterOr<int64_t> Bytes(RegisterOr<type::Type const *> r);
RegisterOr<int64_t> Align(RegisterOr<type::Type const *> r);
RegisterOr<bool> Not(RegisterOr<bool> r);
RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r);
RegisterOr<FlagsVal> XorFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs);
RegisterOr<FlagsVal> OrFlags(type::Flags const *type,
                             RegisterOr<FlagsVal> const &lhs,
                             RegisterOr<FlagsVal> const &rhs);
RegisterOr<FlagsVal> AndFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs);

void DebugIr();

template <typename T, typename... Args>
TypedRegister<T> Load(RegisterOr<Addr> r,
                      type::Type const *t = type::Get<T>()) {
  auto &cmd     = MakeCmd(t, Cmd::OpCode<Cmd::LoadTag, T>());
  cmd.addr_arg_ = r;
  return cmd.result;
}
Reg Load(RegisterOr<Addr> r, type::Type const *t);

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> in,
                                     RegisterOr<type::Type const *> out);
RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r);
RegisterOr<type::Type const *> BufPtr(RegisterOr<type::Type const *> r);

RegisterOr<type::Type const *> Array(RegisterOr<int64_t> len,
                                     RegisterOr<type::Type const *> data_type);
Reg VariantType(RegisterOr<Addr> r);
Reg VariantValue(const type::Type *t, RegisterOr<Addr> r);
// Type repreesents the type of `ptr`
TypedRegister<Addr> PtrIncr(RegisterOr<Addr> ptr, RegisterOr<int64_t> inc,
                            type::Pointer const *t);
type::Typed<Reg> Field(RegisterOr<Addr> r, type::Struct const *t,
                            size_t n);
type::Typed<Reg> Field(RegisterOr<Addr> r, type::Tuple const *t, size_t n);

Cmd &MakeCmd(type::Type const *t, Op op);

template <typename T>
void SetRet(size_t n, T t) {
  if constexpr (!IsRegOr<T>::value) {
    return SetRet(n, RegisterOr<T>(t));
  } else {
    auto &cmd =
        MakeCmd(nullptr, Cmd::OpCode<Cmd::SetRetTag, typename T::type>());
    cmd.template set<Cmd::SetRetTag, typename T::type>(n, t);
    // TODO reenable if (r.is_reg_) {
    // CompiledFn::Current->references_[r.reg_].insert(cmd.result); }
  }
}
void SetRet(size_t n, type::Typed<Results> const &v2, Context *ctx = nullptr);

template <typename T, typename... Args>
void Print(T r, Args &&... args) {
  if constexpr (IsRegOr<T>::value) {
    using type = typename T::type;
    auto &cmd  = MakeCmd(nullptr, Cmd::OpCode<Cmd::PrintTag, type>());
    if constexpr (std::is_same_v<type, EnumVal> ||
                  std::is_same_v<type, FlagsVal>) {
      cmd.template set<Cmd::PrintTag, type>(r, std::forward<Args>(args)...);
    } else {
      static_assert(sizeof...(Args) == 0);
      cmd.template get<Cmd::PrintTag, type>() = r;
    }
  } else {
    return Print(RegisterOr<T>(r), std::forward<Args>(args)...);
  }
}

template <typename T, typename... Args>
void Store(T r, Args &&... args) {
  if constexpr (IsRegOr<T>::value) {
    using type = typename T::type;
    auto &cmd  = MakeCmd(nullptr, Cmd::OpCode<Cmd::StoreTag, type>());
    // TODO reverse all call sites
    cmd.template set<Cmd::StoreTag, type>(std::forward<Args>(args)..., r);
  } else if constexpr (IsTypedReg<T>::value) {
    return Store(RegisterOr<typename T::type>(r), std::forward<Args>(args)...);
  } else {
    return Store(RegisterOr<T>(r), std::forward<Args>(args)...);
  }
}

void Call(RegisterOr<AnyFunc> const &f, Arguments arguments);
void Call(RegisterOr<AnyFunc> const &f, Arguments arguments, OutParams outs);
std::pair<Results, bool> CallInline(
    CompiledFn *f, Arguments const &arguments,
    absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> const &block_map);

Reg CreateTuple();
void AppendToTuple(Reg tup, RegisterOr<type::Type const *> entry);
Reg FinalizeTuple(Reg tup);
Reg CreateVariant();
void AppendToVariant(Reg tup, RegisterOr<type::Type const *> entry);
Reg FinalizeVariant(Reg var);
void CondJump(RegisterOr<bool> cond, BlockIndex true_block,
              BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod);

Results Cast(type::Type const *from, type::Type const *to, Results const &val);

TypedRegister<Addr> Index(type::Pointer const *t, Reg array_ptr,
                          RegisterOr<int64_t> offset);
TypedRegister<Addr> Alloca(type::Type const *t);
TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx);

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type);

TypedRegister<Addr> GetRet(size_t n, type::Type const *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);

void Move(type::Type const *t, Reg from, RegisterOr<Addr> to);
void Copy(type::Type const *t, Reg from, RegisterOr<Addr> to);
void Destroy(type::Type const *t, Reg r);
void Init(type::Type const *t, Reg r);

void VerifyType(ast::Node const *node, Reg ctx);
Reg EvaluateAsType(ast::Node const *node, Reg ctx);

Reg CreateContext(Module *mod);
void AddBoundConstant(Reg ctx, ast::Declaration const *decl,
                      RegisterOr<type::Type const *> type);
void DestroyContext(Reg r);
void JumpPlaceholder(BlockDef const *block_def);

Reg ArgumentCache(ast::StructLiteral const *sl);

Reg CreateScopeDef(::Module const *mod, ScopeDef *scope_def);
void AddScopeDefInit(Reg reg, RegisterOr<AnyFunc> f);
void AddScopeDefDone(Reg reg,  RegisterOr<AnyFunc> f);
void FinishScopeDef();

Reg CreateBlockDef(ast::BlockLiteral const *parent);
void AddBlockDefBefore(RegisterOr<AnyFunc> f);
void AddBlockDefAfter(RegisterOr<AnyFunc> f);
void FinishBlockDef(std::string_view name);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
