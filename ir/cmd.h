#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "ast/hashtag.h"
#include "base/untyped_buffer.h"
#include "ir/results.h"
#include "misc/context.h"

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
struct ScopeLiteral;
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
};
template <typename T>
struct PhiArgs : GenericPhiArgs {
  ~PhiArgs() override {}
  absl::flat_hash_map<BlockIndex, RegisterOr<T>> map_;
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

  struct BlockSeqContains {
    Register reg_;
    ast::BlockLiteral *lit_;
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

  struct CreateStruct {
    core::Scope const *scope_;
    ast::StructLiteral const *parent_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateStruct const &c) {
      return os << c.scope_ << " " << c.parent_;
    }
  };

  struct CreateStructField {
    Register struct_;
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
    Register struct_;
    std::string_view name_;
    inline friend std::ostream &operator<<(std::ostream &os,
                                           SetStructFieldName const &s) {
      return os << s.struct_ << " " << s.name_;
    };
  };

  struct AddHashtag {
    Register struct_;
    ast::Hashtag hashtag_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           AddHashtag const &a) {
      return os << a.struct_ << " " << static_cast<int>(a.hashtag_.kind_);
    }
  };

  struct CondJump {
    Register cond_;
    BlockIndex blocks_[2];
  };

  struct BlockSeqJump {
    RegisterOr<BlockSequence> bseq_;
    absl::flat_hash_map<ast::BlockLiteral const *, ir::BlockIndex> const
        *jump_table_;
  };

  struct LoadSymbol {
    std::string_view name_;
    type::Type const *type_;
  };

  struct AddEnumerator {
    Register enum_;
    std::string_view name_;
  };

  struct SetEnumerator {
    Register enum_;
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
    ast::Node *node_;
    Register ctx_;

    inline friend std::ostream &operator<<(std::ostream &os, AstData ast) {
      return os << ast.node_ << " ctx=" << ast.ctx_;
    }
  };

  struct AddBc {
    Register ctx_;
    ast::Declaration *decl_;
    RegisterOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os, AddBc const &a) {
      return os << a.ctx_ << " " << a.decl_ << " " << a.type_;
    }
  };

  union {
    Empty empty_;
    Register reg_;
    size_t get_ret_;
    type::Type const *type_;
    ast::StructLiteral *sl_;
    LoadSymbol load_sym_;

    CreateStruct create_struct_;
    CreateStructField create_struct_field_;
    SetStructFieldName set_struct_field_name_;
    AddHashtag add_hashtag_;
    AddEnumerator add_enumerator_;
    SetEnumerator set_enumerator_;
    CondJump cond_jump_;
    BlockIndex block_;
    BlockSeqJump block_seq_jump_;
    Call call_;
    PtrIncr ptr_incr_;
    BlockSeqContains block_seq_contains_;
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
    RegisterOr<type::Interface const *> intf_arg_;

    SpecialMember<1> special1_;
    SpecialMember<2> special2_;
    Args<bool> bool_args_;
    Args<int8_t> i8_args_;
    Args<int16_t> i16_args_;
    Args<int32_t> i32_args_;
    Args<int64_t> i64_args_;
    Args<uint8_t> u8_args_;
    Args<uint16_t> u16_args_;
    Args<uint32_t> u32_args_;
    Args<uint64_t> u64_args_;
    Args<float> float32_args_;
    Args<double> float64_args_;
    Args<EnumVal> enum_args_;
    Args<FlagsVal> flags_args_;
    Args<type::Type const *> type_args_;
    Args<Addr> addr_args_;

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
    Store<BlockSequence> store_block_;

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
    SetRet<ast::ScopeLiteral *> set_ret_scope_;
    SetRet<ast::FunctionLiteral *> set_ret_generic_;
    SetRet<Module *> set_ret_module_;
    SetRet<BlockSequence> set_ret_block_;
    SetRet<type::Interface const *> set_ret_intf_;

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
    PhiArgs<BlockSequence> *phi_block_;
    PhiArgs<ir::Addr> *phi_addr_;
    PhiArgs<ir::EnumVal> *phi_enum_;
    PhiArgs<ir::FlagsVal> *phi_flags_;
    PhiArgs<ir::AnyFunc> *phi_func_;

    PrintEnum print_enum_;
    PrintFlags print_flags_;

    type::Typed<Register> typed_reg_;
#define OP_MACRO(...)
#include "ir/op.xmacro.h"
#undef OP_MACRO
  };

  Register result;
};

RegisterOr<int64_t> Bytes(RegisterOr<type::Type const *> r);
RegisterOr<int64_t> Align(RegisterOr<type::Type const *> r);
RegisterOr<bool> Not(RegisterOr<bool> r);
RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r);
RegisterOr<int32_t> ModInt(RegisterOr<int32_t> v1, RegisterOr<int32_t> v2);
RegisterOr<bool> XorBool(RegisterOr<bool> v1, RegisterOr<bool> v2);
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

template <typename T>
RegisterOr<T> Neg(RegisterOr<T> r) {
  if (!r.is_reg_) { return -r.val_; }
  auto &cmd = MakeCmd(type::Get<T>(), Cmd::OpCode<Cmd::NegTag, T>());
  cmd.reg_  = r.reg_;
  // TODO reenable
  // Func::Current->references_[cmd.neg_int_.reg_].insert(cmd.result);
  return cmd.result;
}

template <typename T, typename... Args>
TypedRegister<T> Load(RegisterOr<Addr> r,
                      type::Type const *t = type::Get<T>()) {
  auto &cmd     = MakeCmd(t, Cmd::OpCode<Cmd::LoadTag, T>());
  cmd.addr_arg_ = r;
  return cmd.result;
}
Register Load(RegisterOr<Addr> r, type::Type const *t);

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> in,
                                     RegisterOr<type::Type const *> out);
RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r);
RegisterOr<type::Type const *> BufPtr(RegisterOr<type::Type const *> r);

RegisterOr<type::Type const *> Array(RegisterOr<int64_t> len,
                                     RegisterOr<type::Type const *> data_type);
Register VariantType(RegisterOr<Addr> r);
Register VariantValue(const type::Type *t, RegisterOr<Addr> r);
// Type repreesents the type of `ptr`
TypedRegister<Addr> PtrIncr(RegisterOr<Addr> ptr, RegisterOr<int64_t> inc,
                            type::Pointer const *t);
type::Typed<Register> Field(RegisterOr<Addr> r, type::Struct const *t,
                            size_t n);
type::Typed<Register> Field(RegisterOr<Addr> r, type::Tuple const *t, size_t n);

Cmd &MakeCmd(type::Type const *t, Op op);

namespace internal {
template <typename Tag, template <typename> typename F, typename Lhs,
          typename Rhs>
auto HandleBinop(Lhs lhs, Rhs rhs) {
  if constexpr (!IsRegOr<Lhs>::value) {
    // Can't use std::conditional_t because it evaluates both sides and so
    // calling ::type is no good. You get a cryptic error message "'foo' is not
    // a class, struct, or union type."
    if constexpr (IsTypedReg<Lhs>::value) {
      return HandleBinop<Tag, F>(RegisterOr<typename Lhs::type>(lhs), rhs);
    } else {
      return HandleBinop<Tag, F>(RegisterOr<Lhs>(lhs), rhs);
    }
  } else if constexpr (!IsRegOr<Rhs>::value) {
    return HandleBinop<Tag, F>(lhs, RegisterOr<typename Lhs::type>(rhs));
  } else {
    static_assert(std::is_same_v<Lhs, Rhs>);
    using result_type = decltype(F<typename Lhs::type>{}(lhs.val_, rhs.val_));
    using ret_type    = ir::RegisterOr<result_type>;
    return [&]() -> ret_type {
      if (!lhs.is_reg_ && !rhs.is_reg_) {
        return F<typename Lhs::type>{}(lhs.val_, rhs.val_);
      }
      auto &cmd = MakeCmd(type::Get<result_type>(),
                          Cmd::OpCode<Tag, typename Lhs::type>());
      cmd.template set<Tag, typename Lhs::type>(lhs, rhs);
      /* TODO reenable
      auto &refs = Func::Current->references_;
      if (lhs.is_reg_) { refs[lhs.reg_].insert(cmd.result); }
      if (rhs.is_reg_) { refs[rhs.reg_].insert(cmd.result); }
      */
      return cmd.result;
    }();
  }
}

}  // namespace internal

template <typename T>
void SetRet(size_t n, T t) {
  if constexpr (!IsRegOr<T>::value) {
    return SetRet(n, RegisterOr<T>(t));
  } else {
    auto &cmd =
        MakeCmd(nullptr, Cmd::OpCode<Cmd::SetRetTag, typename T::type>());
    cmd.template set<Cmd::SetRetTag, typename T::type>(n, t);
    // TODO reenable if (r.is_reg_) {
    // Func::Current->references_[r.reg_].insert(cmd.result); }
  }
}
void SetRet(size_t n, type::Typed<Results> const &v2, Context *ctx = nullptr);

template <typename Lhs, typename Rhs>
RegisterOr<bool> Lt(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::LtTag, std::less>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
RegisterOr<bool> Gt(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::GtTag, std::greater>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
RegisterOr<bool> Le(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::LeTag, std::less>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
RegisterOr<bool> Ge(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::GeTag, std::greater>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
RegisterOr<bool> Eq(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::EqTag, std::equal_to>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
RegisterOr<bool> Ne(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::NeTag, std::not_equal_to>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
auto Add(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::AddTag, std::plus>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
auto Mod(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::ModTag, std::modulus>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
auto Sub(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::SubTag, std::minus>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
auto Mul(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::MulTag, std::multiplies>(lhs, rhs);
}

template <typename Lhs, typename Rhs>
auto Div(Lhs lhs, Rhs rhs) {
  return internal::HandleBinop<Cmd::DivTag, std::divides>(lhs, rhs);
}

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
Register CreateTuple();
void AppendToTuple(Register tup, RegisterOr<type::Type const *> entry);
Register FinalizeTuple(Register tup);
Register CreateVariant();
void AppendToVariant(Register tup, RegisterOr<type::Type const *> entry);
Register FinalizeVariant(Register var);
void CondJump(RegisterOr<bool> cond, BlockIndex true_block,
              BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();

TypedRegister<type::Type const *> NewOpaqueType(::Module *mod);

void BlockSeqJump(RegisterOr<BlockSequence> r,
                  absl::flat_hash_map<ast::BlockLiteral const *,
                                      ir::BlockIndex> const *jump_table);

RegisterOr<bool> BlockSeqContains(RegisterOr<BlockSequence> r,
                                  ast::BlockLiteral *lit);

Results Cast(type::Type const *from, type::Type const *to, Results const &val);

TypedRegister<Addr> Index(type::Pointer const *t, Register array_ptr,
                          RegisterOr<int64_t> offset);
TypedRegister<Addr> Alloca(type::Type const *t);
TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx);

type::Typed<Register> LoadSymbol(std::string_view name, type::Type const *type);

TypedRegister<Addr> GetRet(size_t n, type::Type const *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);

void Move(type::Type const *t, Register from, RegisterOr<Addr> to);
void Copy(type::Type const *t, Register from, RegisterOr<Addr> to);
void Destroy(type::Type const *t, Register r);
void Init(type::Type const *t, Register r);

void VerifyType(ast::Node *node, Register ctx);
Register EvaluateAsType(ast::Node *node, Register ctx);

Register CreateContext(Module *mod);
void AddBoundConstant(Register ctx, ast::Declaration *decl,
                      RegisterOr<type::Type const *> type);
void DestroyContext(Register r);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
