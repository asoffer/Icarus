#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "base/untyped_buffer.h"
#include "context.h"
#include "val.h"

namespace std {

template <>
struct less<ir::FlagsVal> {
  bool operator()(ir::FlagsVal lhs, ir::FlagsVal rhs) const {
    return lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value);
  }
};

template <>
struct less_equal<ir::FlagsVal> {
  bool operator()(ir::FlagsVal lhs, ir::FlagsVal rhs) const {
    return (lhs.value | rhs.value) == rhs.value;
  }
};

template <>
struct greater<ir::FlagsVal> {
  bool operator()(ir::FlagsVal lhs, ir::FlagsVal rhs) const {
    return lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value);
  }
};

template <>
struct greater_equal<ir::FlagsVal> {
  bool operator()(ir::FlagsVal lhs, ir::FlagsVal rhs) const {
    return (lhs.value | rhs.value) == lhs.value;
  }
};

}  // namespace std

namespace type {
struct Function;
struct Tuple;
struct Enum;
struct Flags;
struct Struct;
struct Pointer;
struct Variant;
}  // namespace type

namespace ast {
struct StructLiteral;
struct ScopeLiteral;
struct ScopeNode;
struct Function;
}  // namespace ast

namespace ir {
struct LongArgs {
  void append(const ir::Val &val);
  void append(ir::Register reg);
  std::string to_string() const;

  type::Function const *type_ = nullptr;
  base::vector<bool> is_reg_;
  base::untyped_buffer args_{0};
};

// Represents an output parameter. The boolean value denotes whether the
// register is a register to be filled with the value, or it is the address to
// which the value should be written.
struct OutParam {
  OutParam(Register reg, bool is_loc) : reg_(reg), is_loc_(is_loc) {}
  OutParam(const OutParam &) noexcept = default;
  OutParam(OutParam &&) noexcept      = default;
  OutParam &operator=(const OutParam &) noexcept = default;
  OutParam &operator=(OutParam &&) noexcept = default;

  Register reg_;
  bool is_loc_;
};

struct OutParams {
  Register AppendReg(type::Type const *);
  void AppendLoc(Register reg) { outs_.emplace_back(reg, true); }
  base::vector<OutParam> outs_;
};

enum class Op : uint8_t {
#define OP_MACRO(op, ...) op,
#define OP_MACRO_(op, ...) op,
#include "ir/op.xmacro.h"
#undef OP_MACRO_
#undef OP_MACRO
};
char const *OpCodeStr(Op op);

struct GenericPhiArgs : public base::Cast<GenericPhiArgs> {
  virtual ~GenericPhiArgs() {}
};
template <typename T>
struct PhiArgs : GenericPhiArgs {
  ~PhiArgs() override {}
  std::unordered_map<BlockIndex, RegisterOr<T>> map_;
};

struct Cmd {
  template <typename T>
  struct Store {
    Register addr_;
    RegisterOr<T> val_;

    inline friend std::ostream &operator<<(std::ostream &os, Store const &s) {
      return os << s.addr_.to_string() << " " << s.val_;
    }
  };

  template <typename T>
  struct SetRet {
    size_t ret_num_;
    RegisterOr<T> val_;

    inline friend std::ostream &operator<<(std::ostream &os, SetRet const &s) {
      return os << s.ret_num_ << " " << s.val_;
    }
  };

  // TODO finish removing these
  struct PrintEnum {
    RegisterOr<EnumVal> arg_;
    type::Enum const *enum_type_;
  };
  struct PrintFlags {
    RegisterOr<FlagsVal> arg_;
    type::Flags const *flags_type_;
  };

  struct CreateStruct { ast::StructLiteral *lit_; };
  struct CreateStructField {
    type::Struct *struct_;
    RegisterOr<type::Type const *> type_;
  };
  struct SetStructFieldName {
    // Implicitly the last element.
    type::Struct *struct_;
    std::string_view name_;
  };

  struct Array {
    RegisterOr<i32> len_;
    RegisterOr<type::Type const *> type_;
  };

  struct PtrIncr {
    // TODO maybe store the type here rather than on the cmd because most cmds
    // don't need it.
    Register ptr_;
    type::Type const *pointee_type_;
    RegisterOr<i32> incr_;
  };
  struct Field {
    Register ptr_;
    type::Struct const *struct_type_;
    size_t num_;
  };

  struct Call {
    Call(RegisterOr<AnyFunc> f, LongArgs * args, OutParams * outs)
        : fn_(f), long_args_(args), outs_(outs) {}
    RegisterOr<AnyFunc> fn_;
    LongArgs *long_args_;
    OutParams *outs_;
  };

  struct CondJump {
    Register cond_;
    BlockIndex blocks_[2];
  };
  struct UncondJump { BlockIndex block_; };
  struct ReturnJump {};
  struct BlockSeqJump {
    RegisterOr<BlockSequence> bseq_;
    std::unordered_map<ast::BlockLiteral const *, ir::BlockIndex> const
        *jump_table_;
  };

  struct BlockSeqContains {
    Register reg_;
    ast::BlockLiteral *lit_;
  };

#define OP_MACRO(op, tag, ...) struct tag##Tag;
#define OP_MACRO_(op, tag, ...) struct tag##Tag;
#include "ir/op.xmacro.h"
#undef OP_MACRO_
#undef OP_MACRO
  template <typename Tag, typename T>
  static constexpr Op OpCode() {
#define OP_MACRO(op, tag, type, field)                                         \
  if constexpr (std::is_same_v<Tag, tag##Tag> && std::is_same_v<T, type>) {    \
    return Op::op;                                                             \
  }
#define OP_MACRO_(op, tag, type, field)                                        \
  if constexpr (std::is_same_v<Tag, tag##Tag> && std::is_same_v<T, type>) {    \
    return Op::op;                                                             \
  }
#include "ir/op.xmacro.h"
#undef OP_MACRO_
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
#define OP_MACRO_(op, tag, type, field)                                        \
  if constexpr (std::is_same_v<Tag, tag##Tag> && std::is_same_v<T, type>) {    \
    return field;                                                              \
  }
#include "ir/op.xmacro.h"
#undef OP_MACRO_
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

    inline friend std::ostream &operator<<(std::ostream &os, Args const &a) {
      return os << a.args_[0] << " " << a.args_[1];
    }
  };

  struct Empty {
    inline friend std::ostream &operator<<(std::ostream &os, Empty) {
      return os;
    }
  };

  union {
    Empty empty_;
    Register reg_;
    type::Type const *type_;

    // TODO names of these are easily mis-spellable and would lead to UB.
    RegisterOr<bool> bool_arg_;
    RegisterOr<char> char_arg_;
    RegisterOr<i32> i32_arg_;
    RegisterOr<float> float32_arg_;
    RegisterOr<double> float64_arg_;
    RegisterOr<EnumVal> enum_arg_;
    RegisterOr<FlagsVal> flags_arg_;
    RegisterOr<type::Type const *> type_arg_;
    RegisterOr<std::string_view> char_buf_arg_;
    RegisterOr<Addr> addr_arg_;

    Args<bool> bool_args_;
    Args<char> char_args_;
    Args<i32> i32_args_;
    Args<float> float32_args_;
    Args<double> float64_args_;
    Args<EnumVal> enum_args_;
    Args<FlagsVal> flags_args_;
    Args<type::Type const *> type_args_;
    Args<Addr> addr_args_;

    // TODO rename these since some of them are used for things other than
    // storage (e.g., block appending).
    Store<bool> store_bool_;
    Store<char> store_char_;
    Store<i32> store_i32_;
    Store<float> store_float32_;
    Store<double> store_float64_;
    Store<type::Type const *> store_type_;
    Store<EnumVal> store_enum_;
    Store<Func *> store_func_;
    Store<FlagsVal> store_flags_;
    Store<Addr> store_addr_;
    Store<BlockSequence> store_block_;

    SetRet<bool> set_ret_bool_;
    SetRet<char> set_ret_char_;
    SetRet<i32> set_ret_i32_;
    SetRet<float> set_ret_float32_;
    SetRet<double> set_ret_float64_;
    SetRet<type::Type const *> set_ret_type_;
    SetRet<EnumVal> set_ret_enum_;
    SetRet<AnyFunc> set_ret_func_;
    SetRet<FlagsVal> set_ret_flags_;
    SetRet<Addr> set_ret_addr_;
    SetRet<std::string_view> set_ret_char_buf_;
    SetRet<ast::ScopeLiteral *> set_ret_scope_;
    SetRet<ast::FunctionLiteral *> set_ret_generic_;
    SetRet<Module const *> set_ret_module_;
    SetRet<BlockSequence> set_ret_block_;

    PhiArgs<bool> *phi_bool_;
    PhiArgs<char> *phi_char_;
    PhiArgs<i32> *phi_i32_;
    PhiArgs<float> *phi_float32_;
    PhiArgs<double> *phi_float64_;
    PhiArgs<type::Type const *> *phi_type_;
    PhiArgs<BlockSequence> *phi_block_;
    PhiArgs<ir::Addr> *phi_addr_;

    type::Typed<Register> typed_reg_;
#define OP_MACRO(...)
#define OP_MACRO_(op, tag, type, field) Cmd::op field;
#include "ir/op.xmacro.h"
#undef OP_MACRO_
#undef OP_MACRO
  };

  Register result;
};

RegisterOr<char> Trunc(RegisterOr<i32> r);
RegisterOr<i32> Extend(RegisterOr<char> r);
RegisterOr<i32> Bytes(RegisterOr<type::Type const *> r);
RegisterOr<i32> Align(RegisterOr<type::Type const *> r);
RegisterOr<bool> Not(RegisterOr<bool> r);
Register ArrayLength(Register r);
Register ArrayData(Register r, type::Type const *t);
RegisterOr<i32> ModInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
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
  cmd.reg_ = r.reg_;
  // TODO reenable Func::Current->references_[cmd.neg_int_.reg_].insert(cmd.result);
  return cmd.result;
}

template <typename T, typename... Args>
TypedRegister<T> Load(Register r, type::Type const *t = type::Get<T>()) {
  auto &cmd = MakeCmd(t, Cmd::OpCode<Cmd::LoadTag, T>());
  cmd.reg_ = r;
  return cmd.result;
}
Register Load(Register r, type::Type const *t);

TypedRegister<Addr> Malloc(const type::Type *t, RegisterOr<i32> r);
void Free(Register r);
RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> in,
                                     RegisterOr<type::Type const *> out);
RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r);

RegisterOr<type::Type const *> Array(RegisterOr<type::Type const *> data_type);
RegisterOr<type::Type const *> Array(RegisterOr<i32> len,
                                     RegisterOr<type::Type const *> data_type);
Register VariantType(Register r);
Register VariantValue(const type::Type *t, Register r);
// Type repreesents the type of `ptr`
TypedRegister<Addr> PtrIncr(Register ptr, RegisterOr<i32> inc,
                            type::Type const *t);
Register Field(Register r, type::Struct const *t, size_t n);

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
    using ret_type =
        ir::RegisterOr<decltype(F<typename Lhs::type>{}(lhs.val_, rhs.val_))>;
    return [&]() -> ret_type {
      if (!lhs.is_reg_ && !rhs.is_reg_) {
        return F<typename Lhs::type>{}(lhs.val_, rhs.val_);
      }
      auto &cmd = MakeCmd(type::Bool, Cmd::OpCode<Tag, typename Lhs::type>());
      if constexpr (std::is_same_v<typename Lhs::type, bool>) {
        cmd.template set<Tag, typename Lhs::type>(lhs.reg_, rhs.reg_);
      } else {
        cmd.template set<Tag, typename Lhs::type>(lhs, rhs);
      }
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
void SetRet(size_t n, Val const &v2);

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
    if constexpr (std::is_same_v<type, EnumVal> || std::is_same_v<type, FlagsVal>) {
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

void Call(RegisterOr<AnyFunc> const &f, LongArgs long_args);
void Call(RegisterOr<AnyFunc> const &f, LongArgs long_args, OutParams outs);
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
void BlockSeqJump(RegisterOr<BlockSequence> r,
                  std::unordered_map<ast::BlockLiteral const *,
                                     ir::BlockIndex> const *jump_table);

RegisterOr<bool> BlockSeqContains(RegisterOr<BlockSequence> r,
                                  ast::BlockLiteral *lit);

RegisterOr<float> CastIntToFloat32(RegisterOr<i32> r);
RegisterOr<double> CastIntToFloat64(RegisterOr<i32> r);
Register CastPtr(Register r, type::Pointer const *t);

TypedRegister<Addr> Index(type::Type const *t, Register array_ptr,
                          RegisterOr<i32> offset);
TypedRegister<Addr> Alloca(const type::Type *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
