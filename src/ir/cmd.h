#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "base/untyped_buffer.h"
#include "context.h"
#include "val.h"

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
  std::unordered_map<BlockIndex, RegisterOr<T>> map_;
};

struct Cmd {
  template <typename T>
  struct Store {
    Register addr_;
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
    type::Type const *type_;  // Struct or Tuple
    size_t num_;
  };

  struct Call {
    Call(RegisterOr<AnyFunc> f, Arguments * args, OutParams * outs)
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

  struct CreateStructField {
    Register struct_;
    RegisterOr<type::Type const *> type_;

    inline friend std::ostream &operator<<(std::ostream &os,
                                           CreateStructField const &c) {
      return os << c.struct_ << " " << c.type_;
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

  struct CondJump {
    Register cond_;
    BlockIndex blocks_[2];
  };

  struct BlockSeqJump {
    RegisterOr<BlockSequence> bseq_;
    std::unordered_map<ast::BlockLiteral const *, ir::BlockIndex> const
        *jump_table_;
  };

  union {
    Empty empty_;
    Register reg_;
    size_t get_ret_;
    type::Type const *type_;

    ast::StructLiteral *generate_struct_;

    CreateStructField create_struct_field_;
    SetStructFieldName set_struct_field_name_;
    CondJump cond_jump_;
    BlockIndex block_;
    BlockSeqJump block_seq_jump_;
    Call call_;
    PtrIncr ptr_incr_;
    BlockSeqContains block_seq_contains_;
    Cmd::Array array_;
    Field field_;

    // TODO names of these are easily mis-spellable and would lead to UB.
    RegisterOr<bool> bool_arg_;
    RegisterOr<char> char_arg_;
    RegisterOr<i8> i8_arg_;
    RegisterOr<i16> i16_arg_;
    RegisterOr<i32> i32_arg_;
    RegisterOr<i64> i64_arg_;
    RegisterOr<u8> u8_arg_;
    RegisterOr<u16> u16_arg_;
    RegisterOr<u32> u32_arg_;
    RegisterOr<u64> u64_arg_;
    RegisterOr<float> float32_arg_;
    RegisterOr<double> float64_arg_;
    RegisterOr<EnumVal> enum_arg_;
    RegisterOr<FlagsVal> flags_arg_;
    RegisterOr<type::Type const *> type_arg_;
    RegisterOr<std::string_view> char_buf_arg_;
    RegisterOr<Addr> addr_arg_;

    Args<bool> bool_args_;
    Args<char> char_args_;
    Args<i8> i8_args_;
    Args<i16> i16_args_;
    Args<i32> i32_args_;
    Args<i64> i64_args_;
    Args<u8> u8_args_;
    Args<u16> u16_args_;
    Args<u32> u32_args_;
    Args<u64> u64_args_;
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
    Store<i8> store_i8_;
    Store<i16> store_i16_;
    Store<i32> store_i32_;
    Store<i64> store_i64_;
    Store<u8> store_u8_;
    Store<u16> store_u16_;
    Store<u32> store_u32_;
    Store<u64> store_u64_;
    Store<float> store_float32_;
    Store<double> store_float64_;
    Store<type::Type const *> store_type_;
    Store<EnumVal> store_enum_;
    Store<AnyFunc> store_func_;
    Store<FlagsVal> store_flags_;
    Store<Addr> store_addr_;
    Store<BlockSequence> store_block_;

    SetRet<bool> set_ret_bool_;
    SetRet<char> set_ret_char_;
    SetRet<i8> set_ret_i8_;
    SetRet<i16> set_ret_i16_;
    SetRet<i32> set_ret_i32_;
    SetRet<i64> set_ret_i64_;
    SetRet<u8> set_ret_u8_;
    SetRet<u16> set_ret_u16_;
    SetRet<u32> set_ret_u32_;
    SetRet<u64> set_ret_u64_;
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
    PhiArgs<i8> *phi_i8_;
    PhiArgs<i16> *phi_i16_;
    PhiArgs<i32> *phi_i32_;
    PhiArgs<i64> *phi_i64_;
    PhiArgs<u8> *phi_u8_;
    PhiArgs<u16> *phi_u16_;
    PhiArgs<u32> *phi_u32_;
    PhiArgs<u64> *phi_u64_;
    PhiArgs<float> *phi_float32_;
    PhiArgs<double> *phi_float64_;
    PhiArgs<type::Type const *> *phi_type_;
    PhiArgs<BlockSequence> *phi_block_;
    PhiArgs<ir::Addr> *phi_addr_;

    PrintEnum print_enum_;
    PrintFlags print_flags_;

    type::Typed<Register> typed_reg_;
#define OP_MACRO(...)
#include "ir/op.xmacro.h"
#undef OP_MACRO
  };

  Register result;
};

RegisterOr<char> Trunc(RegisterOr<i32> r);
RegisterOr<i32> Extend(RegisterOr<char> r);
RegisterOr<i32> Bytes(RegisterOr<type::Type const *> r);
RegisterOr<i32> Align(RegisterOr<type::Type const *> r);
RegisterOr<bool> Not(RegisterOr<bool> r);
RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r);
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

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> in,
                                     RegisterOr<type::Type const *> out);
RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r);
RegisterOr<type::Type const *> BufPtr(RegisterOr<type::Type const *> r);

RegisterOr<type::Type const *> Array(RegisterOr<i32> len,
                                     RegisterOr<type::Type const *> data_type);
Register VariantType(Register r);
Register VariantValue(const type::Type *t, Register r);
// Type repreesents the type of `ptr`
TypedRegister<Addr> PtrIncr(Register ptr, RegisterOr<i32> inc,
                            type::Pointer const *t);
Register Field(Register r, type::Struct const *t, size_t n);
Register Field(Register r, type::Tuple const *t, size_t n);

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
    using ret_type =  ir::RegisterOr<result_type>;
    return [&]() -> ret_type {
      if (!lhs.is_reg_ && !rhs.is_reg_) {
        return F<typename Lhs::type>{}(lhs.val_, rhs.val_);
      }
      auto &cmd = MakeCmd(type::Get<result_type>(),
                          Cmd::OpCode<Tag, typename Lhs::type>());
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
void SetRet(size_t n, Val const &v2, Context *ctx = nullptr);

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
void BlockSeqJump(RegisterOr<BlockSequence> r,
                  std::unordered_map<ast::BlockLiteral const *,
                                     ir::BlockIndex> const *jump_table);

RegisterOr<bool> BlockSeqContains(RegisterOr<BlockSequence> r,
                                  ast::BlockLiteral *lit);

Val Cast(type::Type const *from, type::Type const *to, Val const& val);

RegisterOr<type::Type const *> GenerateStruct(ast::StructLiteral *sl);

TypedRegister<Addr> Index(type::Type const *t, Register array_ptr,
                          RegisterOr<i32> offset);
TypedRegister<Addr> Alloca(const type::Type *t);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
