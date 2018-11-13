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
#define OP_MACRO(op) op,
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
  template <typename T, size_t Index>
  struct CommandOpCode {
    constexpr static size_t index = Index;

    template <typename... Args>
    static T Make(Args &&... args) {
      return T{{}, std::forward<Args>(args)...};
    }
  };
#define CMD(name)                                                              \
  struct name                                                                  \
      : public CommandOpCode<name, static_cast<std::underlying_type_t<Op>>(    \
                                       Op::name)>

  CMD(Trunc) { Register reg_; };
  CMD(Extend) { Register reg_; };
  CMD(Bytes) { RegisterOr<const type::Type *> arg_; };
  CMD(Align) { RegisterOr<const type::Type *> arg_; };
  CMD(Not) { Register reg_; };
  CMD(NegInt) { Register reg_; };
  CMD(NegReal) { Register reg_; };
  CMD(ArrayLength) { Register arg_; };
  CMD(ArrayData) { Register arg_; };

  CMD(LoadBool) { Register arg_; };
  CMD(LoadChar) { Register arg_; };
  CMD(LoadInt) { Register arg_; };
  CMD(LoadReal) { Register arg_; };
  CMD(LoadType) { Register arg_; };
  CMD(LoadEnum) { Register arg_; };
  CMD(LoadFlags) { Register arg_; };
  CMD(LoadAddr) { Register arg_; };
  CMD(LoadFunc) { Register arg_; };

  CMD(StoreBool) {
    Register addr_;
    RegisterOr<bool> val_;
  };
  CMD(StoreChar) {
    Register addr_;
    RegisterOr<char> val_;
  };
  CMD(StoreInt) {
    Register addr_;
    RegisterOr<i32> val_;
  };
  CMD(StoreReal) {
    Register addr_;
    RegisterOr<double> val_;
  };
  CMD(StoreType) {
    Register addr_;
    RegisterOr<type::Type const *> val_;
  };
  CMD(StoreEnum) {
    Register addr_;
    RegisterOr<EnumVal> val_;
  };
  CMD(StoreFunc) {
    Register addr_;
    RegisterOr<Func *> val_;
  };
  CMD(StoreFlags) {
    Register addr_;
    RegisterOr<FlagsVal> val_;
  };
  CMD(StoreAddr) {
    Register addr_;
    RegisterOr<ir::Addr> val_;
  };

  CMD(PrintBool) { RegisterOr<bool> arg_; };
  CMD(PrintChar) { RegisterOr<char> arg_; };
  CMD(PrintInt) { RegisterOr<i32> arg_; };
  CMD(PrintReal) { RegisterOr<double> arg_; };
  CMD(PrintType) { RegisterOr<type::Type const *> arg_; };
  CMD(PrintEnum) {
    RegisterOr<EnumVal> arg_;
    type::Enum const *enum_type_;
  };
  CMD(PrintFlags) {
    RegisterOr<FlagsVal> arg_;
    type::Flags const *flags_type_;
  };
  CMD(PrintAddr) { RegisterOr<ir::Addr> arg_; };
  CMD(PrintCharBuffer) { RegisterOr<std::string_view> arg_; };

  CMD(AddInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(AddReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(SubInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(SubReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(MulInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(MulReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(DivInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(DivReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(ModInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(ModReal) { std::array<RegisterOr<double>, 2> args_; };

  CMD(LtInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(LtReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(LtFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(LeInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(LeReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(LeFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(GtInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(GtReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(GtFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(GeInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(GeReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(GeFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(EqBool) { std::array<Register, 2> args_; };
  CMD(EqChar) { std::array<RegisterOr<char>, 2> args_; };
  CMD(EqInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(EqReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(EqType) { std::array<RegisterOr<type::Type const *>, 2> args_; };
  CMD(EqEnum) { std::array<RegisterOr<EnumVal>, 2> args_; };
  CMD(EqFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(EqAddr) { std::array<RegisterOr<ir::Addr>, 2> args_; };
  CMD(NeChar) { std::array<RegisterOr<char>, 2> args_; };
  CMD(NeInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(NeReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(NeType) { std::array<RegisterOr<type::Type const *>, 2> args_; };
  CMD(NeEnum) { std::array<RegisterOr<EnumVal>, 2> args_; };
  CMD(NeFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(NeAddr) { std::array<RegisterOr<ir::Addr>, 2> args_; };

  CMD(XorBool) { std::array<RegisterOr<bool>, 2> args_; };

  CMD(XorFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(OrFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(AndFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };

  CMD(CreateStruct) { ast::StructLiteral *lit_; };
  CMD(CreateStructField) {
    type::Struct *struct_;
    RegisterOr<type::Type const *> type_;
  };
  CMD(SetStructFieldName) {
    // Implicitly the last element.
    type::Struct *struct_;
    std::string_view name_;
  };
  CMD(FinalizeStruct) { Register reg_; };

  CMD(DebugIr){};

  CMD(Malloc) { RegisterOr<i32> arg_; };
  CMD(Free) { Register reg_; };
  CMD(Alloca) { type::Type const *type_; };

  CMD(Ptr) { Register reg_; };
  CMD(Arrow) { std::array<RegisterOr<type::Type const *>, 2> args_; };
  CMD(Array) {
    RegisterOr<i32> len_;
    RegisterOr<type::Type const *> type_;
  };
  CMD(CreateTuple){};
  CMD(AppendToTuple) {
    Register tup_;
    RegisterOr<type::Type const *> arg_;
  };
  CMD(FinalizeTuple) { Register tup_; };
  CMD(CreateVariant){};
  CMD(AppendToVariant) {
    Register var_;
    RegisterOr<type::Type const *> arg_;
  };
  CMD(FinalizeVariant) { Register var_; };
  CMD(CreateBlockSeq){};
  CMD(AppendToBlockSeq) {
    Register block_seq_;
    RegisterOr<ir::BlockSequence> arg_;
  };
  CMD(FinalizeBlockSeq) { Register block_seq_; };

  CMD(VariantType) { Register reg_; };
  CMD(VariantValue) { Register reg_; };
  CMD(PtrIncr) {
    // TODO maybe store the type here rather than on the cmd because most cmds
    // don't need it.
    Register ptr_;
    type::Type const *pointee_type_;
    RegisterOr<i32> incr_;
  };
  CMD(Field) {
    Register ptr_;
    type::Struct const *struct_type_;
    size_t num_;
  };

  CMD(Call) {
    Call(RegisterOr<AnyFunc> f, LongArgs * args, OutParams * outs)
        : fn_(f), long_args_(args), outs_(outs) {}
    RegisterOr<AnyFunc> fn_;
    LongArgs *long_args_;
    OutParams *outs_;
  };

  CMD(PhiBool) { PhiArgs<bool> *args_; };
  CMD(PhiChar) { PhiArgs<char> *args_; };
  CMD(PhiInt) { PhiArgs<i32> *args_; };
  CMD(PhiReal) { PhiArgs<double> *args_; };
  CMD(PhiType) { PhiArgs<type::Type const *> *args_; };
  CMD(PhiBlock) { PhiArgs<BlockSequence> *args_; };
  CMD(PhiAddr) { PhiArgs<ir::Addr> *args_; };

  CMD(CondJump) {
    Register cond_;
    BlockIndex blocks_[2];
  };
  CMD(UncondJump) { BlockIndex block_; };
  CMD(ReturnJump){};
  CMD(BlockSeqJump) {
    RegisterOr<BlockSequence> bseq_;
    std::unordered_map<ast::BlockLiteral const *, ir::BlockIndex> const
        *jump_table_;
  };

  CMD(CastIntToReal) { Register reg_; };
  CMD(CastPtr) {
    Register reg_;
    type::Type const *type_;
  };

  CMD(BlockSeqContains) {
    Register reg_;
    ast::BlockLiteral *lit_;
  };

  CMD(SetReturnBool) {
    size_t ret_num_;
    RegisterOr<bool> val_;
  };

  CMD(SetReturnChar) {
    size_t ret_num_;
    RegisterOr<char> val_;
  };

  CMD(SetReturnInt) {
    size_t ret_num_;
    RegisterOr<i32> val_;
  };

  CMD(SetReturnReal) {
    size_t ret_num_;
    RegisterOr<double> val_;
  };

  CMD(SetReturnType) {
    size_t ret_num_;
    RegisterOr<type::Type const *> val_;
  };

  CMD(SetReturnEnum) {
    size_t ret_num_;
    RegisterOr<EnumVal> val_;
  };

  CMD(SetReturnFlags) {
    size_t ret_num_;
    RegisterOr<FlagsVal> val_;
  };

  CMD(SetReturnAddr) {
    size_t ret_num_;
    RegisterOr<ir::Addr> val_;
  };

  CMD(SetReturnCharBuf) {
    size_t ret_num_;
    RegisterOr<std::string_view> val_;
  };

  CMD(SetReturnFunc) {
    size_t ret_num_;
    RegisterOr<ir::AnyFunc> val_;
  };

  CMD(SetReturnScope) {
    size_t ret_num_;
    RegisterOr<ast::ScopeLiteral *> val_;
  };

  CMD(SetReturnGeneric) {
    size_t ret_num_;
    RegisterOr<ast::FunctionLiteral *> val_;
  };

  CMD(SetReturnModule) {
    size_t ret_num_;
    RegisterOr<Module const *> val_;
  };

  CMD(SetReturnBlock) {
    size_t ret_num_;
    RegisterOr<BlockSequence> val_;
  };
#undef CMD

  struct PrintTag;
  struct StoreTag;
  struct EqTag;
  template <typename Tag, typename T>
  static constexpr Op OpCode() {
    return static_cast<Op>(
        std::decay_t<decltype(
            std::declval<Cmd>().template get<Tag, T>())>::index);
  }

  template <typename Tag, typename T>
  constexpr auto &get() {
    if constexpr (std::is_same_v<Tag, PrintTag>) {
      if constexpr (std::is_same_v<T, bool>) { return print_bool_; }
      if constexpr (std::is_same_v<T, char>) { return print_char_; }
      if constexpr (std::is_same_v<T, i32>) { return print_int_; }
      if constexpr (std::is_same_v<T, double>) { return print_real_; }
      if constexpr (std::is_same_v<T, type::Type const *>) {
        return print_type_;
      }
      if constexpr (std::is_same_v<T, EnumVal>) { return print_enum_; }
      if constexpr (std::is_same_v<T, FlagsVal>) { return print_flags_; }
      if constexpr (std::is_same_v<T, Addr>) { return print_addr_; }
      if constexpr (std::is_same_v<T, std::string_view>) { return print_char_buffer_; }
    }
    if constexpr (std::is_same_v<Tag, StoreTag>) {
      if constexpr (std::is_same_v<T, bool>) { return store_bool_; }
      if constexpr (std::is_same_v<T, char>) { return store_char_; }
      if constexpr (std::is_same_v<T, i32>) { return store_int_; }
      if constexpr (std::is_same_v<T, double>) { return store_real_; }
      if constexpr (std::is_same_v<T, type::Type const *>) {
        return store_type_;
      }
      if constexpr (std::is_same_v<T, EnumVal>) { return store_enum_; }
      if constexpr (std::is_same_v<T, FlagsVal>) { return store_flags_; }
      if constexpr (std::is_same_v<T, Func *>) { return store_func_; } // TODO const
      if constexpr (std::is_same_v<T, Addr>) { return store_addr_; }
    }
    if constexpr (std::is_same_v<Tag, EqTag>) {
      if constexpr (std::is_same_v<T, bool>) { return eq_bool_; }
      if constexpr (std::is_same_v<T, char>) { return eq_char_; }
      if constexpr (std::is_same_v<T, i32>) { return eq_int_; }
      if constexpr (std::is_same_v<T, double>) { return eq_real_; }
      if constexpr (std::is_same_v<T, type::Type const *>) {
        return eq_type_;
      }
      if constexpr (std::is_same_v<T, EnumVal>) { return eq_enum_; }
      if constexpr (std::is_same_v<T, FlagsVal>) { return eq_flags_; }
      if constexpr (std::is_same_v<T, Addr>) { return eq_addr_; }
    }
  }

  template <typename Tag, typename T, typename... Args>
  void set(Args &&... args) {
    auto &cmd   = this->template get<Tag, T>();
    using cmd_t = std::decay_t<decltype(cmd)>;
    cmd         = cmd_t::Make(std::forward<Args>(args)...);
  }

  Cmd(const type::Type *t, Op op);
  Op op_code_;

  union {
    Trunc trunc_;
    Extend extend_;
    Bytes bytes_;
    Align align_;
    Not not_;
    NegInt neg_int_;
    NegReal neg_real_;
    ArrayLength array_length_;
    ArrayData array_data_;
    LoadBool load_bool_;
    LoadChar load_char_;
    LoadInt load_int_;
    LoadReal load_real_;
    LoadType load_type_;
    LoadEnum load_enum_;
    LoadFlags load_flags_;
    LoadAddr load_addr_;
    LoadFunc load_func_;

    StoreBool store_bool_;
    StoreChar store_char_;
    StoreInt store_int_;
    StoreReal store_real_;
    StoreType store_type_;
    StoreEnum store_enum_;
    StoreFunc store_func_;
    StoreFlags store_flags_;
    StoreAddr store_addr_;

    PrintBool print_bool_;
    PrintChar print_char_;
    PrintInt print_int_;
    PrintReal print_real_;
    PrintType print_type_;
    PrintEnum print_enum_;
    PrintFlags print_flags_;
    PrintAddr print_addr_;
    PrintCharBuffer print_char_buffer_;

    AddInt add_int_;
    AddReal add_real_;
    SubInt sub_int_;
    SubReal sub_real_;
    MulInt mul_int_;
    MulReal mul_real_;
    DivInt div_int_;
    DivReal div_real_;
    ModInt mod_int_;
    ModReal mod_real_;

    LtInt lt_int_;
    LtReal lt_real_;
    LtFlags lt_flags_;
    LeInt le_int_;
    LeReal le_real_;
    LeFlags le_flags_;
    GtInt gt_int_;
    GtReal gt_real_;
    GtFlags gt_flags_;
    GeInt ge_int_;
    GeReal ge_real_;
    GeFlags ge_flags_;
    EqBool eq_bool_;
    EqChar eq_char_;
    EqInt eq_int_;
    EqReal eq_real_;
    EqType eq_type_;
    EqEnum eq_enum_;
    EqFlags eq_flags_;
    EqAddr eq_addr_;
    NeChar ne_char_;
    NeInt ne_int_;
    NeReal ne_real_;
    NeType ne_type_;
    NeEnum ne_enum_;
    NeFlags ne_flags_;
    NeAddr ne_addr_;

    XorBool xor_bool_;
    XorFlags xor_flags_;
    OrFlags or_flags_;
    AndFlags and_flags_;

    CreateStruct create_struct_;
    CreateStructField create_struct_field_;
    SetStructFieldName set_struct_field_name_;
    FinalizeStruct finalize_struct_;

    DebugIr debug_ir_;

    Malloc malloc_;
    Free free_;
    Alloca alloca_;

    PtrIncr ptr_incr_;
    Field field_;

    Cmd::Ptr ptr_;
    Cmd::Arrow arrow_;
    Cmd::Array array_;
    CreateTuple create_tuple_;
    AppendToTuple append_to_tuple_;
    FinalizeTuple finalize_tuple_;
    CreateVariant create_variant_;
    AppendToVariant append_to_variant_;
    FinalizeVariant finalize_variant_;
    CreateBlockSeq create_block_seq_;
    AppendToBlockSeq append_to_block_seq_;
    FinalizeBlockSeq finalize_block_seq_;

    CondJump cond_jump_;
    UncondJump uncond_jump_;
    ReturnJump return_jump_;
    BlockSeqJump block_seq_jump_;

    Cmd::VariantType variant_type_;
    Cmd::VariantValue variant_value_;

    Call call_;
    CastIntToReal cast_int_to_real_;
    CastPtr cast_ptr_;
    PhiBool phi_bool_;
    PhiChar phi_char_;
    PhiInt phi_int_;
    PhiReal phi_real_;
    PhiType phi_type_;
    PhiBlock phi_block_;
    PhiAddr phi_addr_;

    BlockSeqContains block_seq_contains_;

    SetReturnBool set_return_bool_;
    SetReturnChar set_return_char_;
    SetReturnInt set_return_int_;
    SetReturnReal set_return_real_;
    SetReturnType set_return_type_;
    SetReturnEnum set_return_enum_;
    SetReturnFlags set_return_flags_;
    SetReturnCharBuf set_return_char_buf_;
    SetReturnAddr set_return_addr_;
    SetReturnFunc set_return_func_;
    SetReturnScope set_return_scope_;
    SetReturnGeneric set_return_generic_;
    SetReturnModule set_return_module_;
    SetReturnBlock set_return_block_;
  };

  Register result;
};

RegisterOr<char> Trunc(RegisterOr<i32> r);
RegisterOr<i32> Extend(RegisterOr<char> r);
Register Bytes(RegisterOr<type::Type const *> r);
Register Align(RegisterOr<type::Type const *> r);
RegisterOr<bool> Not(RegisterOr<bool> r);
RegisterOr<i32> NegInt(RegisterOr<i32> r);
RegisterOr<double> NegReal(RegisterOr<double> r);
Register ArrayLength(Register r);
Register ArrayData(Register r, type::Type const *t);
Register LoadBool(Register r);
Register LoadChar(Register r);
Register LoadInt(Register r);
Register LoadReal(Register r);
Register LoadType(Register r);
Register LoadEnum(Register r, type::Type const *t);
Register LoadFlags(Register r, type::Type const *t);
Register LoadAddr(Register r, type::Type const *t);
Register LoadFunc(Register r, type::Type const *t);
RegisterOr<i32> AddInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<double> AddReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<i32> SubInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<double> SubReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<i32> MulInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<double> MulReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<i32> DivInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<double> DivReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<i32> ModInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<double> ModReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> LtInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<bool> LtReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> LtFlags(RegisterOr<FlagsVal> v1, RegisterOr<FlagsVal> v2);
RegisterOr<bool> LeInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<bool> LeReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> LeFlags(RegisterOr<FlagsVal> v1, RegisterOr<FlagsVal> v2);
RegisterOr<bool> GeInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<bool> GeReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> GeFlags(RegisterOr<FlagsVal> v1, RegisterOr<FlagsVal> v2);
RegisterOr<bool> GtInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<bool> GtReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> GtFlags(RegisterOr<FlagsVal> v1, RegisterOr<FlagsVal> v2);
RegisterOr<bool> NeChar(RegisterOr<char> v1, RegisterOr<char> v2);
RegisterOr<bool> NeInt(RegisterOr<i32> v1, RegisterOr<i32> v2);
RegisterOr<bool> NeReal(RegisterOr<double> v1, RegisterOr<double> v2);
RegisterOr<bool> NeType(RegisterOr<type::Type const *> v1,
                        RegisterOr<type::Type const *> v2);
RegisterOr<bool> NeEnum(RegisterOr<EnumVal> v1, RegisterOr<EnumVal> v2);
RegisterOr<bool> NeFlags(RegisterOr<FlagsVal> v1, RegisterOr<FlagsVal> v2);
RegisterOr<bool> NeAddr(RegisterOr<ir::Addr> v1, RegisterOr<ir::Addr> v2);
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

Register Malloc(const type::Type *t, RegisterOr<i32> r);
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
Register PtrIncr(Register ptr, RegisterOr<i32> inc, type::Type const *t);
Register Field(Register r, type::Struct const *t, size_t n);

Cmd &MakeCmd(type::Type const *t, Op op);

template <typename Lhs, typename Rhs>
RegisterOr<bool> Eq(Lhs lhs, Rhs rhs) {
  if constexpr (!IsRegOr<Lhs>::value) {
    return Eq(RegisterOr<Lhs>(lhs), rhs);
  } else if constexpr (!IsRegOr<Rhs>::value) {
    return Eq(lhs, RegisterOr<Rhs>(rhs));
  } else {
    static_assert(std::is_same_v<Lhs, Rhs>);

    if (!lhs.is_reg_ && !rhs.is_reg_) {
      return std::equal_to<typename Lhs::type>{}(lhs.val_, rhs.val_);
    }
    auto &cmd =
        MakeCmd(type::Bool, Cmd::OpCode<Cmd::EqTag, typename Lhs::type>());
    if constexpr (std::is_same_v<typename Lhs::type, bool>) {
      cmd.template set<Cmd::EqTag, typename Lhs::type>(lhs.reg_, rhs.reg_);
    } else {
      cmd.template set<Cmd::EqTag, typename Lhs::type>(lhs, rhs);
    }
    /* TODO reenable
    auto &refs = Func::Current->references_;
    if (lhs.is_reg_) { refs[lhs.reg_].insert(cmd.result); }
    if (rhs.is_reg_) { refs[rhs.reg_].insert(cmd.result); }
    */
    return cmd.result;
  }
}

template <typename T, typename... Args>
void Print(T r, Args &&... args) {
  if constexpr (IsRegOr<T>::value) {
    using type = typename T::type;
    auto &cmd  = MakeCmd(nullptr, Cmd::OpCode<Cmd::PrintTag, type>());
    cmd.template set<Cmd::PrintTag, type>(r, std::forward<Args>(args)...);
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
void SetReturnBool(size_t n, RegisterOr<bool> r);
void SetReturnChar(size_t n, RegisterOr<char> r);
void SetReturnInt(size_t n, RegisterOr<i32> r);
void SetReturnReal(size_t n, RegisterOr<double> r);
void SetReturnType(size_t n, RegisterOr<type::Type const *> r);
void SetReturnEnum(size_t n, RegisterOr<EnumVal> r);
void SetReturnFlags(size_t n, RegisterOr<FlagsVal> r);
void SetReturnCharBuf(size_t n, RegisterOr<std::string_view> r);
void SetReturnAddr(size_t n, RegisterOr<Addr> r);
void SetReturnFunc(size_t n, RegisterOr<AnyFunc> const &r);
void SetReturnScope(size_t n, RegisterOr<ast::ScopeLiteral *> r);
void SetReturnGeneric(size_t n, RegisterOr<ast::FunctionLiteral *> r);
void SetReturnModule(size_t n, RegisterOr<Module const *> r);
void SetReturnBlock(size_t n, RegisterOr<BlockSequence> r);


Register Load(Register r, type::Type const *t);

RegisterOr<double> CastIntToReal(RegisterOr<i32> r);
Register CastPtr(Register r, type::Pointer const *t);

Register Index(type::Type const *t, Register array_ptr, RegisterOr<i32> offset);
Register Alloca(const type::Type *t);

void SetReturn(size_t n, Val const &v2);

std::ostream &operator<<(std::ostream &os, Cmd const &cmd);
}  // namespace ir
#endif  // ICARUS_IR_CMD_H
