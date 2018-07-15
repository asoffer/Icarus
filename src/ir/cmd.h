#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "context.h"
#include "val.h"

namespace IR {
enum class Op : char {
  // clang-format off
  Trunc, Extend,
  Bytes, Align,
  Not,
  NegInt, NegReal,
  ArrayLength, ArrayData,
  LoadBool, LoadChar, LoadInt, LoadReal, LoadType, LoadEnum, LoadFlags, LoadAddr,

  AddInt, AddReal, AddCharBuf,
  SubInt, SubReal,
  MulInt, MulReal,
  DivInt, DivReal,
  ModInt, ModReal,

  LtInt, LtReal, LtFlags,
  LeInt, LeReal, LeFlags,
  GtInt, GtReal, GtFlags,
  GeInt, GeReal, GeFlags,
  EqBool, EqChar, EqInt, EqReal, EqType, EqFlags, EqAddr,
  NeBool, NeChar, NeInt, NeReal, NeType, NeFlags, NeAddr,
  
  XorBool, XorFlags,
  OrBool, OrFlags,
  AndBool, AndFlags,

  CreateStruct, FinalizeStruct,

  Malloc, Free, Alloca,

  Arrow, Ptr,

  VariantType, VariantValue,
  PtrIncr, Field,
  // clang-format on

  InsertField,
  AddCodeBlock, // TODO remove codeblock
  Print,
  Store,
  SetReturn,
  Phi, Call,
  Tup, Variant, Array, 
  Contextualize,
  BlockSeq, BlockSeqContains,
  Cast,
  CondJump,
  UncondJump,
  ReturnJump,
};

template <typename T>
struct RegisterOr {
  static_assert(!std::is_same_v<Register, T>);
  RegisterOr(Register reg) : reg_(reg), is_reg_(true) {}
  RegisterOr(T val) : val_(val), is_reg_(false) {}

  union {
    Register reg_;
    T val_;
  };
  bool is_reg_;
};

struct Cmd {
  template <typename T, size_t Index> struct CommandOpCode {
    constexpr static size_t index = Index;

    template <typename... Args >
    static T Make(Args&&... args) { return T{{}, std::forward<Args>(args)...}; }
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
  CMD(ArrayLength) { RegisterOr<IR::Addr> arg_; };
  CMD(ArrayData) { RegisterOr<IR::Addr> arg_; };

  CMD(LoadBool) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadChar) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadInt) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadReal) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadType) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadEnum) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadFlags) { RegisterOr<IR::Addr> arg_; };
  CMD(LoadAddr) { RegisterOr<IR::Addr> arg_; };

  CMD(AddInt) { RegisterOr<i32> args_[2]; };
  CMD(AddReal) { RegisterOr<double> args_[2]; };
  CMD(AddCharBuf) { RegisterOr<std::string_view> args_[2]; };
  CMD(SubInt) { RegisterOr<i32> args_[2]; };
  CMD(SubReal) { RegisterOr<double> args_[2]; };
  CMD(MulInt) { RegisterOr<i32> args_[2]; };
  CMD(MulReal) { RegisterOr<double> args_[2]; };
  CMD(DivInt) { RegisterOr<i32> args_[2]; };
  CMD(DivReal) { RegisterOr<double> args_[2]; };
  CMD(ModInt) { RegisterOr<i32> args_[2]; };
  CMD(ModReal) { RegisterOr<double> args_[2]; };

  CMD(LtInt) { RegisterOr<i32> args_[2]; };
  CMD(LtReal) { RegisterOr<double> args_[2]; };
  CMD(LtFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(LeInt) { RegisterOr<i32> args_[2]; };
  CMD(LeReal) { RegisterOr<double> args_[2]; };
  CMD(LeFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(GtInt) { RegisterOr<i32> args_[2]; };
  CMD(GtReal) { RegisterOr<double> args_[2]; };
  CMD(GtFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(GeInt) { RegisterOr<i32> args_[2]; };
  CMD(GeReal) { RegisterOr<double> args_[2]; };
  CMD(GeFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(EqBool) { RegisterOr<bool> args_[2]; };
  CMD(EqChar) { RegisterOr<char> args_[2]; };
  CMD(EqInt) { RegisterOr<i32> args_[2]; };
  CMD(EqReal) { RegisterOr<double> args_[2]; };
  CMD(EqType) { RegisterOr<type::Type const *> args_[2]; };
  CMD(EqFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(EqAddr) { RegisterOr<IR::Addr> args_[2]; };
  CMD(NeBool) { RegisterOr<bool> args_[2]; };
  CMD(NeChar) { RegisterOr<char> args_[2]; };
  CMD(NeInt) { RegisterOr<i32> args_[2]; };
  CMD(NeReal) { RegisterOr<double> args_[2]; };
  CMD(NeType) { RegisterOr<type::Type const *> args_[2]; };
  CMD(NeFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(NeAddr) { RegisterOr<IR::Addr> args_[2]; };

  CMD(XorBool) { RegisterOr<bool> args_[2]; };
  CMD(XorFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(OrBool) { RegisterOr<bool> args_[2]; };
  CMD(OrFlags) { RegisterOr<FlagsVal> args_[2]; };
  CMD(AndBool) { RegisterOr<bool> args_[2]; };
  CMD(AndFlags) { RegisterOr<FlagsVal> args_[2]; };

  CMD(CreateStruct) {};
  CMD(FinalizeStruct) { Register reg_; };

  CMD(Malloc) { RegisterOr<i32> arg_; };
  CMD(Free) { Register reg_; };
  CMD(Alloca) {};

  CMD(Ptr) { Register reg_; };
  CMD(Arrow) { RegisterOr<type::Type const *> args_[2]; };
  CMD(VariantType) { Register reg_; };
  CMD(VariantValue) { Register reg_; };
  CMD(PtrIncr) {
    Register ptr_;
    RegisterOr<i32> incr_;
  };
  CMD(Field) {
    Register ptr_;
    size_t num_;
  };

#undef CMD

  operator IR::Val() const { return reg(); }

  Cmd(const type::Type *t, Op op, base::vector<Val> args);
  base::vector<Val> args;
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

    AddInt add_int_;
    AddReal add_real_;
    AddCharBuf add_char_buf_;
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
    EqFlags eq_flags_;
    EqAddr eq_addr_;
    NeBool ne_bool_;
    NeChar ne_char_;
    NeInt ne_int_;
    NeReal ne_real_;
    NeType ne_type_;
    NeFlags ne_flags_;
    NeAddr ne_addr_;

    XorBool xor_bool_;
    XorFlags xor_flags_;
    OrBool or_bool_;
    OrFlags or_flags_;
    AndBool and_bool_;
    AndFlags and_flags_;

    CreateStruct create_struct_;
    FinalizeStruct finalize_struct_;

    Malloc malloc_;
    Free free_;
    Alloca alloca_;

    PtrIncr ptr_incr_;
    Field field_;

    Cmd::Ptr ptr_;
    Cmd::Arrow arrow_;

    Cmd::VariantType variant_type_;
    Cmd::VariantValue variant_value_;
  };

  const type::Type *type = nullptr;
  Register result;

  Val reg() const { return Val::Reg(result, type); }

  void dump(size_t indent) const;
};

Val Trunc(const Val &v);
Val Extend(const Val &v);
Val Bytes(const Val &v);
Val Align(const Val &v);
Val Not(const Val &v);
Val NegInt(const Val &v);
Val NegReal(const Val &v);
Val ArrayLength(const Val &v);
Val ArrayData(const Val &v);
Val LoadBool(const Val &v);
Val LoadChar(const Val &v);
Val LoadInt(const Val &v);
Val LoadReal(const Val &v);
Val LoadType(const Val &v);
Val LoadEnum(const Val &v);
Val LoadFlags(const Val &v);
Val LoadAddr(const Val &v);
Val AddInt(const Val &v1, const Val &v2);
Val AddReal(const Val &v1, const Val &v2);
Val SubInt(const Val &v1, const Val &v2);
Val SubReal(const Val &v1, const Val &v2);
Val MulInt(const Val &v1, const Val &v2);
Val MulReal(const Val &v1, const Val &v2);
Val DivInt(const Val &v1, const Val &v2);
Val DivReal(const Val &v1, const Val &v2);
Val LtInt(const Val &v1, const Val &v2);
Val LtReal(const Val &v1, const Val &v2);
Val LtFlags(const Val &v1, const Val &v2);
Val LeInt(const Val &v1, const Val &v2);
Val LeReal(const Val &v1, const Val &v2);
Val LeFlags(const Val &v1, const Val &v2);
Val GeInt(const Val &v1, const Val &v2);
Val GeReal(const Val &v1, const Val &v2);
Val GeFlags(const Val &v1, const Val &v2);
Val GtInt(const Val &v1, const Val &v2);
Val GtReal(const Val &v1, const Val &v2);
Val GtFlags(const Val &v1, const Val &v2);
Val EqBool(const Val &v1, const Val &v2);
Val EqChar(const Val &v1, const Val &v2);
Val EqInt(const Val &v1, const Val &v2);
Val EqReal(const Val &v1, const Val &v2);
Val EqType(const Val &v1, const Val &v2);
Val EqAddr(const Val &v1, const Val &v2);
Val NeBool(const Val &v1, const Val &v2);
Val NeChar(const Val &v1, const Val &v2);
Val NeInt(const Val &v1, const Val &v2);
Val NeReal(const Val &v1, const Val &v2);
Val NeType(const Val &v1, const Val &v2);
Val NeAddr(const Val &v1, const Val &v2);
Val XorBool(const Val &v1, const Val &v2);
Val XorFlags(const Val &v1, const Val &v2);
Val OrBool(const Val &v1, const Val &v2);
Val OrFlags(const Val &v1, const Val &v2);
Val AndBool(const Val &v1, const Val &v2);
Val AndFlags(const Val &v1, const Val &v2);
Val CreateStruct();
Val FinalizeStruct(const Val &v);
Val Malloc(const type::Type *t, const Val& v);
void Free(const Val &v);
Val Arrow(const Val &v1, const Val &v2);
Val Ptr(const Val &v);
Val VariantType(const Val &v);
Val VariantValue(const type::Type *t, const Val&);
Val PtrIncr(const Val &v1, const Val &v2);
Val Field(const Val &v, size_t n);

Val Load(const Val& v);
Val Add(const Val& v1, const Val& v2);
Val Sub(const Val &v1, const Val &v2);
Val Mul(const Val &v1, const Val &v2);
Val Div(const Val &v1, const Val &v2);
Val Mod(const Val &v1, const Val &v2);
Val Lt(const Val &v1, const Val &v2);
Val Le(const Val &v1, const Val &v2);
Val Ge(const Val &v1, const Val &v2);
Val Gt(const Val &v1, const Val &v2);
Val Eq(const Val &v1, const Val &v2);
Val Ne(const Val &v1, const Val &v2);
Val Xor(const Val &v1, const Val &v2);
Val Or(const Val &v1, const Val &v2);
Val And(const Val &v1, const Val &v2);
Val Index(const Val &v1, const Val &v2);
Val Alloca(const type::Type *t);

Val AddCodeBlock(const Val& v1, const Val& v2);
Val Call(Val fn, base::vector<Val> vals, base::vector<Val> result_locs);
Val Tup(base::vector<IR::Val> vals);
Val Variant(base::vector<Val> vals);
Val Array(Val v1, Val v2);
Val BlockSeq(base::vector<Val> blocks);
Val Cast(const type::Type *to, Val v, Context* ctx);
Val BlockSeqContains(Val v, AST::BlockLiteral *lit);

void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val);
void SetReturn(size_t n, Val v2);
void Print(Val v);
void Store(Val val, Val loc);
void CondJump(Val cond, BlockIndex true_block, BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();

CmdIndex Phi(const type::Type *t);

} // namespace IR
#endif // ICARUS_IR_CMD_H
