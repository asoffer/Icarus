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
  // clang-format on

  Or, And,
  AddCodeBlock, // TODO remove codeblock
  Eq, Ne, // numeric types only
  Xor,
  Print,
  Malloc, Free,
  Store,
  SetReturn,
   PtrIncr,
  Phi, Field, Call,
  Tup, Arrow, Variant, Array, Ptr,
  Alloca,
  Contextualize,
  VariantType, VariantValue,
  BlockSeq, BlockSeqContains,
  Cast,
  CondJump,
  UncondJump,
  ReturnJump,
  CreateStruct,
  InsertField,
  FinalizeStruct,
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
  CMD(Ptr) { Register reg_; };
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
    Cmd::Ptr ptr_;
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
Val Ptr(const Val &v);
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

Val AddCodeBlock(const Val& v1, const Val& v2);
Val Eq(Val v1, Val v2);
Val Ne(Val v1, Val v2);
Val Xor(Val v1, Val v2);
Val Or(Val v1, Val v2);
Val And(Val v1, Val v2);
Val Index(Val v1, Val v2);
Val Call(Val fn, base::vector<Val> vals, base::vector<Val> result_locs);
Val PtrIncr(Val v1, Val v2);
Val Malloc(const type::Type *t, Val v);
Val Field(Val v, size_t n);
Val Tup(base::vector<IR::Val> vals);
Val Arrow(Val v1, Val v2);
Val Variant(base::vector<Val> vals);
Val Array(Val v1, Val v2);
Val Alloca(const type::Type *t);
Val VariantType(Val v1);
Val VariantValue(const type::Type *t, Val);
Val BlockSeq(base::vector<Val> blocks);
Val CreateStruct();
Val FinalizeStruct(Val v);
Val Cast(const type::Type *to, Val v, Context* ctx);
Val BlockSeqContains(Val v, AST::BlockLiteral *lit);

void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val);
void SetReturn(size_t n, Val v2);
void Print(Val v);
void Store(Val val, Val loc);
void Free(Val v);
void CondJump(Val cond, BlockIndex true_block, BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();

CmdIndex Phi(const type::Type *t);

} // namespace IR
#endif // ICARUS_IR_CMD_H
