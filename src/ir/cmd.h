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
  PrintBool, PrintChar, PrintInt, PrintReal, PrintType, PrintEnum, PrintFlags, PrintAddr, PrintCharBuffer,
  StoreBool, StoreChar, StoreInt, StoreReal, StoreType, StoreEnum, StoreFlags, StoreAddr,

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

  CreateStruct, InsertField, FinalizeStruct,

  Malloc, Free, Alloca,

  Arrow, Ptr, Array, Tup, Variant,

  VariantType, VariantValue,
  PtrIncr, Field,

  CondJump, UncondJump, ReturnJump,

  BlockSeq, BlockSeqContains,

  Call, CastIntToReal, CastPtr,
  Phi,

  AddCodeBlock,  // TODO remove codeblock
  Contextualize,
  // clang-format on
  SetReturn,
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
  CMD(StoreFlags) {
    Register addr_;
    RegisterOr<FlagsVal> val_;
  };
  CMD(StoreAddr) {
    Register addr_;
    RegisterOr<IR::Addr> val_;
  };

  CMD(PrintBool) { RegisterOr<bool> arg_; };
  CMD(PrintChar) { RegisterOr<char> arg_; };
  CMD(PrintInt) { RegisterOr<i32> arg_; };
  CMD(PrintReal) { RegisterOr<double> arg_; };
  CMD(PrintType) { RegisterOr<type::Type const *> arg_; };
  CMD(PrintEnum) { RegisterOr<EnumVal> arg_; };
  CMD(PrintFlags) { RegisterOr<FlagsVal> arg_; };
  CMD(PrintAddr) { RegisterOr<IR::Addr> arg_; };
  CMD(PrintCharBuffer) { RegisterOr<std::string_view> arg_; };

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
  CMD(InsertField) { base::vector<Val> *args_; };
  CMD(FinalizeStruct) { Register reg_; };

  CMD(Malloc) { RegisterOr<i32> arg_; };
  CMD(Free) { Register reg_; };
  CMD(Alloca) {};

  CMD(Ptr) { Register reg_; };
  CMD(Arrow) { RegisterOr<type::Type const *> args_[2]; };
  CMD(Array) {
    RegisterOr<i32> len_;
    RegisterOr<type::Type const *> type_;
  };
  CMD(Tup) { base::vector<Val> *args_; };
  CMD(Variant) { base::vector<Val> *args_; };

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

  CMD(Call) {
    Call(Register r, base::vector<IR::Val> * args)
        : reg_(r), which_active_(0x00), args_(args) {}
    Call(Func * f, base::vector<IR::Val> * args)
        : fn_(f), which_active_(0x01), args_(args) {}
    Call(ForeignFn f, base::vector<IR::Val> * args)
        : foreign_fn_(f), which_active_(0x02), args_(args) {}
    union {
      Register reg_;
      Func *fn_;
      ForeignFn foreign_fn_;
    };
    char which_active_;
    base::vector<IR::Val> *args_;
  };

  CMD(Phi) { base::vector<Val> *args_; };

  CMD(CondJump) {
    Register cond_;
    BlockIndex blocks_[2];
  };
  CMD(UncondJump) { BlockIndex block_; };
  CMD(ReturnJump){};

  CMD(CastIntToReal) { Register reg_; };
  CMD(CastPtr) {
    Register reg_;
    type::Type const *type_;
  };

  CMD(BlockSeq) { base::vector<Val> *args_; };
  CMD(BlockSeqContains) {
    Register reg_;
    AST::BlockLiteral *lit_;
  };

#undef CMD

  operator IR::Val() const { return reg(); }

  Cmd(const type::Type *t, Op op, base::vector<Val> args, bool do_it = false);
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

    StoreBool store_bool_;
    StoreChar store_char_;
    StoreInt store_int_;
    StoreReal store_real_;
    StoreType store_type_;
    StoreEnum store_enum_;
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
    InsertField insert_field_;
    FinalizeStruct finalize_struct_;

    Malloc malloc_;
    Free free_;
    Alloca alloca_;

    PtrIncr ptr_incr_;
    Field field_;

    Cmd::Ptr ptr_;
    Cmd::Arrow arrow_;
    Cmd::Array array_;
    Cmd::Tup tup_;
    Cmd::Variant variant_;

    CondJump cond_jump_;
    UncondJump uncond_jump_;
    ReturnJump return_jump_;

    Cmd::VariantType variant_type_;
    Cmd::VariantValue variant_value_;

    Call call_;
    CastIntToReal cast_int_to_real_;
    CastPtr cast_ptr_;
    Phi phi_;

    BlockSeq block_seq_;
    BlockSeqContains block_seq_contains_;
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
void StoreBool(const Val &v, const Val &addr);
void StoreChar(const Val &v, const Val &addr);
void StoreInt(const Val &v, const Val &addr);
void StoreReal(const Val &v, const Val &addr);
void StoreType(const Val &v, const Val &addr);
void StoreEnum(const Val &v, const Val &addr);
void StoreFlags(const Val &v, const Val &addr);
void StoreAddr(const Val &v, const Val &addr);
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
void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val);
Val FinalizeStruct(const Val &v);
Val Malloc(const type::Type *t, const Val& v);
void Free(const Val &v);
Val Arrow(const Val &v1, const Val &v2);
Val Ptr(const Val &v);
Val Array(const Val &v1, const Val &v2);
Val VariantType(const Val &v);
Val VariantValue(const type::Type *t, const Val&);
Val PtrIncr(const Val &v1, const Val &v2);
Val Field(const Val &v, size_t n);
Val PrintBool(const Val &v);
Val PrintChar(const Val &v);
Val PrintInt(const Val &v);
Val PrintReal(const Val &v);
Val PrintType(const Val &v);
Val PrintEnum(const Val &v);
Val PrintFlags(const Val &v);
Val PrintAddr(const Val &v);
Val PrintCharBuffer(const Val &v);
Val Call(const Val &fn, base::vector<Val> vals, base::vector<Val> result_locs);
Val Tup(base::vector<IR::Val> vals);
Val Variant(base::vector<Val> vals);
void CondJump(const Val &cond, BlockIndex true_block, BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();
Val BlockSeq(const base::vector<Val>& blocks);
Val BlockSeqContains(const Val& v, AST::BlockLiteral *lit);

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
Val Print(const Val& v);
Val Cast(const type::Type *to, const Val& v, Context* ctx);
void Store(const Val &val, const Val &loc);

void SetReturn(size_t n, Val v2);
std::pair<CmdIndex, base::vector<IR::Val> *> Phi(const type::Type *t);

Val AddCodeBlock(const Val& v1, const Val& v2);
} // namespace IR
#endif // ICARUS_IR_CMD_H
