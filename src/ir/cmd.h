#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "context.h"
#include "val.h"
#include "base/untyped_buffer.h"

namespace type {
struct Function;
}  // namespace type

namespace IR {
struct LongArgs {
  void append(const IR::Val &val);
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
  Val AppendReg(type::Type const *);
  void AppendLoc(Register reg) { outs_.emplace_back(reg, true); }
  base::vector<OutParam> outs_;
};

enum class Op : char {
  // clang-format off
  Death,
  Trunc, Extend,
  Bytes, Align,
  Not,
  NegInt, NegReal,
  ArrayLength, ArrayData,

  LoadBool, LoadChar, LoadInt, LoadReal, LoadType, LoadEnum, LoadFlags, LoadAddr,
  PrintBool, PrintChar, PrintInt, PrintReal, PrintType, PrintEnum, PrintFlags, PrintAddr, PrintCharBuffer,
  StoreBool, StoreChar, StoreInt, StoreReal, StoreType, StoreEnum, StoreFlags, StoreAddr,
  SetReturnBool, SetReturnChar, SetReturnInt, SetReturnReal, SetReturnType, SetReturnEnum,
  SetReturnCharBuf, SetReturnFlags, SetReturnAddr, SetReturnFunc, SetReturnScope,
  SetReturnModule, SetReturnGeneric, SetReturnBlock,

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
  
  XorBool,
  XorFlags, OrFlags, AndFlags,

  CreateStruct, InsertField, FinalizeStruct,

  Malloc, Free, Alloca,

  Arrow, Ptr, Array, Tup, Variant,

  VariantType, VariantValue,
  PtrIncr, Field,

  CondJump, UncondJump, ReturnJump,

  BlockSeq, BlockSeqContains,

  Call, CastIntToReal, CastPtr,
  PhiBool, PhiChar, PhiInt, PhiReal, PhiType, PhiBlock, PhiAddr,

  AddCodeBlock,  // TODO remove codeblock
  Contextualize,
  // clang-format on
};

struct GenericPhiArgs : public base::Cast<GenericPhiArgs> {
  virtual ~GenericPhiArgs() {}
};
template <typename T>
struct PhiArgs : GenericPhiArgs {
  ~PhiArgs() override {}
  std::unordered_map<BlockIndex, RegisterOr<T>> map_;
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

  CMD(AddInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(AddReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(AddCharBuf) { std::array<RegisterOr<std::string_view>, 2> args_; };
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
  CMD(EqBool) { std::array<RegisterOr<bool>, 2> args_; };
  CMD(EqChar) { std::array<RegisterOr<char>, 2> args_; };
  CMD(EqInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(EqReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(EqType) { std::array<RegisterOr<type::Type const *>, 2> args_; };
  CMD(EqFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(EqAddr) { std::array<RegisterOr<IR::Addr>, 2> args_; };
  CMD(NeBool) { std::array<RegisterOr<bool>, 2> args_; };
  CMD(NeChar) { std::array<RegisterOr<char>, 2> args_; };
  CMD(NeInt) { std::array<RegisterOr<i32>, 2> args_; };
  CMD(NeReal) { std::array<RegisterOr<double>, 2> args_; };
  CMD(NeType) { std::array<RegisterOr<type::Type const *>, 2> args_; };
  CMD(NeFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(NeAddr) { std::array<RegisterOr<IR::Addr>, 2> args_; };

  CMD(XorBool) { std::array<RegisterOr<bool>, 2> args_; };

  CMD(XorFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(OrFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };
  CMD(AndFlags) { std::array<RegisterOr<FlagsVal>, 2> args_; };

  CMD(CreateStruct){};
  CMD(InsertField) { base::vector<Val> *args_; };
  CMD(FinalizeStruct) { Register reg_; };

  CMD(Malloc) { RegisterOr<i32> arg_; };
  CMD(Free) { Register reg_; };
  CMD(Alloca){};

  CMD(Ptr) { Register reg_; };
  CMD(Arrow) { std::array<RegisterOr<type::Type const *>, 2> args_; };
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
    type::Struct const *struct_type_;
    size_t num_;
  };

  CMD(Call) {
    Call(Register r, LongArgs * args, OutParams * outs)
        : reg_(r), which_active_(0x00), long_args_(args), outs_(outs) {}
    Call(Func * f, LongArgs * args, OutParams * outs)
        : fn_(f), which_active_(0x01), long_args_(args), outs_(outs) {}
    Call(ForeignFn f, LongArgs * args, OutParams * outs)
        : foreign_fn_(f), which_active_(0x02), long_args_(args), outs_(outs) {}
    union {
      Register reg_;
      Func *fn_;
      ForeignFn foreign_fn_;
    };
    char which_active_;
    LongArgs *long_args_;
    OutParams *outs_;
  };

  CMD(PhiBool) { PhiArgs<bool> *args_; };
  CMD(PhiChar) { PhiArgs<char> *args_; };
  CMD(PhiInt) { PhiArgs<i32> *args_; };
  CMD(PhiReal) { PhiArgs<double> *args_; };
  CMD(PhiType) { PhiArgs<type::Type const *> *args_; };
  CMD(PhiBlock) { PhiArgs<BlockSequence> *args_; };
  CMD(PhiAddr) { PhiArgs<IR::Addr> *args_; };

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
    RegisterOr<IR::Addr> val_;
  };

  CMD(SetReturnCharBuf) {
    size_t ret_num_;
    RegisterOr<std::string_view> val_;
  };

  CMD(SetReturnFunc) {
    size_t ret_num_;
    RegisterOr<IR::AnyFunc> val_;
  };

  CMD(SetReturnScope) {
    size_t ret_num_;
    RegisterOr<AST::ScopeLiteral *> val_;
  };

  CMD(SetReturnModule) {
    size_t ret_num_;
    RegisterOr<Module const *> val_;
  };

  CMD(SetReturnGeneric) {
    size_t ret_num_;
    RegisterOr<AST::Function *> val_;
  };


  CMD(SetReturnBlock) {
    size_t ret_num_;
    RegisterOr<BlockSequence> val_;
  };
#undef CMD

  operator IR::Val() const { return reg(); }

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
    OrFlags or_flags_;
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
    PhiBool phi_bool_;
    PhiChar phi_char_;
    PhiInt phi_int_;
    PhiReal phi_real_;
    PhiType phi_type_;
    PhiBlock phi_block_;
    PhiAddr phi_addr_;

    BlockSeq block_seq_;
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
    SetReturnModule set_return_module_;
    SetReturnGeneric set_return_generic_;
    SetReturnBlock set_return_block_;
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
Val OrFlags(const Val &v1, const Val &v2);
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
void Call(const Val &fn, LongArgs long_args);
void Call(const Val &fn, LongArgs long_args, IR::OutParams outs);
Val Tup(base::vector<IR::Val> vals);
Val Variant(base::vector<Val> vals);
void CondJump(const Val &cond, BlockIndex true_block, BlockIndex false_block);
void UncondJump(BlockIndex block);
void ReturnJump();
Val BlockSeq(const base::vector<Val> &blocks);
Val BlockSeqContains(const Val &v, AST::BlockLiteral *lit);
void SetReturnBool(size_t n, const Val &v2);
void SetReturnChar(size_t n, const Val &v2);
void SetReturnInt(size_t n, const Val &v2);
void SetReturnReal(size_t n, const Val &v2);
void SetReturnType(size_t n, const Val &v2);
void SetReturnEnum(size_t n, const Val &v2);
void SetReturnFlags(size_t n, const Val &v2);
void SetReturnCharBuf(size_t n, const Val &v2);
void SetReturnAddr(size_t n, const Val &v2);
void SetReturnFunc(size_t n, const Val &v2);
void SetReturnScope(size_t n, const Val &v2);
void SetReturnModule(size_t n, const Val &v2);
void SetReturnGeneric(size_t n, const Val &v2);
void SetReturnBlock(size_t n, const Val &v2);

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
Val Index(const Val &v1, const Val &v2);
Register Alloca(const type::Type *t);
Val Print(const Val& v);
Val Cast(const type::Type *to, const Val& v, Context* ctx);
void Store(const Val &val, const Val &loc);

void SetReturn(size_t n, Val const &v2);

CmdIndex Phi(type::Type const *);
Val MakePhi(CmdIndex phi_index,
             const std::unordered_map<BlockIndex, IR::Val> &val_map);

Val AddCodeBlock(const Val& v1, const Val& v2);
} // namespace IR
#endif // ICARUS_IR_CMD_H
