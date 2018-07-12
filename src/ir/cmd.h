#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "context.h"
#include "val.h"

namespace IR {
enum class Op : char {
  Trunc, Extend, Bytes, Align, Not, NegInt, NegReal, ArrayLength, ArrayData,
  LoadBool, LoadChar, LoadInt, LoadReal, LoadType, LoadEnum, LoadFlags,

  Load,
  Or, And,
  Add, Sub, Mul, Div, Mod, // numeric types only
  Lt, Le, Eq, Ne, Gt, Ge, // numeric types only
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
  };

  const type::Type *type = nullptr;
  Register result;

  Val reg() const { return Val::Reg(result, type); }

  void dump(size_t indent) const;
};

Val Trunc(Val v);
Val Extend(Val v);
Val Bytes(Val v);
Val Align(Val v);
Val Not(Val v);
Val NegInt(Val v);
Val NegReal(Val v);
Val ArrayLength(Val v);
Val ArrayData(Val v);
Val Ptr(Val v);
Val LoadBool(Val v);
Val LoadChar(Val v);
Val LoadInt(Val v);
Val LoadReal(Val v);
Val LoadType(Val v);
Val LoadEnum(Val v);
Val LoadFlags(Val v);

Val Load(Val v);
Val Add(Val v1, Val v2);
Val Sub(Val v1, Val v2);
Val Mul(Val v1, Val v2);
Val Div(Val v1, Val v2);
Val Mod(Val v1, Val v2);
Val Lt(Val v1, Val v2);
Val Le(Val v1, Val v2);
Val Eq(Val v1, Val v2);
Val Ne(Val v1, Val v2);
Val Ge(Val v1, Val v2);
Val Gt(Val v1, Val v2);
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
