#ifndef ICARUS_IR_CMD_H
#define ICARUS_IR_CMD_H

#include "val.h"

namespace IR {
enum class Op : char {
  Trunc, Extend,
  Neg, // ! for bool, - for numeric types
  Or, And,
  Add, Sub, Mul, Div, Mod, // numeric types only
  Lt, Le, Eq, Ne, Gt, Ge, // numeric types only
  Xor,
  Print,
  Malloc, Free,
  Load, Store,
  SetReturn,
  ArrayLength, ArrayData, PtrIncr,
  Phi, Field, Call,
  Tup, Arrow, Variant, Array, Ptr,
  Alloca,
  Contextualize,
  VariantType, VariantValue,
  BlockSeq, BlockSeqContains,
  Err,
  Cast,
  CondJump,
  UncondJump,
  ReturnJump,
  CreateStruct,
  InsertField,
  FinalizeStruct,
};

struct Cmd {
  // For pre/post-conditions, we generate blocks of code in the same function
  // but which would otherwise be unreachable from the normal execution paths of
  // that function. This enum attached to each command indicates what kind of
  // command, so we know not to emit this during code gen or during execution if
  // it's not 'Exec'.
  enum class Kind : char { Exec, PreCondition, PostCondition };

  Cmd(const type::Type *t, Op op, std::vector<Val> args);
  std::vector<Val> args;
  Op op_code_;
  Kind kind_ = Kind::Exec;

  const type::Type *type = nullptr;
  Register result;

  Val reg() const { return Val::Reg(result, type); }

  void dump(size_t indent) const;
};

Val Neg(Val v);
Val Trunc(Val v);
Val Extend(Val v);
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
Val Call(Val fn, std::vector<Val> vals, std::vector<Val> result_locs);
Val Load(Val v);
Val ArrayLength(Val v);
Val ArrayData(Val v);
Val PtrIncr(Val v1, Val v2);
Val Malloc(const type::Type *t, Val v);
Val Field(Val v, size_t n);
Val Tup(std::vector<IR::Val> vals);
Val Arrow(Val v1, Val v2);
Val Variant(std::vector<Val> vals);
Val Array(Val v1, Val v2);
Val Ptr(Val v1);
Val Alloca(const type::Type *t);
Val VariantType(Val v1);
Val VariantValue(const type::Type *t, Val);
Val BlockSeq(std::vector<Val> blocks);
Val Err(Val v);
Val CreateStruct();
Val FinalizeStruct(Val v);
Val Cast(const type::Type *to, Val v);
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
