#include "ir/cmd.h"

#include <cmath>
#include <iostream>

#include "architecture.h"
#include "base/container/vector.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "ir/val.h"
#include "type/all.h"
#include "type/typed_value.h"

namespace ir {
using base::check::Is;
BlockIndex BasicBlock::Current;

// TODO namespace appropriately
Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(BasicBlock::Current)
                  .cmds_.emplace_back(t, op);
  return cmd;
}

// TODO pass atyped_reg? not sure that's right.
Register CastPtr(Register r, type::Pointer const *t) {
  auto &cmd     = ir::MakeCmd(t, Op::CastPtr);
  cmd.typed_reg_ = type::Typed<Register>(r, t);
  return cmd.result;
}

RegisterOr<char> Trunc(RegisterOr<i32> r) {
  if (!r.is_reg_) { return static_cast<char>(r.val_); }
  auto &cmd = MakeCmd(type::Char, Op::Trunc);
  cmd.reg_  = r.reg_;
  return cmd.result;
}

RegisterOr<i32> Extend(RegisterOr<char> r) {
  if (!r.is_reg_) { return static_cast<i32>(r.val_); }
  auto &cmd = MakeCmd(type::Int32, Op::Extend);
  cmd.reg_  = r.reg_;
  return cmd.result;
}

RegisterOr<i32> Bytes(RegisterOr<type::Type const *> r) {
  auto &cmd  = MakeCmd(type::Int32, Op::Bytes);
  cmd.type_arg_ = r;
  return cmd.result;
}

RegisterOr<i32> Align(RegisterOr<type::Type const *> r) {
  auto &cmd  = MakeCmd(type::Int32, Op::Align);
  cmd.type_arg_ = r;
  return cmd.result;
}

template <typename T>
Val CastTo(type::Type const *from, Val const &val, Op op) {
  if (auto *r = std::get_if<Register>(&val.value)) {
    auto *to       = type::Get<T>();
    auto &cmd      = MakeCmd(to, op);
    cmd.typed_reg_ = type::Typed<Register>(*r, from);
    return ir::Val::Reg(cmd.result, to);
  } else {
    return type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float>(
        from, [&](auto type_holder) {
          using FromType = typename decltype(type_holder)::type;
          return ir::Val(static_cast<T>(std::get<FromType>(val.value)));
        });
  }
  UNREACHABLE("To ", type::Get<T>(), " from ", from);
}

Val Cast(type::Type const *from, type::Type const *to, Val const &val) {
  if (from == to) { return val; }
  // TODO Shouldn't be any reason we need to pass the opcode in.
  if (to == type::Int16) { return CastTo<i16>(from, val, Op::CastToInt16); }
  if (to == type::Nat16) { return CastTo<u16>(from, val, Op::CastToNat16); }
  if (to == type::Int32) { return CastTo<i32>(from, val, Op::CastToInt32); }
  if (to == type::Nat32) { return CastTo<u32>(from, val, Op::CastToNat32); }
  if (to == type::Int64) { return CastTo<i64>(from, val, Op::CastToInt64); }
  if (to == type::Nat64) { return CastTo<u64>(from, val, Op::CastToNat64); }
  if (to == type::Float32) {
    return CastTo<float>(from, val, Op::CastToFloat32);
  }
  if (to == type::Float64) {
    return CastTo<double>(from, val, Op::CastToFloat64);
  }
  UNREACHABLE();
}

RegisterOr<bool> Not(RegisterOr<bool> r) {
  if (!r.is_reg_) { return !r.val_; }
  auto &cmd = MakeCmd(type::Bool, Op::NotBool);
  cmd.reg_ = r.reg_;
  Func::Current->references_[cmd.reg_].insert(cmd.result);
  return cmd.result;
}

RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r) {
  if (!r->is_reg_) { return NotFlags(r->val_, r.type()); }
  auto &cmd      = MakeCmd(r.type(), Op::NotFlags);
  cmd.typed_reg_ = type::Typed<Register>(r->reg_, r.type());
  return cmd.result;
}

// TODO do you really want to support this? How can array allocation be
// customized?
TypedRegister<Addr> Malloc(const type::Type *t, RegisterOr<i32> r) {
  auto &cmd    = MakeCmd(type::Ptr(t), Op::Malloc);
  cmd.i32_arg_ = r;
  return cmd.result;
}

void Free(Register r) { MakeCmd(nullptr, Op::Free).reg_ = r; }

Register ArrayLength(Register r) {
  auto &cmd = MakeCmd(type::Ptr(type::Int64), Op::ArrayLength);
  cmd.reg_  = r;
  return cmd.result;
}

Register ArrayData(Register r, type::Type const *ptr) {
  auto *array_type = &ptr->as<type::Pointer>().pointee->as<type::Array>();
  ASSERT(!array_type->fixed_length);

  auto &cmd = MakeCmd(type::Ptr(array_type->data_type), Op::ArrayData);
  cmd.reg_  = r;
  return cmd.result;
}

RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::Ptr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::Ptr);
  cmd.reg_ = r.reg_;
  return cmd.result;
}

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> v1,
                                     RegisterOr<type::Type const *> v2) {
  if (!v1.is_reg_ && !v2.is_reg_) {
    base::vector<type::Type const *> ins =
        v1.val_->is<type::Tuple>() ? v1.val_->as<type::Tuple>().entries_
                                   : base::vector<type::Type const *>{v1.val_};
    base::vector<type::Type const *> outs =
        v2.val_->is<type::Tuple>() ? v2.val_->as<type::Tuple>().entries_
                                   : base::vector<type::Type const *>{v2.val_};
    return type::Func(std::move(ins), std::move(outs));
  }
  auto &cmd = MakeCmd(type::Type_, Op::Arrow);
  cmd.set<Cmd::ArrowTag, type::Type const*>(v1, v2);
  auto &refs = Func::Current->references_;
  if (v1.is_reg_) { refs[v1.reg_].insert(cmd.result); }
  if (v2.is_reg_) { refs[v2.reg_].insert(cmd.result); }
  return cmd.result;
}

RegisterOr<type::Type const *> Array(RegisterOr<type::Type const *> data_type) {
  if (!data_type.is_reg_) { return type::Arr(data_type.val_); }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = {-1, data_type};
  return cmd.result;
}

RegisterOr<type::Type const *> Array(RegisterOr<i32> len,
                                     RegisterOr<type::Type const *> data_type) {
  if (!data_type.is_reg_ && !len.is_reg_) {
    return type::Arr(data_type.val_, len.val_);
  }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = {len, data_type};
  return cmd.result;
}

Register CreateTuple() { return MakeCmd(type::Type_, Op::CreateTuple).result; }

void AppendToTuple(Register tup, RegisterOr<type::Type const *> entry) {
  auto &cmd       = MakeCmd(nullptr, Op::AppendToTuple);
  cmd.store_type_ = {tup, entry};
}

Register FinalizeTuple(Register r) {
  auto &cmd           = MakeCmd(type::Type_, Op::FinalizeTuple);
  cmd.reg_ = r;
  return cmd.result;
}

RegisterOr<type::Type const *> Tup(base::vector<Val> const &entries) {
  if (std::all_of(entries.begin(), entries.end(), [](ir::Val const &v) {
        return std::holds_alternative<type::Type const *>(v.value);
      })) {
    std::vector<type::Type const *> types;
    for (auto const &val : entries) {
      types.push_back(std::get<type::Type const *>(val.value));
    }
    return type::Tup(std::move(types));
  }

  ir::Register tup = ir::CreateTuple();
  for (auto const &val : entries) {
    ir::AppendToTuple(tup, val.reg_or<type::Type const *>());
  }
  return ir::FinalizeTuple(tup);
}

Register CreateVariant() {
  return MakeCmd(type::Type_, Op::CreateVariant).result;
}

void AppendToVariant(Register var, RegisterOr<type::Type const *> entry) {
  auto &cmd       = MakeCmd(nullptr, Op::AppendToVariant);
  cmd.store_type_ = {var, entry};
}

Register FinalizeVariant(Register r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeVariant);
  cmd.reg_  = r;
  return cmd.result;
}

RegisterOr<type::Type const *> Variant(base::vector<Val> const &vals) {
  if (std::all_of(vals.begin(), vals.end(), [](Val const &v) {
        return std::holds_alternative<type::Type const *>(v.value);
      })) {
    base::vector<type::Type const *> types;
    types.reserve(vals.size());
    for (Val const &v : vals) {
      types.push_back(std::get<type::Type const *>(v.value));
    }

    return type::Var(std::move(types));
  }
  ir::Register var = ir::CreateVariant();
  for (auto const &val : vals) {
    ir::AppendToVariant(var, val.reg_or<type::Type const *>());
  }
  return ir::FinalizeVariant(var);
}

RegisterOr<bool> XorBool(RegisterOr<bool> v1, RegisterOr<bool> v2) {
  if (!v1.is_reg_) { return v1.val_ ? Not(v2) : v2; }
  if (!v2.is_reg_) { return v2.val_ ? Not(v1) : v1; }
  auto &cmd = MakeCmd(type::Bool, Op::XorBool);
  cmd.set<Cmd::XorTag, bool>(v1, v2);
  return cmd.result;
}

Register Field(Register r, type::Struct const *t, size_t n) {
  auto &cmd  = MakeCmd(type::Ptr(t->fields().at(n).type), Op::Field);
  cmd.field_ = {r, t, n};
  return cmd.result;
}

static Register Reserve(type::Type const *t, bool incr_num_regs = true) {
  auto arch    = Architecture::InterprettingMachine();
  auto reg_val = arch.MoveForwardToAlignment(t, Func::Current->reg_size_);
  auto result  = Register(reg_val);
  Func::Current->reg_size_ = reg_val + arch.bytes(t);
  if (incr_num_regs) {
    Func::Current->reg_map_.emplace(Func::Current->num_regs_, result);
    ++Func::Current->num_regs_;
  }
  return result;
}

Cmd::Cmd(const type::Type *t, Op op) : op_code_(op) {
  ASSERT(Func::Current != nullptr);
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  if (t == nullptr) {
    result = Register{--Func::Current->neg_bound_};
    Func::Current->references_[result];  // Guarantee it exists.
    Func::Current->reg_to_cmd_.emplace(result, cmd_index);
    Func::Current->reg_map_.emplace(Func::Current->neg_bound_, result);
    return;
  }

  result = Reserve(t);
  Func::Current->references_[result];  // Guarantee it exists.
  // TODO for implicitly declared out-params of a Call, map them to the call.
  Func::Current->reg_to_cmd_.emplace(result, cmd_index);
}

Register OutParams::AppendReg(type::Type const *t) {
  auto reg = Reserve(t, false);
  outs_.emplace_back(reg, false);
  return reg;
}

BlockSequence MakeBlockSeq(base::vector<ir::BlockSequence> const &blocks);

Register CreateBlockSeq() {
  return MakeCmd(type::Type_, Op::CreateBlockSeq).result;
}

void AppendToBlockSeq(Register block_seq,
                      RegisterOr<ir::BlockSequence> more_block_seq) {
  auto &cmd = MakeCmd(nullptr, Op::AppendToBlockSeq);
  cmd.store_block_ = {block_seq, more_block_seq};
}

Register FinalizeBlockSeq(Register r) {
  auto &cmd = MakeCmd(type::Block, Op::FinalizeBlockSeq);
  cmd.reg_  = r;
  return cmd.result;
}

// TODO replace Val with RegOr<BlockSequence>
Val BlockSeq(const base::vector<Val> &blocks) {
  if (std::all_of(blocks.begin(), blocks.end(), [](const ir::Val &v) {
        return std::holds_alternative<ir::BlockSequence>(v.value);
      })) {
    std::vector<ir::BlockSequence> block_seqs;
    block_seqs.reserve(blocks.size());
    for (const auto &val : blocks) {
      block_seqs.push_back(std::get<ir::BlockSequence>(val.value));
    }
    return ir::Val::BlockSeq(MakeBlockSeq(block_seqs));
  }

  auto reg = CreateBlockSeq();
  for (auto const &val : blocks) {
    ir::AppendToBlockSeq(reg, val.reg_or<ir::BlockSequence>());
  }
  // TODO can it be an opt block?
  return ir::Val::Reg(ir::FinalizeBlockSeq(reg), type::Block);
}

RegisterOr<bool> BlockSeqContains(RegisterOr<BlockSequence> r,
                                  ast::BlockLiteral *lit) {
  if (r.is_reg_) {
    auto &cmd               = MakeCmd(type::Bool, Op::BlockSeqContains);
    cmd.block_seq_contains_ = Cmd::BlockSeqContains{r.reg_, lit};
    return cmd.result;
  }

  return std::any_of(r.val_.seq_->begin(), r.val_.seq_->end(),
                     [lit](ast::BlockLiteral *l) { return lit == l; });
}

Register CreateStruct(ast::StructLiteral *lit) {
  auto &cmd       = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.struct_lit_ = lit;
  return cmd.result;
}

Register FinalizeStruct(Register r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeStruct);
  cmd.reg_  = r;
  return cmd.result;
}

void DebugIr() { MakeCmd(nullptr, Op::DebugIr); }

Register VariantType(Register r) {
  auto &cmd = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.reg_  = r;
  return cmd.result;
}

Register VariantValue(type::Type const *t, Register r) {
  auto &cmd = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.reg_  = r;
  return cmd.result;
}

void CreateStructField(type::Struct *struct_type,
                       RegisterOr<type::Type const *> type) {
  auto &cmd = MakeCmd(nullptr, Op::CreateStructField);
  cmd.create_struct_field_ = {struct_type, std::move(type)};
}

void SetStructFieldName(type::Struct *struct_type,
                        std::string_view field_name) {
  auto &cmd = MakeCmd(nullptr, Op::SetStructFieldName);
  cmd.set_struct_field_name_ = {struct_type, field_name};
}

TypedRegister<Addr> Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));

  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(Func::Current->entry())
                  .cmds_.emplace_back(type::Ptr(t), Op::Alloca);
  cmd.type_ = t;
  return cmd.result;
}

void SetRet(size_t n, Val const &v) {
  ASSERT(v.type != nullptr);

  if (v.type->is<type::Function>()) {
    return std::visit(
        [&](auto &val) {
          using val_t = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<val_t, ir::Func *> ||
                        std::is_same_v<val_t, ir::ForeignFn>) {
            return SetRet(n, ir::AnyFunc{val});
          } else if constexpr (std::is_same_v<val_t, ir::Register>) {
            return SetRet(n, RegisterOr<AnyFunc>(val));
          } else {
            UNREACHABLE(val);
          }
        },
        v.value);
  }
  if (v.type == type::Generic) {
    return SetRet(n, v.reg_or<ast::FunctionLiteral *>());
  }

  if (v.type == type::Scope) {
    return SetRet(n, v.reg_or<ast::ScopeLiteral *>());
  }
  if (v.type == type::Module) { return SetRet(n, v.reg_or<Module const *>()); }
  if (v.type == type::Block || v.type == type::OptBlock ||
      v.type == type::RepBlock) {
    return SetRet(n, v.reg_or<BlockSequence>());
  }

  return type::Apply(v.type, [&](auto type_holder) {
    using T = typename decltype(type_holder)::type;
    return SetRet(n, v.reg_or<T>());
  });
  UNREACHABLE(v.type->to_string());
}

TypedRegister<Addr> PtrIncr(Register ptr, RegisterOr<i32> inc,
                            type::Type const *t) {
  // TODO type must be a pointer.
  if (!inc.is_reg_ && inc.val_ == 0) { return ptr; }
  auto &cmd     = MakeCmd(t, Op::PtrIncr);
  cmd.ptr_incr_ = {ptr, t->as<type::Pointer>().pointee, inc};
  return cmd.result;
}

RegisterOr<FlagsVal> XorFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ ^ rhs.val_; }
  // TODO could do a tiny bit more constant propogation for empty and full flag
  // sets here. And on OrFlags and AndFlags
  auto &cmd      = MakeCmd(type, Op::XorFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> OrFlags(type::Flags const *type,
                             RegisterOr<FlagsVal> const &lhs,
                             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ | rhs.val_; }
  auto &cmd     = MakeCmd(type, Op::OrFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> AndFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ & rhs.val_; }
  auto &cmd      = MakeCmd(type, Op::AndFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

Register Load(Register r, type::Type const *t) {
  if (t->is<type::Function>()) { return Load<AnyFunc>(r, t); }
  return type::Apply(t, [&](auto type_holder) -> Register {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, ir::EnumVal> ||
                  std::is_same_v<T, ir::FlagsVal>) {
      return Load<T>(r, t);
    } else {
      return Load<T>(r);
    }
  });
}

TypedRegister<Addr> Index(type::Type const *t, Register array_ptr,
                          RegisterOr<i32> offset) {
  auto *array_type = &t->as<type::Pointer>().pointee->as<type::Array>();
  // TODO this works but generates worse ir (both here and in llvm). It's worth
  // figuring out how to do this better.
  return PtrIncr(
      array_type->fixed_length
          ? array_ptr
          : Load(ArrayData(array_ptr, t), type::Ptr(array_type->data_type)),
      offset, type::Ptr(array_type->data_type));
}

void Call(RegisterOr<AnyFunc> const &f, Arguments arguments) {
  auto &block     = Func::Current->block(BasicBlock::Current);
  Arguments *args = &block.arguments_.emplace_back(std::move(arguments));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, nullptr);
}

void Call(RegisterOr<AnyFunc> const &f, Arguments arguments, OutParams outs) {
  auto &block    = Func::Current->block(BasicBlock::Current);
  auto *args     = &block.arguments_.emplace_back(std::move(arguments));
  auto *outs_ptr = &block.outs_.emplace_back(std::move(outs));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, outs_ptr);
}

void CondJump(RegisterOr<bool> cond, BlockIndex true_block,
              BlockIndex false_block) {
  if (!cond.is_reg_) {
    UncondJump(cond.val_ ? true_block : false_block);
    return;
  }
  auto &cmd      = MakeCmd(nullptr, Op::CondJump);
  cmd.cond_jump_ = Cmd::CondJump{cond.reg_, {false_block, true_block}};
}

void UncondJump(BlockIndex block) {
  MakeCmd(nullptr, Op::UncondJump).block_ = block;
}

void ReturnJump() { auto &cmd = MakeCmd(nullptr, Op::ReturnJump); }

void BlockSeqJump(RegisterOr<BlockSequence> bseq,
                  std::unordered_map<ast::BlockLiteral const *,
                                     ir::BlockIndex> const *jump_table) {
  auto &cmd           = MakeCmd(nullptr, Op::BlockSeqJump);
  cmd.block_seq_jump_ = Cmd::BlockSeqJump{bseq, jump_table};
}

template <typename T>
static std::ostream &operator<<(std::ostream &os,
                                std::array<RegisterOr<T>, 2> r) {
  return os << r[0] << " " << r[1];
}

char const *OpCodeStr(Op op) {
  switch (op) {
#define OP_MACRO(op, ...)                                                      \
  case Op::op:                                                                 \
    return #op;
#include "ir/op.xmacro.h"
#undef OP_MACRO
  }
  __builtin_unreachable();
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::Store<T> const &s) {
  return os << s.addr_.to_string() << " " << s.val_;
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::SetRet<T> const &s) {
  return os << s.ret_num_ << " " << s.val_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::PrintEnum const &p) {
  return os << p.arg_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::PrintFlags const &p) {
  return os << p.arg_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::BlockSeqJump const &b) {
  return os << b.bseq_;
}

static std::ostream &operator<<(std::ostream &os,
                                Cmd::BlockSeqContains const &b) {
  return os << b.lit_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::PtrIncr const &p) {
  return os << p.incr_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::Array const &a) {
  return os << a.type_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::Field const &f) {
  return os << f.ptr_ << " " << f.struct_type_->to_string() << " " << f.num_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::Call const &call) {
  if (call.fn_.is_reg_) {
    os << call.fn_.reg_;
  } else if (call.fn_.val_.is_fn_) {
    os << call.fn_.val_.fn_;
  } else {
    os << call.fn_.val_.foreign_.name_;
  }
  os << call.arguments_->to_string();
  if (call.outs_) {
    for (const auto &out : call.outs_->outs_) {
      if (out.is_loc_) { os << "*"; }
      os << out.reg_;
    }
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, Cmd const &cmd) {
  if (cmd.result.value >= 0) { os << cmd.result << " = "; }
  os << OpCodeStr(cmd.op_code_) << " ";
  switch (cmd.op_code_) {
#define OP_MACRO(op, tag, type, field)                                         \
  case Op::op:                                                                 \
    return os << cmd.field;
#include "ir/op.xmacro.h"
#undef OP_MACRO
  }
  UNREACHABLE();
}
}  // namespace ir
