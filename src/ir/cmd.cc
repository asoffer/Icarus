#include "ir/cmd.h"

#include <cmath>
#include <iostream>

#include "architecture.h"
#include "ast/struct_literal.h"
#include "base/container/vector.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "ir/val.h"
#include "type/all.h"
#include "type/typed_value.h"

namespace ir {
using base::check::Is;
thread_local BlockIndex BasicBlock::Current;

Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(BasicBlock::Current)
                  .cmds_.emplace_back(t, op);
  return cmd;
}

type::Typed<Register> LoadSymbol(std::string_view name, type::Type const *t) {
  auto &cmd     = MakeCmd(t, Op::LoadSymbol);
  cmd.load_sym_ = {name, t};
  return type::Typed<Register>{cmd.result, t};
}

// TODO pass atyped_reg? not sure that's right.
Register CastPtr(Register r, type::Pointer const *t) {
  auto &cmd      = MakeCmd(t, Op::CastPtr);
  cmd.typed_reg_ = type::Typed<Register>(r, t);
  return cmd.result;
}

RegisterOr<i32> Bytes(RegisterOr<type::Type const *> r) {
  auto &cmd     = MakeCmd(type::Int32, Op::Bytes);
  cmd.type_arg_ = r;
  return cmd.result;
}

RegisterOr<i32> Align(RegisterOr<type::Type const *> r) {
  auto &cmd     = MakeCmd(type::Int32, Op::Align);
  cmd.type_arg_ = r;
  return cmd.result;
}

template <typename T>
static Val CastTo(type::Type const *from, Val const &val) {
  if (auto *r = std::get_if<Register>(&val.value)) {
    auto *to       = type::Get<T>();
    auto &cmd      = MakeCmd(to, Cmd::OpCode<Cmd::CastTag, T>());
    cmd.typed_reg_ = type::Typed<Register>(*r, from);
    return Val::Reg(cmd.result, to);
  } else {
    return type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float>(
        from, [&](auto type_holder) {
          using FromType = typename decltype(type_holder)::type;
          return Val(static_cast<T>(std::get<FromType>(val.value)));
        });
  }
  UNREACHABLE("To ", type::Get<T>(), " from ", from);
}

Val Cast(type::Type const *from, type::Type const *to, Val const &val) {
  if (from == type::NullPtr) {
    Val copy  = val;
    copy.type = to;
    return copy;
  }

  // Note: ir::Cast is called with types associated IR commands and registers.
  // Because arrays are considered big, they would never be stored directly in
  // a register, so we would not see an empty array but rather a pointer to an
  // empty arry.
  if (from == type::Ptr(type::EmptyArray)) {
    Val copy = val;
    copy.type = type::Ptr(to);
    return copy;
  }

  if (to->is<type::Enum>()) {
    ASSERT(from == type::Int32);
    auto x = val.reg_or<i32>();
    if (x.is_reg_) {
      auto &cmd = MakeCmd(to, Op::CastToEnum);
      cmd.reg_  = x.reg_;
      return Val::Reg(cmd.result, to);
    } else {
      return ValFrom(EnumVal(x.val_), &to->as<type::Enum>());
    }
  } else if (to->is<type::Flags>()) {
    ASSERT(from == type::Int32);
    auto x = val.reg_or<i32>();
    if (x.is_reg_) {
      auto &cmd = MakeCmd(to, Op::CastToFlags);
      cmd.reg_  = x.reg_;
      return Val::Reg(cmd.result, to);
    } else {
      return ValFrom(FlagsVal(x.val_), &to->as<type::Flags>());
    }
  }

  // TODO We only need to include i8 and u8 here for supporting loose casting.
  // If that disappears, we can remove those types.
  return type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
      to, [&](auto type_holder) {
        using ToType = typename decltype(type_holder)::type;
        return CastTo<ToType>(from, val);
      });
}

RegisterOr<bool> Not(RegisterOr<bool> r) {
  if (!r.is_reg_) { return !r.val_; }
  auto &cmd = MakeCmd(type::Bool, Op::NotBool);
  cmd.reg_  = r.reg_;
  Func::Current->references_[cmd.reg_].insert(cmd.result);
  return cmd.result;
}

RegisterOr<FlagsVal> Not(type::Typed<RegisterOr<FlagsVal>, type::Flags> r) {
  if (!r->is_reg_) { return NotFlags(r->val_, r.type()); }
  auto &cmd      = MakeCmd(r.type(), Op::NotFlags);
  cmd.typed_reg_ = type::Typed<Register>(r->reg_, r.type());
  return cmd.result;
}

RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::Ptr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::Ptr);
  cmd.reg_  = r.reg_;
  return cmd.result;
}

RegisterOr<type::Type const *> BufPtr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::BufPtr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::BufPtr);
  cmd.reg_  = r.reg_;
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
  cmd.set<Cmd::ArrowTag, type::Type const *>(v1, v2);
  auto &refs = Func::Current->references_;
  if (v1.is_reg_) { refs[v1.reg_].insert(cmd.result); }
  if (v2.is_reg_) { refs[v2.reg_].insert(cmd.result); }
  return cmd.result;
}

RegisterOr<type::Type const *> Array(RegisterOr<i64> len,
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
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeTuple);
  cmd.reg_  = r;
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

Register Field(RegisterOr<Addr> r, type::Tuple const *t, size_t n) {
  auto &cmd  = MakeCmd(type::Ptr(t->entries_.at(n)), Op::Field);
  cmd.field_ = {r, t, n};
  return cmd.result;
}

Register Field(RegisterOr<Addr> r, type::Struct const *t, size_t n) {
  auto &cmd  = MakeCmd(type::Ptr(t->fields().at(n).type), Op::Field);
  cmd.field_ = {r, t, n};
  return cmd.result;
}

Register Reserve(type::Type const *t, bool incr_num_regs = true) {
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

Register CreateStruct(::Scope const *scope) {
  auto &cmd  = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.scope_ = scope;
  return cmd.result;
}

Register CreateInterface(::Scope const *scope) {
  auto &cmd  = MakeCmd(type::Type_, Op::CreateInterface);
  cmd.scope_ = scope;
  return cmd.result;
}

Register ArgumentCache(ast::StructLiteral *sl) {
  auto &cmd = MakeCmd(type::Ptr(type::Type_), Op::ArgumentCache);
  cmd.sl_ = sl;
  return cmd.result;
}

TypedRegister<type::Interface const *> FinalizeStruct(Register r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeStruct);
  cmd.reg_  = r;
  return cmd.result;
}

Register FinalizeInterface(Register r) {
  auto &cmd = MakeCmd(type::Type_, Op::FinalizeInterface);
  cmd.reg_  = r;
  return cmd.result;
}

void DebugIr() { MakeCmd(nullptr, Op::DebugIr); }

Register VariantType(RegisterOr<Addr> r) {
  auto &cmd = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.addr_arg_ = r;
  return cmd.result;
}

Register VariantValue(type::Type const *t, RegisterOr<Addr> r) {
  auto &cmd = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.addr_arg_ = r;
  return cmd.result;
}

void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type) {
  auto &cmd                = MakeCmd(nullptr, Op::CreateStructField);
  cmd.create_struct_field_ = {struct_type, std::move(type)};
}

void SetStructFieldName(Register struct_type, std::string_view field_name) {
  auto &cmd                  = MakeCmd(nullptr, Op::SetStructFieldName);
  cmd.set_struct_field_name_ = {struct_type, field_name};
}

void AddHashtagToField(Register struct_type, ast::Hashtag hashtag) {
  auto &cmd        = MakeCmd(nullptr, Op::AddHashtagToField);
  cmd.add_hashtag_ = {struct_type, hashtag};
}

void AddHashtagToStruct(Register struct_type, ast::Hashtag hashtag) {
  auto &cmd        = MakeCmd(nullptr, Op::AddHashtagToStruct);
  cmd.add_hashtag_ = {struct_type, hashtag};
}

TypedRegister<Addr> Alloca(type::Type const *t) {
  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(Func::Current->entry())
                  .cmds_.emplace_back(type::Ptr(t), Op::Alloca);
  cmd.type_ = t;
  return cmd.result;
}

TypedRegister<Addr> TmpAlloca(type::Type const *t, Context *ctx) {
  auto reg = Alloca(t);
  ctx->temporaries_to_destroy_->emplace_back(reg, t);
  return reg;
}

TypedRegister<Addr> GetRet(size_t n, type::Type const *t) {
  ASSERT(t->is_big());
  auto &cmd    = MakeCmd(type::Ptr(t), Op::GetRet);
  cmd.get_ret_ = n;
  return cmd.result;
}

void SetRet(size_t n, Val const &v, Context *ctx) {
  if (v.type->is<type::GenericStruct>()) {
    return SetRet(n, v.reg_or<AnyFunc>());
  }
  return type::Apply(ASSERT_NOT_NULL(v.type), [&](auto type_holder) {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, type::Struct const *>) {
      auto *t = ir::Func::Current->type_->output[n];
      // TODO guaranteed copy-elision
      t->EmitCopyAssign(t, v, GetRet(n, t), ctx);
    } else {
      SetRet(n, v.reg_or<T>());
    }
  });
}

TypedRegister<Addr> PtrIncr(RegisterOr<Addr> ptr, RegisterOr<i32> inc,
                            type::Pointer const *t) {
  auto &cmd     = MakeCmd(t, Op::PtrIncr);
  cmd.ptr_incr_ = {ptr, t->pointee, inc};
  return cmd.result;
}

RegisterOr<FlagsVal> XorFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ ^ rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return rhs; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) {
      return Not(type::Typed<RegisterOr<FlagsVal>, type::Flags>(rhs, type));
    }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return rhs; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) {
      return Not(type::Typed<RegisterOr<FlagsVal>, type::Flags>(lhs, type));
    }
  }

  auto &cmd = MakeCmd(type, Op::XorFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> OrFlags(type::Flags const *type,
                             RegisterOr<FlagsVal> const &lhs,
                             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ | rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return rhs; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) {
      return FlagsVal{(1ull << type->members_.size()) - 1};
    }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return lhs; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) {
      return FlagsVal{(1ull << type->members_.size()) - 1};
    }
  }

  auto &cmd = MakeCmd(type, Op::OrFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> AndFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ & rhs.val_; }
  if (!lhs.is_reg_) {
    if (lhs.val_.value == 0) { return FlagsVal{0}; }
    if (lhs.val_.value == (1ull << type->members_.size()) - 1) { return rhs; }
  }

  if (!rhs.is_reg_) {
    if (rhs.val_.value == 0) { return FlagsVal{0}; }
    if (rhs.val_.value == (1ull << type->members_.size()) - 1) { return lhs; }
  }

  auto &cmd = MakeCmd(type, Op::AndFlags);
  cmd.set<Cmd::XorTag, FlagsVal>(lhs, rhs);
  return cmd.result;
}

Register Load(RegisterOr<Addr> r, type::Type const *t) {
  if (t->is<type::Function>()) { return Load<AnyFunc>(r, t); }
  return type::Apply(t, [&](auto type_holder) -> Register {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, ir::Addr> ||
                  std::is_same_v<T, ir::EnumVal> ||
                  std::is_same_v<T, ir::FlagsVal>) {
      return Load<T>(r, t);
    } else {
      return Load<T>(r);
    }
  });
}

TypedRegister<Addr> Index(type::Pointer const *t, Register array_ptr,
                          RegisterOr<i32> offset) {
  auto *array_type = &t->pointee->as<type::Array>();
  // TODO this works but generates worse ir (both here and in llvm). It's worth
  // figuring out how to do this better. Is this still true without
  // variable-length arrays?
  return PtrIncr(array_ptr, offset, type::Ptr(array_type->data_type));
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

TypedRegister<type::Type const *> NewOpaqueType(::Module const *mod) {
  auto &cmd = MakeCmd(type::Type_, Op::NewOpaqueType);
  cmd.mod_  = mod;
  return cmd.result;
}

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
static auto Stringify(T&& val) {
  if constexpr (std::is_same_v<std::decay_t<T>, type::Type const *>) {
    return val->to_string();
  } else if constexpr (std::is_same_v<std::decay_t<T>,
                                      RegisterOr<type::Type const *>>) {
    std::stringstream ss;
    if (val.is_reg_) {
      ss << val.reg_;
    } else {
      if (val.val_ == nullptr) {
        ss << "0x0";
      } else {
        ss << val.val_->to_string();
      }
    }
    return ss.str();
  } else {
    return val;
  }
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::Store<T> const &s) {
  return os << s.addr_ << " <- " << Stringify(s.val_);
}

template <typename T>
std::ostream &operator<<(std::ostream &os, Cmd::Args<T> const &a) {
  return os << Stringify(a.args_[0]) << " " << Stringify(a.args_[1]);
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, Cmd::SetRet<T> const &s) {
  return os << s.ret_num_ << " " << Stringify(s.val_);
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
  return os << f.ptr_ << " " << f.type_->to_string() << " " << f.num_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::CondJump const &j) {
  return os << j.cond_ << " false -> " << j.blocks_[0] << " true -> "
            << j.blocks_[1];
}

static std::ostream &operator<<(std::ostream &os, Cmd::LoadSymbol const &ls) {
  return os << ls.name_ << ": " << ASSERT_NOT_NULL(ls.type_)->to_string();
}

static std::ostream &operator<<(std::ostream &os, Cmd::Call const &call) {
  if (call.fn_.is_reg_) {
    os << call.fn_.reg_;
  } else if (call.fn_.val_.is_fn()) {
    os << call.fn_.val_.func();
  } else {
    os << "foreign("
       << reinterpret_cast<uintptr_t>(call.fn_.val_.foreign().get()) << ")";
  }
  os << call.arguments_->to_string();

  if (call.outs_) {
    for (size_t i = 0; i < call.outs_->size(); ++i) {
      if (call.outs_->is_loc_[i]) { os << "*"; }
      os << call.outs_->regs_[i];
    }
  }

  return os;
}

static std::ostream &operator<<(std::ostream &os, Cmd::AddEnumerator const &s) {
  return os << s.enum_ << " " << s.name_;
}

static std::ostream &operator<<(std::ostream &os, Cmd::SetEnumerator const &s) {
  return os << s.enum_ << " " << s.val_;
}

std::ostream &operator<<(std::ostream &os, Cmd const &cmd) {
  if (cmd.result.value >= 0) { os << cmd.result << " = "; }
  os << OpCodeStr(cmd.op_code_) << " ";
  switch (cmd.op_code_) {
#define OP_MACRO(op, tag, type, field)                                         \
  case Op::op:                                                                 \
    return os << Stringify(cmd.field);
#include "ir/op.xmacro.h"
#undef OP_MACRO
  }
  UNREACHABLE();
}
}  // namespace ir
