#include "ir/cmd.h"

#include <cmath>
#include <iostream>

#include "architecture.h"
#include "base/container/vector.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "ir/val.h"
#include "type/all.h"

namespace ir {
using base::check::Is;
BlockIndex BasicBlock::Current;

std::string LongArgs::to_string() const {
  std::stringstream ss;
  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  size_t i      = 0;
  for (auto *t : type_->input) {
    offset = arch.MoveForwardToAlignment(t, offset);
    if (is_reg_[i]) {
      ss << " " << args_.get<Register>(offset).to_string();
      offset += sizeof(Register);
    } else {
      ss << " [??]";
      offset += arch.bytes(t);
    }
    ++i;
  }
  return ss.str();
}

void LongArgs::append(Register reg) {
  args_.append(reg);
  is_reg_.push_back(true);
}
void LongArgs::append(const ir::Val &val) {
  // TODO deal with alignment?
  std::visit(
      base::overloaded{
          [](const ir::Interface &) { UNREACHABLE(); },
          [&](auto &&v) {
            args_.append(v);
            is_reg_.push_back(
                std::is_same_v<ir::Register, std::decay_t<decltype(v)>>);
          }},
      val.value);
}

// TODO namespace appropriately
Cmd &MakeCmd(type::Type const *t, Op op) {
  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(BasicBlock::Current)
                  .cmds_.emplace_back(t, op);
  return cmd;
}

RegisterOr<float> CastIntToFloat32(RegisterOr<i32> r) {
  if (!r.is_reg_) { return static_cast<float>(r.val_); }
  auto &cmd                = ir::MakeCmd(type::Float32, Op::CastIntToFloat32);
  cmd.cast_int_to_float32_ = ir::Cmd::CastIntToFloat32{{}, r.reg_};
  return cmd.result;
}

RegisterOr<double> CastIntToFloat64(RegisterOr<i32> r) {
  if (!r.is_reg_) { return static_cast<double>(r.val_); }
  auto &cmd                = ir::MakeCmd(type::Float64, Op::CastIntToFloat64);
  cmd.cast_int_to_float64_ = ir::Cmd::CastIntToFloat64{{}, r.reg_};
  return cmd.result;
}

Register CastPtr(Register r, type::Pointer const *t) {
  auto &cmd     = ir::MakeCmd(t, Op::CastPtr);
  cmd.cast_ptr_ = ir::Cmd::CastPtr{{}, r, t->pointee};
  return cmd.result;
}

RegisterOr<char> Trunc(RegisterOr<i32> r) {
  if (!r.is_reg_) { return static_cast<char>(r.val_); }
  auto &cmd  = MakeCmd(type::Char, Op::Trunc);
  cmd.trunc_ = Cmd::Trunc::Make(r.reg_);
  return cmd.result;
}

RegisterOr<i32> Extend(RegisterOr<char> r) {
  if (!r.is_reg_) { return static_cast<i32>(r.val_); }
  auto &cmd   = MakeCmd(type::Int, Op::Extend);
  cmd.extend_ = Cmd::Extend::Make(r.reg_);
  return cmd.result;
}

RegisterOr<i32> Bytes(RegisterOr<type::Type const *> r) {
  auto &cmd  = MakeCmd(type::Int, Op::Bytes);
  cmd.bytes_ = Cmd::Bytes::Make(r);
  return cmd.result;
}

RegisterOr<i32> Align(RegisterOr<type::Type const *> r) {
  auto &cmd  = MakeCmd(type::Int, Op::Align);
  cmd.align_ = Cmd::Align::Make(r);
  return cmd.result;
}

RegisterOr<bool> Not(RegisterOr<bool> r) {
  if (!r.is_reg_) { return !r.val_; }
  auto &cmd = MakeCmd(type::Bool, Op::Not);
  cmd.not_  = Cmd::Not::Make(r.reg_);
  Func::Current->references_[cmd.not_.reg_].insert(cmd.result);
  return cmd.result;
}

// TODO do you really want to support this? How can array allocation be
// customized?
TypedRegister<Addr> Malloc(const type::Type *t, RegisterOr<i32> r) {
  auto &cmd   = MakeCmd(type::Ptr(t), Op::Malloc);
  cmd.malloc_ = Cmd::Malloc::Make(r);
  return cmd.result;
}

void Free(Register r) {
  auto &cmd = MakeCmd(nullptr, Op::Free);
  cmd.free_ = Cmd::Free::Make(r);
}

Register ArrayLength(Register r) {
  auto &cmd         = MakeCmd(type::Ptr(type::Int), Op::ArrayLength);
  cmd.array_length_ = Cmd::ArrayLength::Make(r);
  return cmd.result;
}

Register ArrayData(Register r, type::Type const *ptr) {
  auto *array_type = &ptr->as<type::Pointer>().pointee->as<type::Array>();
  ASSERT(!array_type->fixed_length);

  auto &cmd       = MakeCmd(type::Ptr(array_type->data_type), Op::ArrayData);
  cmd.array_data_ = Cmd::ArrayData::Make(r);
  return cmd.result;
}

RegisterOr<type::Type const *> Ptr(RegisterOr<type::Type const *> r) {
  if (!r.is_reg_) { return type::Ptr(r.val_); }
  auto &cmd = MakeCmd(type::Type_, Op::Ptr);
  cmd.ptr_  = Cmd::Ptr::Make(r.reg_);
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
  cmd.array_ = Cmd::Array::Make(-1, data_type);
  return cmd.result;
}

RegisterOr<type::Type const *> Array(RegisterOr<i32> len,
                                     RegisterOr<type::Type const *> data_type) {
  if (!data_type.is_reg_ && !len.is_reg_) {
    return type::Arr(data_type.val_, len.val_);
  }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = Cmd::Array::Make(len, data_type);
  return cmd.result;
}

Register CreateTuple() { return MakeCmd(type::Type_, Op::CreateTuple).result; }

void AppendToTuple(Register tup, RegisterOr<type::Type const *> entry) {
  auto &cmd            = MakeCmd(nullptr, Op::AppendToTuple);
  cmd.append_to_tuple_ = Cmd::AppendToTuple::Make(tup, entry);
}

Register FinalizeTuple(Register r) {
  auto &cmd           = MakeCmd(type::Type_, Op::FinalizeTuple);
  cmd.finalize_tuple_ = Cmd::FinalizeTuple::Make(r);
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
  auto &cmd              = MakeCmd(nullptr, Op::AppendToVariant);
  cmd.append_to_variant_ = Cmd::AppendToVariant::Make(var, entry);
}

Register FinalizeVariant(Register r) {
  auto &cmd             = MakeCmd(type::Type_, Op::FinalizeVariant);
  cmd.finalize_variant_ = Cmd::FinalizeVariant::Make(r);
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
  cmd.field_ = Cmd::Field::Make(r, t, n);
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
  cmd.append_to_block_seq_ =
      Cmd::AppendToBlockSeq::Make(block_seq, more_block_seq);
}

Register FinalizeBlockSeq(Register r) {
  auto &cmd               = MakeCmd(type::Block, Op::FinalizeBlockSeq);
  cmd.finalize_block_seq_ = Cmd::FinalizeBlockSeq::Make(r);
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
    cmd.block_seq_contains_ = Cmd::BlockSeqContains{{}, r.reg_, lit};
    return cmd.result;
  }

  return std::any_of(r.val_.seq_->begin(), r.val_.seq_->end(),
                     [lit](ast::BlockLiteral *l) { return lit == l; });
}

Register CreateStruct(ast::StructLiteral *lit) {
  auto &cmd          = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.create_struct_ = Cmd::CreateStruct::Make(lit);
  return cmd.result;
}

Register FinalizeStruct(Register r) {
  auto &cmd            = MakeCmd(type::Type_, Op::FinalizeStruct);
  cmd.finalize_struct_ = Cmd::FinalizeStruct::Make(r);
  return cmd.result;
}

void DebugIr() { MakeCmd(nullptr, Op::DebugIr); }

Register VariantType(Register r) {
  auto &cmd         = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.variant_type_ = Cmd::VariantType::Make(r);
  return cmd.result;
}

Register VariantValue(type::Type const *t, Register r) {
  auto &cmd         = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.variant_type_ = Cmd::VariantType::Make(r);
  return cmd.result;
}

void CreateStructField(type::Struct *struct_type,
                       RegisterOr<type::Type const *> type) {
  auto &cmd = MakeCmd(nullptr, Op::CreateStructField);
  cmd.create_struct_field_ =
      Cmd::CreateStructField::Make(struct_type, std::move(type));
}

void SetStructFieldName(type::Struct *struct_type,
                        std::string_view field_name) {
  auto &cmd = MakeCmd(nullptr, Op::SetStructFieldName);
  cmd.set_struct_field_name_ =
      Cmd::SetStructFieldName::Make(struct_type, field_name);
}

TypedRegister<Addr> Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));

  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(Func::Current->entry())
                  .cmds_.emplace_back(type::Ptr(t), Op::Alloca);

  cmd.alloca_ = Cmd::Alloca::Make(t);
  return cmd.result;
}

void SetRet(size_t n, Val const &v) {
  ASSERT(v.type != nullptr);
  if (v.type == type::Bool) { return SetRet(n, v.reg_or<bool>()); }
  if (v.type == type::Char) { return SetRet(n, v.reg_or<char>()); }
  if (v.type == type::Int) { return SetRet(n, v.reg_or<i32>()); }
  if (v.type == type::Float32) { return SetRet(n, v.reg_or<float>()); }
  if (v.type == type::Float64) { return SetRet(n, v.reg_or<double>()); }
  if (v.type == type::Type_) {
    return SetRet(n, v.reg_or<type::Type const *>());
  }
  if (v.type->is<type::Enum>()) { return SetRet(n, v.reg_or<EnumVal>()); }
  if (v.type->is<type::Flags>()) { return SetRet(n, v.reg_or<FlagsVal>()); }
  if (v.type->is<type::CharBuffer>()) {
    return SetRet(n, v.reg_or<std::string_view>());
  }
  if (v.type->is<type::Pointer>()) { return SetRet(n, v.reg_or<ir::Addr>()); }
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
  UNREACHABLE(v.type->to_string());
}

TypedRegister<Addr> PtrIncr(Register ptr, RegisterOr<i32> inc,
                            type::Type const *t) {
  // TODO type must be a pointer.
  if (!inc.is_reg_ && inc.val_ == 0) { return ptr; }
  auto &cmd     = MakeCmd(t, Op::PtrIncr);
  cmd.ptr_incr_ = Cmd::PtrIncr::Make(ptr, t->as<type::Pointer>().pointee, inc);
  return cmd.result;
}

RegisterOr<FlagsVal> XorFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ ^ rhs.val_; }
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
  if (t == type::Bool) { return Load<bool>(r); }
  if (t == type::Char) { return Load<char>(r); }
  if (t == type::Int) { return Load<i32>(r); }
  if (t == type::Float32) { return Load<float>(r); }
  if (t == type::Float64) { return Load<double>(r); }
  if (t == type::Type_) { return Load<type::Type const*>(r); }
  if (t->is<type::Enum>()) { return Load<EnumVal>(r, t); }
  if (t->is<type::Flags>()) { return Load<FlagsVal>(r, t); }
  if (t->is<type::Pointer>()) { return Load<Addr>(r, t); }
  if (t->is<type::Function>()) { return Load<AnyFunc>(r, t); }
  UNREACHABLE(t);
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

void Call(RegisterOr<AnyFunc> const &f, LongArgs long_args) {
  auto &block    = Func::Current->block(BasicBlock::Current);
  LongArgs *args = &block.long_args_.emplace_back(std::move(long_args));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, nullptr);
}

void Call(RegisterOr<AnyFunc> const &f, LongArgs long_args, OutParams outs) {
  auto &block    = Func::Current->block(BasicBlock::Current);
  auto *args     = &block.long_args_.emplace_back(std::move(long_args));
  auto *outs_ptr = &block.outs_.emplace_back(std::move(outs));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  cmd.call_ = Cmd::Call(f, args, outs_ptr);
}

void CondJump(RegisterOr<bool> cond, BlockIndex true_block,
              BlockIndex false_block) {
  if (!cond.is_reg_) {
    return UncondJump(cond.val_ ? true_block : false_block);
  }
  auto &cmd      = MakeCmd(nullptr, Op::CondJump);
  cmd.cond_jump_ = Cmd::CondJump{{}, cond.reg_, {false_block, true_block}};
}

void UncondJump(BlockIndex block) {
  auto &cmd        = MakeCmd(nullptr, Op::UncondJump);
  cmd.uncond_jump_ = Cmd::UncondJump{{}, block};
}

void ReturnJump() {
  auto &cmd        = MakeCmd(nullptr, Op::ReturnJump);
  cmd.return_jump_ = Cmd::ReturnJump{};
}

void BlockSeqJump(RegisterOr<BlockSequence> bseq,
                  std::unordered_map<ast::BlockLiteral const *,
                                     ir::BlockIndex> const *jump_table) {
  auto &cmd           = MakeCmd(nullptr, Op::BlockSeqJump);
  cmd.block_seq_jump_ = Cmd::BlockSeqJump{{}, bseq, jump_table};
}

static std::ostream &operator<<(std::ostream &os, Register r) {
  return os << "reg." << r.value;
}

static std::ostream &operator<<(std::ostream &os, Addr addr) {
  return os << addr.to_string();
}

static std::ostream &operator<<(std::ostream &os, FlagsVal f) {
  return os << f.value;
}

static std::ostream &operator<<(std::ostream &os, EnumVal e) {
  return os << e.value;
}

static std::ostream &operator<<(std::ostream &os, BlockIndex b) {
  return os << "block." << b.value;
}

template <typename T>
static std::ostream &operator<<(std::ostream &os, RegisterOr<T> r) {
  if (r.is_reg_) {
    return os << r.reg_;
  } else {
    return os << r.val_;
  }
}

template <typename T>
static std::ostream &operator<<(std::ostream &os,
                                std::array<RegisterOr<T>, 2> r) {
  return os << r[0] << " " << r[1];
}

char const *OpCodeStr(Op op) {
  switch (op) {
#define OP_MACRO(op, ...)                                                      \
  case Op::op: return #op;
  //
#define OP_MACRO_(op, ...)                                                     \
  case Op::op: return #op;
  //
#include "ir/op.xmacro.h"
#undef OP_MACRO_
#undef OP_MACRO
  }
  __builtin_unreachable();
}

std::ostream &operator<<(std::ostream &os, Cmd const &cmd) {
  if (cmd.result.value >= 0) { os << cmd.result << " = "; }
  os << OpCodeStr(cmd.op_code_) << " ";
  switch (cmd.op_code_) {
    case Op::Trunc: return os << cmd.trunc_.reg_;
    case Op::Extend: return os << cmd.extend_.reg_;
    case Op::Bytes: return os << cmd.bytes_.arg_;
    case Op::Align: return os << cmd.align_.arg_;
    case Op::Not: return os << cmd.not_.reg_;
    case Op::NegInt: return os << cmd.neg_int_.reg_;
    case Op::NegFloat32: return os << cmd.neg_float32_.reg_;
    case Op::NegFloat64: return os << cmd.neg_float64_.reg_;
    case Op::ArrayLength: return os << cmd.array_length_.arg_;
    case Op::ArrayData: return os << cmd.array_data_.arg_;
    case Op::Ptr: return os << cmd.ptr_.reg_;
    case Op::LoadBool: return os << cmd.load_bool_.arg_;
    case Op::LoadChar: return os << cmd.load_char_.arg_;
    case Op::LoadInt: return os << cmd.load_int_.arg_;
    case Op::LoadFloat32: return os << cmd.load_float32_.arg_;
    case Op::LoadFloat64: return os << cmd.load_float64_.arg_;
    case Op::LoadType: return os << cmd.load_type_.arg_;
    case Op::LoadEnum: return os << cmd.load_enum_.arg_;
    case Op::LoadFlags: return os << cmd.load_type_.arg_;
    case Op::LoadAddr: return os << cmd.load_addr_.arg_;
    case Op::LoadFunc: return os << cmd.load_func_.arg_;
    case Op::PrintBool: return os << cmd.print_bool_.arg_;
    case Op::PrintChar: return os << cmd.print_char_.arg_;
    case Op::PrintInt: return os << cmd.print_int_.arg_;
    case Op::PrintFloat32: return os << cmd.print_float32_.arg_;
    case Op::PrintFloat64: return os << cmd.print_float64_.arg_;
    case Op::PrintType: return os << cmd.print_type_.arg_;
    case Op::PrintEnum: return os << cmd.print_enum_.arg_;
    case Op::PrintFlags: return os << cmd.print_flags_.arg_;
    case Op::PrintAddr: return os << cmd.print_addr_.arg_;
    case Op::PrintCharBuffer: return os << cmd.print_char_buffer_.arg_;
    case Op::AddInt: return os << cmd.i32_args_.args_;
    case Op::AddFloat32: return os << cmd.float32_args_.args_;
    case Op::AddFloat64: return os << cmd.float64_args_.args_;
    case Op::SubInt: return os << cmd.i32_args_.args_;
    case Op::SubFloat32: return os << cmd.float32_args_.args_;
    case Op::SubFloat64: return os << cmd.float64_args_.args_;
    case Op::MulInt: return os << cmd.i32_args_.args_;
    case Op::MulFloat32: return os << cmd.float32_args_.args_;
    case Op::MulFloat64: return os << cmd.float64_args_.args_;
    case Op::DivInt: return os << cmd.i32_args_.args_;
    case Op::DivFloat32: return os << cmd.float32_args_.args_;
    case Op::DivFloat64: return os << cmd.float64_args_.args_;
    case Op::ModInt: return os << cmd.i32_args_.args_;
    case Op::LtInt: return os << cmd.i32_args_.args_;
    case Op::LtFloat32: return os << cmd.float32_args_.args_;
    case Op::LtFloat64: return os << cmd.float64_args_.args_;
    case Op::LtFlags: return os << cmd.flags_args_.args_;
    case Op::LeInt: return os << cmd.i32_args_.args_;
    case Op::LeFloat32: return os << cmd.float32_args_.args_;
    case Op::LeFloat64: return os << cmd.float64_args_.args_;
    case Op::LeFlags: return os << cmd.flags_args_.args_;
    case Op::GtInt: return os << cmd.i32_args_.args_;
    case Op::GtFloat32: return os << cmd.float32_args_.args_;
    case Op::GtFloat64: return os << cmd.float64_args_.args_;
    case Op::GtFlags: return os << cmd.flags_args_.args_;
    case Op::GeInt: return os << cmd.i32_args_.args_;
    case Op::GeFloat32: return os << cmd.float32_args_.args_;
    case Op::GeFloat64: return os << cmd.float64_args_.args_;
    case Op::GeFlags: return os << cmd.flags_args_.args_;
    case Op::EqBool:
      return os << cmd.eq_bool_.args_[0] << " " << cmd.eq_bool_.args_[1];
    case Op::EqChar: return os << cmd.char_args_.args_;
    case Op::EqInt: return os << cmd.i32_args_.args_;
    case Op::EqFloat32: return os << cmd.float32_args_.args_;
    case Op::EqFloat64: return os << cmd.float64_args_.args_;
    case Op::EqEnum: return os << cmd.enum_args_.args_;
    case Op::EqFlags: return os << cmd.flags_args_.args_;
    case Op::EqType: return os << cmd.type_args_.args_;
    case Op::EqAddr: return os << cmd.addr_args_.args_;
    case Op::NeChar: return os << cmd.char_args_.args_;
    case Op::NeInt: return os << cmd.i32_args_.args_;
    case Op::NeFloat32: return os << cmd.float32_args_.args_;
    case Op::NeFloat64: return os << cmd.float64_args_.args_;
    case Op::NeEnum: return os << cmd.enum_args_.args_;
    case Op::NeFlags: return os << cmd.flags_args_.args_;
    case Op::NeType: return os << cmd.type_args_.args_;
    case Op::NeAddr: return os << cmd.addr_args_.args_;
    case Op::XorBool: return os << cmd.bool_args_.args_;
    case Op::XorFlags: return os << cmd.flags_args_.args_;
    case Op::OrFlags: return os << cmd.flags_args_.args_;
    case Op::AndFlags: return os << cmd.flags_args_.args_;
    case Op::CreateStruct: return os << cmd.create_struct_.lit_;
    case Op::CreateStructField:
      return os << cmd.create_struct_field_.struct_ << " "
                << cmd.create_struct_field_.type_;
    case Op::SetStructFieldName:
      return os << cmd.set_struct_field_name_.struct_ << " "
                << cmd.set_struct_field_name_.name_;
    case Op::FinalizeStruct: return os << cmd.finalize_struct_.reg_;

    case Op::Malloc: return os << cmd.malloc_.arg_;
    case Op::Free: return os << cmd.free_.reg_;
    case Op::Alloca: return os << cmd.alloca_.type_->to_string();
    case Op::Arrow: return os << cmd.type_args_.args_;
    case Op::Array: return os << cmd.array_.type_;
    case Op::CreateTuple: return os;
    case Op::AppendToTuple:
      return os << cmd.append_to_tuple_.tup_ << " "
                << cmd.append_to_tuple_.arg_;
    case Op::FinalizeTuple: return os << cmd.finalize_tuple_.tup_;
    case Op::CreateVariant: return os;
    case Op::AppendToVariant:
      return os << cmd.append_to_variant_.var_ << " "
                << cmd.append_to_variant_.arg_;
    case Op::FinalizeVariant: return os << cmd.finalize_variant_.var_;
    case Op::CreateBlockSeq: return os;
    case Op::AppendToBlockSeq:
      return os << cmd.append_to_block_seq_.block_seq_ << " "
                << cmd.append_to_block_seq_.arg_;
    case Op::FinalizeBlockSeq: return os << cmd.finalize_block_seq_.block_seq_;
    case Op::VariantType: return os << cmd.variant_type_.reg_;
    case Op::VariantValue: return os << cmd.variant_value_.reg_;
    case Op::PtrIncr: return os << cmd.ptr_incr_.incr_;
    case Op::Field:
      return os << cmd.field_.ptr_ << " "
                << cmd.field_.struct_type_->to_string() << " "
                << cmd.field_.num_;
    case Op::CondJump:
      return os << cmd.cond_jump_.cond_ << " " << cmd.cond_jump_.blocks_[0]
                << " " << cmd.cond_jump_.blocks_[1];
    case Op::UncondJump: return os << cmd.uncond_jump_.block_;
    case Op::ReturnJump: return os;
    case Op::BlockSeqJump: return os << cmd.block_seq_jump_.bseq_;
    case Op::Call:
      if (cmd.call_.fn_.is_reg_) {
        os << cmd.call_.fn_.reg_;
      } else if (cmd.call_.fn_.val_.is_fn_) {
        os << cmd.call_.fn_.val_.fn_;
      } else {
        os << cmd.call_.fn_.val_.foreign_.name_;
      }
      os << cmd.call_.long_args_->to_string();
      if (cmd.call_.outs_) {
        for (const auto &out : cmd.call_.outs_->outs_) {
          if (out.is_loc_) { os << "*"; }
          os << out.reg_;
        }
      }

      return os;
    case Op::BlockSeqContains: return os << cmd.block_seq_contains_.lit_;

    case Op::CastIntToFloat32: return os << cmd.cast_int_to_float32_.reg_;
    case Op::CastIntToFloat64: return os << cmd.cast_int_to_float64_.reg_;

    case Op::CastPtr: return os << cmd.cast_ptr_.type_;

    case Op::StoreBool:
      return os << cmd.store_bool_.addr_ << " " << cmd.store_bool_.val_;
    case Op::StoreChar:
      return os << cmd.store_char_.addr_ << " " << cmd.store_char_.val_;
    case Op::StoreInt:
      return os << cmd.store_int_.addr_ << " " << cmd.store_int_.val_;
    case Op::StoreFloat32:
      return os << cmd.store_float32_.addr_ << " " << cmd.store_float32_.val_;
    case Op::StoreFloat64:
      return os << cmd.store_float64_.addr_ << " " << cmd.store_float64_.val_;
    case Op::StoreType:
      if (cmd.store_type_.val_.is_reg_) {
        return os << cmd.store_type_.addr_ << " " << cmd.store_type_.val_.reg_;
      } else {
        return os << cmd.store_type_.addr_ << " "
                  << cmd.store_type_.val_.val_->to_string();
      }
    case Op::StoreEnum:
      return os << cmd.store_enum_.addr_ << " " << cmd.store_enum_.val_;
    case Op::StoreFunc:
      return os << cmd.store_func_.addr_ << " " << cmd.store_func_.val_;
    case Op::StoreFlags:
      return os << cmd.store_flags_.addr_ << " " << cmd.store_flags_.val_;
    case Op::StoreAddr:
      return os << cmd.store_addr_.addr_ << " " << cmd.store_addr_.val_;
    case Op::SetRetBool: return os << cmd.set_ret_bool_.val_;
    case Op::SetRetChar: return os << cmd.set_ret_char_.val_;
    case Op::SetRetInt: return os << cmd.set_ret_int_.val_;
    case Op::SetRetFloat32: return os << cmd.set_ret_float32_.val_;
    case Op::SetRetFloat64: return os << cmd.set_ret_float64_.val_;
    case Op::SetRetType: return os << cmd.set_ret_type_.val_;
    case Op::SetRetEnum: return os << cmd.set_ret_enum_.val_;
    case Op::SetRetFlags: return os << cmd.set_ret_flags_.val_;
    case Op::SetRetCharBuf: return os << cmd.set_ret_char_buf_.val_;
    case Op::SetRetAddr: return os << cmd.set_ret_addr_.val_;
    case Op::SetRetFunc: return os << cmd.set_ret_func_.val_;
    case Op::SetRetScope: return os << cmd.set_ret_scope_.val_;
    case Op::SetRetGeneric: return os << cmd.set_ret_scope_.val_;
    case Op::SetRetModule: return os << cmd.set_ret_module_.val_;
    case Op::SetRetBlock: return os << cmd.set_ret_block_.val_;
    case Op::PhiBool: return os << cmd.phi_bool_.args_;
    case Op::PhiChar: return os << cmd.phi_char_.args_;
    case Op::PhiInt: return os << cmd.phi_int_.args_;
    case Op::PhiFloat32: return os << cmd.phi_float32_.args_;
    case Op::PhiFloat64: return os << cmd.phi_float64_.args_;
    case Op::PhiType: return os << cmd.phi_type_.args_;
    case Op::PhiBlock: return os << cmd.phi_block_.args_;
    case Op::PhiAddr: return os << cmd.phi_addr_.args_;
    case Op::Death: return os;
    case Op::DebugIr: return os;
  }
  UNREACHABLE();
}
}  // namespace ir
