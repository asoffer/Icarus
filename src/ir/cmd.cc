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

RegisterOr<double> CastIntToReal(RegisterOr<i32> r) {
  if (!r.is_reg_) { return static_cast<double>(r.val_); }
  auto &cmd             = ir::MakeCmd(type::Real, Op::CastIntToReal);
  cmd.cast_int_to_real_ = ir::Cmd::CastIntToReal{{}, r.reg_};
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

Register Bytes(RegisterOr<type::Type const *> r) {
  auto &cmd  = MakeCmd(type::Int, Op::Bytes);
  cmd.bytes_ = Cmd::Bytes::Make(r);
  return cmd.result;
}

Register Align(RegisterOr<type::Type const *> r) {
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
Register Malloc(const type::Type *t, RegisterOr<i32> r) {
  auto &cmd   = MakeCmd(type::Ptr(t), Op::Malloc);
  cmd.malloc_ = Cmd::Malloc::Make(r);
  return cmd.result;
}

void Free(Register r) {
  auto &cmd = MakeCmd(nullptr, Op::Free);
  cmd.free_ = Cmd::Free::Make(r);
}

RegisterOr<i32> NegInt(RegisterOr<i32> r) {
  if (!r.is_reg_) { return -r.val_; }
  auto &cmd    = MakeCmd(type::Bool, Op::NegInt);
  cmd.neg_int_ = Cmd::NegInt::Make(r.reg_);
  Func::Current->references_[cmd.neg_int_.reg_].insert(cmd.result);
  return cmd.result;
}

RegisterOr<double> NegReal(RegisterOr<double> r) {
  if (!r.is_reg_) { return -r.val_; }
  auto &cmd     = MakeCmd(type::Bool, Op::NegReal);
  cmd.neg_real_ = Cmd::NegReal::Make(r.reg_);
  Func::Current->references_[cmd.neg_real_.reg_].insert(cmd.result);
  return cmd.result;
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

#define DEFINE_CMD1(Name, name, t)                                             \
  Register Name(Register r) {                                                  \
    auto &cmd = MakeCmd(t, Op::Name);                                          \
    cmd.name  = Cmd::Name::Make(r);                                            \
    return cmd.result;                                                         \
  }                                                                            \
  struct AllowSemicolon
DEFINE_CMD1(LoadBool, load_bool_, type::Bool);
DEFINE_CMD1(LoadChar, load_char_, type::Char);
DEFINE_CMD1(LoadInt, load_int_, type::Int);
DEFINE_CMD1(LoadReal, load_real_, type::Real);
DEFINE_CMD1(LoadType, load_type_, type::Type_);
#undef DEFINE_CMD1

#define DEFINE_CMD1(Name, name)                                                \
  Register Name(Register r, type::Type const *t) {                             \
    auto &cmd = MakeCmd(t, Op::Name);                                          \
    cmd.name  = Cmd::Name::Make(r);                                            \
    return cmd.result;                                                         \
  }                                                                            \
  struct AllowSemicolon
DEFINE_CMD1(LoadEnum, load_enum_);
DEFINE_CMD1(LoadFlags, load_flags_);
DEFINE_CMD1(LoadAddr, load_addr_);
DEFINE_CMD1(LoadFunc, load_func_);
#undef DEFINE_CMD1

#define DEFINE_CMD2(Name, name, arg_type, RetType, ret_type, fn)               \
  RegisterOr<ret_type> Name(RegisterOr<arg_type> v1,                           \
                            RegisterOr<arg_type> v2) {                         \
    if (!v1.is_reg_ && !v2.is_reg_) { return fn(v1.val_, v2.val_); }           \
    auto &cmd  = MakeCmd(type::RetType, Op::Name);                             \
    cmd.name   = Cmd::Name::Make(v1, v2);                                      \
    auto &refs = Func::Current->references_;                                   \
    if (v1.is_reg_) { refs[v1.reg_].insert(cmd.result); }                      \
    if (v2.is_reg_) { refs[v2.reg_].insert(cmd.result); }                      \
    return cmd.result;                                                         \
  }                                                                            \
  struct AllowSemicolon
DEFINE_CMD2(AddInt, add_int_, i32, Int, i32, std::plus<i32>{});
DEFINE_CMD2(AddReal, add_real_, double, Real, double, std::plus<double>{});
DEFINE_CMD2(SubInt, sub_int_, i32, Int, i32, std::minus<i32>{});
DEFINE_CMD2(SubReal, sub_real_, double, Real, double, std::minus<double>{});
DEFINE_CMD2(MulInt, mul_int_, i32, Int, i32, std::multiplies<i32>{});
DEFINE_CMD2(MulReal, mul_real_, double, Real, double,
            std::multiplies<double>{});
DEFINE_CMD2(DivInt, div_int_, i32, Int, i32, std::divides<i32>{});
DEFINE_CMD2(DivReal, div_real_, double, Real, double, std::divides<double>{});
DEFINE_CMD2(ModInt, mod_int_, i32, Int, i32, std::modulus<i32>{});
DEFINE_CMD2(ModReal, mod_real_, double, Real, double, std::fmod);
DEFINE_CMD2(LtInt, lt_int_, i32, Bool, bool, std::less<i32>{});
DEFINE_CMD2(LtReal, lt_real_, double, Bool, bool, std::less<double>{});
DEFINE_CMD2(LtFlags, lt_flags_, FlagsVal, Bool, bool,
            [](FlagsVal lhs, FlagsVal rhs) {
              return lhs.value != rhs.value &&
                     ((lhs.value | rhs.value) == rhs.value);
            });
DEFINE_CMD2(LeInt, le_int_, i32, Bool, bool, std::less_equal<i32>{});
DEFINE_CMD2(LeReal, le_real_, double, Bool, bool, std::less_equal<double>{});
DEFINE_CMD2(LeFlags, le_flags_, FlagsVal, Bool, bool,
            [](FlagsVal lhs, FlagsVal rhs) {
              return (lhs.value | rhs.value) == rhs.value;
            });
DEFINE_CMD2(GtInt, gt_int_, i32, Bool, bool, std::less<i32>{});
DEFINE_CMD2(GtReal, gt_real_, double, Bool, bool, std::less<double>{});
DEFINE_CMD2(GtFlags, gt_flags_, FlagsVal, Bool, bool,
            [](FlagsVal lhs, FlagsVal rhs) {
              return lhs.value != rhs.value &&
                     ((lhs.value | rhs.value) == lhs.value);
            });
DEFINE_CMD2(GeInt, ge_int_, i32, Bool, bool, std::less_equal<i32>{});
DEFINE_CMD2(GeReal, ge_real_, double, Bool, bool, std::less_equal<double>{});
DEFINE_CMD2(GeFlags, ge_flags_, FlagsVal, Bool, bool,
            [](FlagsVal lhs, FlagsVal rhs) {
              return (lhs.value | rhs.value) == lhs.value;
            });
DEFINE_CMD2(NeChar, ne_char_, char, Bool, bool, std::not_equal_to<char>{});
DEFINE_CMD2(NeInt, ne_int_, i32, Bool, bool, std::not_equal_to<i32>{});
DEFINE_CMD2(NeReal, ne_real_, double, Bool, bool, std::not_equal_to<double>{});
DEFINE_CMD2(NeType, ne_type_, const type::Type *, Bool, bool,
            std::not_equal_to<const type::Type *>{});
DEFINE_CMD2(NeEnum, ne_enum_, EnumVal, Bool, bool,
            std::not_equal_to<EnumVal>{});
DEFINE_CMD2(NeFlags, ne_flags_, FlagsVal, Bool, bool,
            std::not_equal_to<FlagsVal>{});
DEFINE_CMD2(NeAddr, ne_addr_, Addr, Bool, bool, std::not_equal_to<Addr>{});
DEFINE_CMD2(Arrow, arrow_, type::Type const *, Type_, type::Type const *,
            [](type::Type const *lhs, type::Type const *rhs) {
              base::vector<type::Type const *> ins =
                  lhs->is<type::Tuple>()
                      ? lhs->as<type::Tuple>().entries_
                      : base::vector<type::Type const *>{lhs};
              base::vector<type::Type const *> outs =
                  rhs->is<type::Tuple>()
                      ? rhs->as<type::Tuple>().entries_
                      : base::vector<type::Type const *>{rhs};
              return type::Func(std::move(ins), std::move(outs));
            });
#undef DEFINE_CMD2

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
  auto &cmd     = MakeCmd(type::Bool, Op::XorBool);
  cmd.xor_bool_ = Cmd::XorBool::Make(v1, v2);
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

Register Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));

  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(Func::Current->entry())
                  .cmds_.emplace_back(type::Ptr(t), Op::Alloca);

  cmd.alloca_ = Cmd::Alloca::Make(t);
  return cmd.result;
}

void SetReturn(size_t n, Val const &v2) {
  ASSERT(v2.type != nullptr);
  if (v2.type == type::Bool) { return SetReturnBool(n, v2.reg_or<bool>()); }
  if (v2.type == type::Char) { return SetReturnChar(n, v2.reg_or<char>()); }
  if (v2.type == type::Int) { return SetReturnInt(n, v2.reg_or<i32>()); }
  if (v2.type == type::Real) { return SetReturnReal(n, v2.reg_or<double>()); }
  if (v2.type == type::Type_) {
    return SetReturnType(n, v2.reg_or<type::Type const *>());
  }
  if (v2.type->is<type::Enum>()) {
    return SetReturnEnum(n, v2.reg_or<EnumVal>());
  }
  if (v2.type->is<type::Flags>()) {
    return SetReturnFlags(n, v2.reg_or<FlagsVal>());
  }
  if (v2.type->is<type::CharBuffer>()) {
    return SetReturnCharBuf(n, v2.reg_or<std::string_view>());
  }
  if (v2.type->is<type::Pointer>()) {
    return SetReturnAddr(n, v2.reg_or<ir::Addr>());
  }
  if (v2.type->is<type::Function>()) {
    return std::visit(
        [&](auto &val) {
          using val_t = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<val_t, ir::Func *> ||
                        std::is_same_v<val_t, ir::ForeignFn>) {
            return SetReturnFunc(n, ir::AnyFunc{val});
          } else if constexpr (std::is_same_v<val_t, ir::Register>) {
            return SetReturnFunc(n, val);
          } else {
            UNREACHABLE(val);
          }
        },
        v2.value);
  }
  if (v2.type == type::Generic) {
    return SetReturnGeneric(n, v2.reg_or<ast::FunctionLiteral *>());
  }

  if (v2.type == type::Scope) {
    return SetReturnScope(n, v2.reg_or<ast::ScopeLiteral *>());
  }
  if (v2.type == type::Module) {
    return SetReturnModule(n, v2.reg_or<Module const *>());
  }
  if (v2.type == type::Block || v2.type == type::OptBlock ||
      v2.type == type::RepBlock) {
    return SetReturnBlock(n, v2.reg_or<BlockSequence>());
  }
  UNREACHABLE(v2.type->to_string());
}
Register PtrIncr(Register ptr, RegisterOr<i32> inc, type::Type const *t) {
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
  cmd.xor_flags_ = Cmd::XorFlags::Make(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> OrFlags(type::Flags const *type,
                             RegisterOr<FlagsVal> const &lhs,
                             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ | rhs.val_; }
  auto &cmd     = MakeCmd(type, Op::OrFlags);
  cmd.or_flags_ = Cmd::OrFlags::Make(lhs, rhs);
  return cmd.result;
}

RegisterOr<FlagsVal> AndFlags(type::Flags const *type,
                              RegisterOr<FlagsVal> const &lhs,
                              RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) { return lhs.val_ & rhs.val_; }
  auto &cmd      = MakeCmd(type, Op::AndFlags);
  cmd.and_flags_ = Cmd::AndFlags::Make(lhs, rhs);
  return cmd.result;
}

void SetReturnBool(size_t n, RegisterOr<bool> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnBool);
  cmd.set_return_bool_ = Cmd::SetReturnBool::Make(n, r);
  if (r.is_reg_) { Func::Current->references_[r.reg_].insert(cmd.result); }
}

void SetReturnChar(size_t n, RegisterOr<char> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnChar);
  cmd.set_return_char_ = Cmd::SetReturnChar::Make(n, r);
}

void SetReturnInt(size_t n, RegisterOr<i32> r) {
  auto &cmd           = MakeCmd(nullptr, Op::SetReturnInt);
  cmd.set_return_int_ = Cmd::SetReturnInt::Make(n, r);
}

void SetReturnCharBuf(size_t n, RegisterOr<std::string_view> r) {
  auto &cmd                = MakeCmd(nullptr, Op::SetReturnCharBuf);
  cmd.set_return_char_buf_ = Cmd::SetReturnCharBuf::Make(n, r);
}

void SetReturnReal(size_t n, RegisterOr<double> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnReal);
  cmd.set_return_real_ = Cmd::SetReturnReal::Make(n, r);
}

void SetReturnType(size_t n, RegisterOr<type::Type const *> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnType);
  cmd.set_return_type_ = Cmd::SetReturnType::Make(n, r);
}

void SetReturnEnum(size_t n, RegisterOr<EnumVal> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnEnum);
  cmd.set_return_enum_ = Cmd::SetReturnEnum::Make(n, r);
}

void SetReturnFlags(size_t n, RegisterOr<FlagsVal> r) {
  auto &cmd             = MakeCmd(nullptr, Op::SetReturnFlags);
  cmd.set_return_flags_ = Cmd::SetReturnFlags::Make(n, r);
}

void SetReturnAddr(size_t n, RegisterOr<Addr> r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnAddr);
  cmd.set_return_addr_ = Cmd::SetReturnAddr::Make(n, r);
}

void SetReturnFunc(size_t n, RegisterOr<AnyFunc> const &r) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnFunc);
  cmd.set_return_func_ = Cmd::SetReturnFunc::Make(n, r);
}

void SetReturnGeneric(size_t n, RegisterOr<ast::FunctionLiteral *> r) {
  auto &cmd               = MakeCmd(nullptr, Op::SetReturnGeneric);
  cmd.set_return_generic_ = Cmd::SetReturnGeneric::Make(n, r);
}

void SetReturnScope(size_t n, RegisterOr<ast::ScopeLiteral *> r) {
  auto &cmd             = MakeCmd(nullptr, Op::SetReturnScope);
  cmd.set_return_scope_ = Cmd::SetReturnScope::Make(n, r);
}

void SetReturnModule(size_t n, RegisterOr<Module const *> r) {
  auto &cmd              = MakeCmd(nullptr, Op::SetReturnModule);
  cmd.set_return_module_ = Cmd::SetReturnModule::Make(n, r);
}

void SetReturnBlock(size_t n, RegisterOr<BlockSequence> r) {
  auto &cmd             = MakeCmd(nullptr, Op::SetReturnBlock);
  cmd.set_return_block_ = Cmd::SetReturnBlock::Make(n, r);
}

Register Load(Register r, type::Type const *t) {
  if (t == type::Bool) { return LoadBool(r); }
  if (t == type::Char) { return LoadChar(r); }
  if (t == type::Int) { return LoadInt(r); }
  if (t == type::Real) { return LoadReal(r); }
  if (t == type::Type_) { return LoadType(r); }
  if (t->is<type::Enum>()) { return LoadEnum(r, t); }
  if (t->is<type::Flags>()) { return LoadFlags(r, t); }
  if (t->is<type::Pointer>()) { return LoadAddr(r, t); }
  if (t->is<type::Function>()) { return LoadFunc(r, t); }
  UNREACHABLE(t);
}

Register Index(type::Type const *t, Register array_ptr,
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
#define OP_MACRO(op)                                                           \
  case Op::op:                                                                 \
    return #op;
#include "ir/op.xmacro.h"
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
    case Op::NegReal: return os << cmd.neg_real_.reg_;
    case Op::ArrayLength: return os << cmd.array_length_.arg_;
    case Op::ArrayData: return os << cmd.array_data_.arg_;
    case Op::Ptr: return os << cmd.ptr_.reg_;
    case Op::LoadBool: return os << cmd.load_bool_.arg_;
    case Op::LoadChar: return os << cmd.load_char_.arg_;
    case Op::LoadInt: return os << cmd.load_int_.arg_;
    case Op::LoadReal: return os << cmd.load_real_.arg_;
    case Op::LoadType: return os << cmd.load_type_.arg_;
    case Op::LoadEnum: return os << cmd.load_enum_.arg_;
    case Op::LoadFlags: return os << cmd.load_type_.arg_;
    case Op::LoadAddr: return os << cmd.load_addr_.arg_;
    case Op::LoadFunc: return os << cmd.load_func_.arg_;
    case Op::PrintBool: return os << cmd.print_bool_.arg_;
    case Op::PrintChar: return os << cmd.print_char_.arg_;
    case Op::PrintInt: return os << cmd.print_int_.arg_;
    case Op::PrintReal: return os << cmd.print_real_.arg_;
    case Op::PrintType: return os << cmd.print_type_.arg_;
    case Op::PrintEnum: return os << cmd.print_enum_.arg_;
    case Op::PrintFlags: return os << cmd.print_flags_.arg_;
    case Op::PrintAddr: return os << cmd.print_addr_.arg_;
    case Op::PrintCharBuffer: return os << cmd.print_char_buffer_.arg_;
    case Op::AddInt: return os << cmd.add_int_.args_;
    case Op::AddReal: return os << cmd.add_real_.args_;
    case Op::SubInt: return os << cmd.sub_int_.args_;
    case Op::SubReal: return os << cmd.sub_real_.args_;
    case Op::MulInt: return os << cmd.mul_int_.args_;
    case Op::MulReal: return os << cmd.mul_real_.args_;
    case Op::DivInt: return os << cmd.div_int_.args_;
    case Op::DivReal: return os << cmd.div_real_.args_;
    case Op::ModInt: return os << cmd.mod_int_.args_;
    case Op::ModReal: return os << cmd.mod_real_.args_;
    case Op::LtInt: return os << cmd.lt_int_.args_;
    case Op::LtReal: return os << cmd.lt_real_.args_;
    case Op::LtFlags: return os << cmd.lt_flags_.args_;
    case Op::LeInt: return os << cmd.le_int_.args_;
    case Op::LeReal: return os << cmd.le_real_.args_;
    case Op::LeFlags: return os << cmd.le_flags_.args_;
    case Op::GtInt: return os << cmd.gt_int_.args_;
    case Op::GtReal: return os << cmd.gt_real_.args_;
    case Op::GtFlags: return os << cmd.gt_flags_.args_;
    case Op::GeInt: return os << cmd.ge_int_.args_;
    case Op::GeReal: return os << cmd.ge_real_.args_;
    case Op::GeFlags: return os << cmd.ge_flags_.args_;
    case Op::EqBool:
      return os << cmd.eq_bool_.args_[0] << " " << cmd.eq_bool_.args_[1];
    case Op::EqChar: return os << cmd.eq_char_.args_;
    case Op::EqInt: return os << cmd.eq_int_.args_;
    case Op::EqReal: return os << cmd.eq_real_.args_;
    case Op::EqEnum: return os << cmd.eq_enum_.args_;
    case Op::EqFlags: return os << cmd.eq_flags_.args_;
    case Op::EqType: return os << cmd.eq_type_.args_;
    case Op::EqAddr: return os << cmd.eq_addr_.args_;
    case Op::NeChar: return os << cmd.ne_char_.args_;
    case Op::NeInt: return os << cmd.ne_int_.args_;
    case Op::NeReal: return os << cmd.ne_real_.args_;
    case Op::NeEnum: return os << cmd.ne_enum_.args_;
    case Op::NeFlags: return os << cmd.ne_flags_.args_;
    case Op::NeType: return os << cmd.ne_type_.args_;
    case Op::NeAddr: return os << cmd.ne_addr_.args_;
    case Op::XorBool: return os << cmd.xor_bool_.args_;
    case Op::XorFlags: return os << cmd.xor_flags_.args_;
    case Op::OrFlags: return os << cmd.or_flags_.args_;
    case Op::AndFlags: return os << cmd.and_flags_.args_;
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
    case Op::Arrow: return os << cmd.arrow_.args_;
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

    case Op::CastIntToReal: return os << cmd.cast_int_to_real_.reg_;

    case Op::CastPtr: return os << cmd.cast_ptr_.type_;

    case Op::Contextualize: NOT_YET();

    case Op::StoreBool:
      return os << cmd.store_bool_.addr_ << " " << cmd.store_bool_.val_;
    case Op::StoreChar:
      return os << cmd.store_char_.addr_ << " " << cmd.store_char_.val_;
    case Op::StoreInt:
      return os << cmd.store_int_.addr_ << " " << cmd.store_int_.val_;
    case Op::StoreReal:
      return os << cmd.store_real_.addr_ << " " << cmd.store_real_.val_;
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
    case Op::SetReturnBool: return os << cmd.set_return_bool_.val_;
    case Op::SetReturnChar: return os << cmd.set_return_char_.val_;
    case Op::SetReturnInt: return os << cmd.set_return_int_.val_;
    case Op::SetReturnReal: return os << cmd.set_return_real_.val_;
    case Op::SetReturnType: return os << cmd.set_return_type_.val_;
    case Op::SetReturnEnum: return os << cmd.set_return_enum_.val_;
    case Op::SetReturnFlags: return os << cmd.set_return_flags_.val_;
    case Op::SetReturnCharBuf: return os << cmd.set_return_char_buf_.val_;
    case Op::SetReturnAddr: return os << cmd.set_return_addr_.val_;
    case Op::SetReturnFunc: return os << cmd.set_return_func_.val_;
    case Op::SetReturnScope: return os << cmd.set_return_scope_.val_;
    case Op::SetReturnGeneric: return os << cmd.set_return_scope_.val_;
    case Op::SetReturnModule: return os << cmd.set_return_module_.val_;
    case Op::SetReturnBlock: return os << cmd.set_return_block_.val_;
    case Op::PhiBool: return os << cmd.phi_bool_.args_;
    case Op::PhiChar: return os << cmd.phi_char_.args_;
    case Op::PhiInt: return os << cmd.phi_int_.args_;
    case Op::PhiReal: return os << cmd.phi_real_.args_;
    case Op::PhiType: return os << cmd.phi_type_.args_;
    case Op::PhiBlock: return os << cmd.phi_block_.args_;
    case Op::PhiAddr: return os << cmd.phi_addr_.args_;
    case Op::Death: return os;
    case Op::DebugIr: return os;
  }
  UNREACHABLE();
}
}  // namespace ir
