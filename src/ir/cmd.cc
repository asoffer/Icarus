#include "cmd.h"

#include <cmath>
#include <iostream>

#include "architecture.h"
#include "base/container/vector.h"
#include "ir/func.h"
#include "type/all.h"

namespace IR {
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

void LongArgs::append(const IR::Val &val) {
  // TODO deal with alignment?
  std::visit(
      base::overloaded{
          [](const IR::Interface &) { UNREACHABLE(); },
          [&](auto &&val) {
            args_.append(val);
            is_reg_.push_back(
                std::is_same_v<IR::Register, std::decay_t<decltype(val)>>);
          }},
      val.value);
}

static Cmd &MakeCmd(const type::Type *t, Op op) {
  auto &cmd = ASSERT_NOT_NULL(Func::Current)
                  ->block(BasicBlock::Current)
                  .cmds_.emplace_back(t, op);
  return cmd;
}

Val Trunc(const Val &v) {
  if (const i32 *n = std::get_if<i32>(&v.value)) {
    return Val::Char(static_cast<char>(*n));
  }

  auto &cmd  = MakeCmd(type::Char, Op::Trunc);
  cmd.trunc_ = Cmd::Trunc::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Extend(const Val &v) {
  if (const char *c = std::get_if<char>(&v.value)) {
    return Val::Int(static_cast<i32>(*c));
  }

  auto &cmd   = MakeCmd(type::Int, Op::Extend);
  cmd.extend_ = Cmd::Extend::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Bytes(const Val &v) {
  auto &cmd         = MakeCmd(type::Int, Op::Bytes);
  const Register *r = std::get_if<Register>(&v.value);
  cmd.bytes_        = Cmd::Bytes::Make(r ? RegisterOr<const type::Type *>(*r)
                                  : RegisterOr<const type::Type *>(
                                        std::get<const type::Type *>(v.value)));
  return cmd.reg();
}

Val Align(const Val &v) {
  auto &cmd         = MakeCmd(type::Int, Op::Align);
  Register const *r = std::get_if<Register>(&v.value);
  cmd.align_        = Cmd::Align::Make(r ? RegisterOr<const type::Type *>(*r)
                                  : RegisterOr<const type::Type *>(
                                        std::get<const type::Type *>(v.value)));
  return cmd.reg();
}

Val Not(const Val &v) {
  if (const bool *b = std::get_if<bool>(&v.value)) { return Val::Bool(!*b); }
  auto &cmd = MakeCmd(type::Bool, Op::Not);
  cmd.not_  = Cmd::Not::Make(std::get<Register>(v.value));
  Func::Current->references_[cmd.not_.reg_].insert(cmd.result);

  return cmd.reg();
}

// TODO do you really want to support this? How can array allocation be
// customized?
Val Malloc(const type::Type *t, const Val &v) {
  auto &cmd         = MakeCmd(type::Ptr(t), Op::Malloc);
  Register const *r = std::get_if<Register>(&v.value);
  cmd.malloc_       = Cmd::Malloc::Make(r ? RegisterOr<i32>(*r)
                                    : RegisterOr<i32>(std::get<i32>(v.value)));
  return cmd.reg();
}

void Free(const Val &v) {
  auto &cmd = MakeCmd(v.type, Op::Free);
  cmd.free_ = Cmd::Free::Make(std::get<Register>(v.value));
}

Val NegInt(const Val &v) {
  if (const i32 *n = std::get_if<i32>(&v.value)) { return Val::Int(-*n); }
  auto &cmd    = MakeCmd(type::Bool, Op::NegInt);
  cmd.neg_int_ = Cmd::NegInt::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val NegReal(const Val &v) {
  if (const double *r = std::get_if<double>(&v.value)) {
    return Val::Real(-*r);
  }
  auto &cmd     = MakeCmd(type::Bool, Op::NegReal);
  cmd.neg_real_ = Cmd::NegReal::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val ArrayLength(const Val &v) {
  auto &cmd         = MakeCmd(type::Ptr(type::Int), Op::ArrayLength);
  const Register *r = std::get_if<Register>(&v.value);
  cmd.array_length_ = Cmd::ArrayLength::Make(
      r ? RegisterOr<IR::Addr>(*r)
        : RegisterOr<IR::Addr>(std::get<IR::Addr>(v.value)));
  return cmd.reg();
}

Val ArrayData(const Val &v) {
  ASSERT(v.type, Is<type::Pointer>());
  auto *ptee = v.type->as<type::Pointer>().pointee;
  ASSERT(ptee, Is<type::Array>());
  auto *array_type = &ptee->as<type::Array>();
  ASSERT(!array_type->fixed_length);

  auto &cmd         = MakeCmd(type::Ptr(array_type->data_type), Op::ArrayData);
  const Register *r = std::get_if<Register>(&v.value);
  cmd.array_data_   = Cmd::ArrayData::Make(
      r ? RegisterOr<IR::Addr>(*r)
        : RegisterOr<IR::Addr>(std::get<IR::Addr>(v.value)));
  return cmd.reg();
}

Val Ptr(const Val &v) {
  ASSERT(v.type == type::Type_);
  if (type::Type const *const *t = std::get_if<const type::Type *>(&v.value)) {
    return Val::Type(type::Ptr(*t));
  }

  auto &cmd = MakeCmd(type::Type_, Op::Ptr);
  cmd.ptr_  = Cmd::Ptr::Make(std::get<Register>(v.value));
  return cmd.reg();
}

#define DEFINE_CMD1(Name, name, arg_type, RetType)                             \
  Val Name(Val const &v) {                                                     \
    auto &cmd = MakeCmd(RetType, Op::Name);                                    \
    auto *x   = std::get_if<arg_type>(&v.value);                               \
    cmd.name  = Cmd::Name::Make(                                               \
        x ? RegisterOr<arg_type>(*x)                                          \
          : RegisterOr<arg_type>(std::get<Register>(v.value)));               \
    return cmd.reg();                                                          \
  }                                                                            \
  struct AllowSemicolon
DEFINE_CMD1(LoadBool, load_bool_, IR::Addr, type::Bool);
DEFINE_CMD1(LoadChar, load_char_, IR::Addr, type::Char);
DEFINE_CMD1(LoadInt, load_int_, IR::Addr, type::Int);
DEFINE_CMD1(LoadReal, load_real_, IR::Addr, type::Real);
DEFINE_CMD1(LoadType, load_type_, IR::Addr, type::Type_);
DEFINE_CMD1(LoadEnum, load_enum_, IR::Addr,
            v.type->as<type::Pointer>().pointee);
DEFINE_CMD1(LoadFlags, load_flags_, IR::Addr,
            v.type->as<type::Pointer>().pointee);
DEFINE_CMD1(LoadAddr, load_addr_, IR::Addr,
            v.type->as<type::Pointer>().pointee);
DEFINE_CMD1(PrintBool, print_bool_, bool, type::Bool);
DEFINE_CMD1(PrintChar, print_char_, char, type::Char);
DEFINE_CMD1(PrintInt, print_int_, i32, type::Int);
DEFINE_CMD1(PrintReal, print_real_, double, type::Real);
DEFINE_CMD1(PrintType, print_type_, type::Type const *, type::Type_);
DEFINE_CMD1(PrintEnum, print_enum_, EnumVal, v.type);
DEFINE_CMD1(PrintFlags, print_flags_, FlagsVal, v.type);
DEFINE_CMD1(PrintAddr, print_addr_, IR::Addr, v.type);
DEFINE_CMD1(PrintCharBuffer, print_char_buffer_, std::string_view, v.type);
#undef DEFINE_CMD1

Val AddCharBuf(const Val &v1, const Val &v2) {
  auto *s1 = std::get_if<std::string_view>(&v1.value);
  auto *s2 = std::get_if<std::string_view>(&v2.value);
  if (s1 && s2) {
    return IR::Val::CharBuf(std::string(*s1) + std::string(*s2));
  }

  auto &cmd         = MakeCmd(nullptr /* TODO */, Op::AddCharBuf);
  cmd.add_char_buf_ = Cmd::AddCharBuf::Make(
      s1 ? RegisterOr<std::string_view>(*s1)
         : RegisterOr<std::string_view>(std::get<Register>(v1.value)),
      s2 ? RegisterOr<std::string_view>(*s2)
         : RegisterOr<std::string_view>(std::get<Register>(v2.value)));
  return cmd.reg();
}

#define DEFINE_CMD2(Name, name, arg_type, RetType, fn)                         \
  Val Name(Val const &v1, Val const &v2) {                                     \
    auto const *x1 = std::get_if<arg_type>(&v1.value);                         \
    auto const *x2 = std::get_if<arg_type>(&v2.value);                         \
    if (x1 && x2) { return Val::RetType(fn(*x1, *x2)); }                       \
    auto &cmd = MakeCmd(type::RetType, Op::Name);                              \
    cmd.name  = Cmd::Name::Make(                                               \
        x1 ? RegisterOr<arg_type>(*x1)                                        \
           : RegisterOr<arg_type>(std::get<Register>(v1.value)),              \
        x2 ? RegisterOr<arg_type>(*x2)                                        \
           : RegisterOr<arg_type>(std::get<Register>(v2.value)));             \
    if (!x1) {                                                                 \
      Func::Current->references_[std::get<Register>(v1.value)].insert(         \
          cmd.result);                                                         \
    }                                                                          \
    if (!x2) {                                                                 \
      Func::Current->references_[std::get<Register>(v2.value)].insert(         \
          cmd.result);                                                         \
    }                                                                          \
    return cmd.reg();                                                          \
  }                                                                            \
  struct AllowSemicolon
DEFINE_CMD2(AddInt, add_int_, i32, Int, std::plus<i32>{});
DEFINE_CMD2(AddReal, add_real_, double, Real, std::plus<double>{});
DEFINE_CMD2(SubInt, sub_int_, i32, Int, std::minus<i32>{});
DEFINE_CMD2(SubReal, sub_real_, double, Real, std::minus<double>{});
DEFINE_CMD2(MulInt, mul_int_, i32, Int, std::multiplies<i32>{});
DEFINE_CMD2(MulReal, mul_real_, double, Real, std::multiplies<double>{});
DEFINE_CMD2(DivInt, div_int_, i32, Int, std::divides<i32>{});
DEFINE_CMD2(DivReal, div_real_, double, Real, std::divides<double>{});
DEFINE_CMD2(ModInt, mod_int_, i32, Int, std::modulus<i32>{});
DEFINE_CMD2(ModReal, mod_real_, double, Real, std::fmod);
DEFINE_CMD2(LtInt, lt_int_, i32, Bool, std::less<i32>{});
DEFINE_CMD2(LtReal, lt_real_, double, Bool, std::less<double>{});
DEFINE_CMD2(LtFlags, lt_flags_, FlagsVal, Bool, [](FlagsVal lhs, FlagsVal rhs) {
  return lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value);
});
DEFINE_CMD2(LeInt, le_int_, i32, Bool, std::less_equal<i32>{});
DEFINE_CMD2(LeReal, le_real_, double, Bool, std::less_equal<double>{});
DEFINE_CMD2(LeFlags, le_flags_, FlagsVal, Bool, [](FlagsVal lhs, FlagsVal rhs) {
  return (lhs.value | rhs.value) == rhs.value;
});
DEFINE_CMD2(GtInt, gt_int_, i32, Bool, std::less<i32>{});
DEFINE_CMD2(GtReal, gt_real_, double, Bool, std::less<double>{});
DEFINE_CMD2(GtFlags, gt_flags_, FlagsVal, Bool, [](FlagsVal lhs, FlagsVal rhs) {
  return lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value);
});
DEFINE_CMD2(GeInt, ge_int_, i32, Bool, std::less_equal<i32>{});
DEFINE_CMD2(GeReal, ge_real_, double, Bool, std::less_equal<double>{});
DEFINE_CMD2(GeFlags, ge_flags_, FlagsVal, Bool, [](FlagsVal lhs, FlagsVal rhs) {
  return (lhs.value | rhs.value) == lhs.value;
});
DEFINE_CMD2(EqChar, eq_char_, char, Bool, std::equal_to<char>{});
DEFINE_CMD2(EqInt, eq_int_, i32, Bool, std::equal_to<i32>{});
DEFINE_CMD2(EqReal, eq_real_, double, Bool, std::equal_to<double>{});
DEFINE_CMD2(EqType, eq_type_, const type::Type *, Bool,
            std::equal_to<const type::Type *>{});
DEFINE_CMD2(EqFlags, eq_flags_, FlagsVal, Bool, std::equal_to<FlagsVal>{});
DEFINE_CMD2(EqAddr, eq_addr_, Addr, Bool, std::equal_to<Addr>{});
DEFINE_CMD2(NeChar, ne_char_, char, Bool, std::not_equal_to<char>{});
DEFINE_CMD2(NeInt, ne_int_, i32, Bool, std::not_equal_to<i32>{});
DEFINE_CMD2(NeReal, ne_real_, double, Bool, std::not_equal_to<double>{});
DEFINE_CMD2(NeType, ne_type_, const type::Type *, Bool,
            std::not_equal_to<const type::Type *>{});
DEFINE_CMD2(NeFlags, ne_flags_, FlagsVal, Bool, std::not_equal_to<FlagsVal>{});
DEFINE_CMD2(NeAddr, ne_addr_, Addr, Bool, std::not_equal_to<Addr>{});
DEFINE_CMD2(Arrow, arrow_, type::Type const *, Type_,
            [](type::Type const *lhs, type::Type const *rhs) {
              return type::Func({lhs}, {rhs});
            });
#undef DEFINE_CMD2

RegisterOr<type::Type const *> Arrow(RegisterOr<type::Type const *> in,
                                     RegisterOr<type::Type const *> out) {
  if (!in.is_reg_ && !out.is_reg_) {
    return RegisterOr<type::Type const *>(type::Func({in.val_}, {out.val_}));
  }
  auto &cmd = MakeCmd(type::Type_, Op::Arrow);
  cmd.arrow_ =
      Cmd::Arrow::Make(in.is_reg_ ? RegisterOr<type::Type const *>(in.reg_)
                                  : RegisterOr<type::Type const *>(in.val_),
                       out.is_reg_ ? RegisterOr<type::Type const *>(out.reg_)
                                   : RegisterOr<type::Type const *>(out.val_));
  return RegisterOr<type::Type const *>(cmd.result);
}

Val EqBool(Val const &v1, Val const &v2) {
  if (auto const *b = std::get_if<bool>(&v1.value)) {
    return *b ? v2 : Not(v2);
  }
  if (auto const *b = std::get_if<bool>(&v1.value)) {
    return *b ? v1 : Not(v1);
  }

  auto &cmd    = MakeCmd(type::Bool, Op::EqBool);
  cmd.eq_bool_ = Cmd::EqBool::Make(std::get<Register>(v1.value),
                                   std::get<Register>(v2.value));
  return cmd.reg();
}

Val NeBool(Val const &v1, Val const &v2) {
  if (auto const *b = std::get_if<bool>(&v1.value)) {
    return *b ? Not(v2) : v2;
  }
  if (auto const *b = std::get_if<bool>(&v1.value)) {
    return *b ? Not(v1) : v1;
  }

  auto &cmd    = MakeCmd(type::Bool, Op::NeBool);
  cmd.ne_bool_ = Cmd::NeBool::Make(std::get<Register>(v1.value),
                                   std::get<Register>(v2.value));
  return cmd.reg();
}

Val Array(const Val &v1, const Val &v2) {
  ASSERT(v2.type == type::Type_);

  i32 const *m               = std::get_if<i32>(&v1.value);
  type::Type const *const *t = std::get_if<const type::Type *>(&v2.value);
  if (t) {
    if (m) { return Val::Type(type::Arr(*t, *m)); }
    if (v1 == Val::None()) { return Val::Type(type::Arr(*t)); }
  }

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = Cmd::Array::Make(
      m ? RegisterOr<i32>(*m)
        : v1 == Val::None() ? RegisterOr<i32>(-1)
                            : RegisterOr<i32>(std::get<Register>(v1.value)),
      t ? RegisterOr<type::Type const *>(*t)
        : RegisterOr<type::Type const *>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Register CreateTuple() { return MakeCmd(type::Type_, Op::CreateTuple).result; }

void AppendToTuple(Register tup, RegisterOr<type::Type const *> entry) {
  auto &cmd                  = MakeCmd(nullptr, Op::AppendToTuple);
  cmd.append_to_tuple_       = Cmd::AppendToTuple::Make(tup, entry);
}

Register FinalizeTuple(Register r) {
  auto &cmd           = MakeCmd(type::Type_, Op::FinalizeTuple);
  cmd.finalize_tuple_ = Cmd::FinalizeTuple::Make(r);
  return cmd.result;
}

Register Tup(base::vector<Val> const &entries) {
  IR::Register tup = IR::CreateTuple();
  for (auto const &val : entries) {
    IR::AppendToTuple(tup, val.reg_or<type::Type const *>());
  }
  return IR::FinalizeTuple(tup);
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
  if (std::all_of(vals.begin(), vals.end(), [](Val const& v) {
        return std::holds_alternative<type::Type const *>(v.value);
        })) {

    base::vector<type::Type const *> types;
    types.reserve(vals.size());
    for (Val const &v : vals) {
      types.push_back(std::get<type::Type const *>(v.value));
    }

    return type::Var(std::move(types));
  }
  IR::Register var = IR::CreateVariant();
  for (auto const &val : vals) {
    IR::AppendToVariant(var, val.reg_or<type::Type const *>());
  }
  return IR::FinalizeVariant(var);
}

Val XorBool(const Val &v1, const Val &v2) {
  bool const *x1 = std::get_if<bool>(&v1.value);
  bool const *x2 = std::get_if<bool>(&v2.value);
  if (x1) { return *x1 ? Not(v2) : v2; }
  if (x2) { return *x2 ? Not(v1) : v1; }
  auto &cmd = MakeCmd(type::Bool, Op::XorBool);
  cmd.xor_bool_ =
      Cmd::XorBool::Make(x1 ? RegisterOr<bool>(*x1)
                            : RegisterOr<bool>(std::get<Register>(v1.value)),
                         x2 ? RegisterOr<bool>(*x2)
                            : RegisterOr<bool>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val Field(const Val &v, size_t n) {
  const type::Struct *struct_type =
      &v.type->as<type::Pointer>().pointee->as<type::Struct>();
  const type::Type *result_type = type::Ptr(struct_type->fields_.at(n).type);
  auto &cmd                     = MakeCmd(result_type, Op::Field);
  cmd.field_ = Cmd::Field::Make(std::get<Register>(v.value), struct_type, n);
  return cmd.reg();
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

Cmd::Cmd(const type::Type *t, Op op) : op_code_(op), type(t) {
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

Val OutParams::AppendReg(type::Type const *t) {
  auto reg = Reserve(t, false);
  outs_.emplace_back(reg, false);
  return IR::Val::Reg(reg, t);
}

BlockSequence MakeBlockSeq(const base::vector<IR::BlockSequence> &blocks);

Val BlockSeq(const base::vector<Val> &blocks) {
  if (std::all_of(blocks.begin(), blocks.end(), [](const IR::Val &v) {
        return std::holds_alternative<IR::BlockSequence>(v.value);
      })) {
    std::vector<IR::BlockSequence> block_seqs;
    block_seqs.reserve(blocks.size());
    for (const auto &val : blocks) {
      block_seqs.push_back(std::get<IR::BlockSequence>(val.value));
    }
    return IR::Val::BlockSeq(MakeBlockSeq(block_seqs));
  }

  auto &cmd  = MakeCmd(blocks.back().type, Op::BlockSeq);
  auto *args = &Func::Current->block(BasicBlock::Current)
                    .call_args_.emplace_back(blocks);
  cmd.block_seq_ = Cmd::BlockSeq{{}, args};
  return cmd.reg();
}

Val BlockSeqContains(const Val &v, AST::BlockLiteral *lit) {
  auto &cmd = MakeCmd(type::Bool, Op::BlockSeqContains);
  if (auto *bs = std::get_if<BlockSequence>(&v.value)) {
    return IR::Val::Bool(
        std::any_of(bs->seq_->begin(), bs->seq_->end(),
                    [lit](AST::BlockLiteral *l) { return lit == l; }));
  }
  cmd.block_seq_contains_ =
      Cmd::BlockSeqContains{{}, std::get<Register>(v.value), lit};
  return cmd.reg();
}

Val Cast(const type::Type *to, const Val &v, Context *ctx) {
  if (v.type == to) {
    // TODO lvalue/rvalue?
    return v;
  } else if (i32 const *n = std::get_if<i32>(&v.value); n && to == type::Real) {
    return Val::Real(static_cast<double>(*n));

  } else if (to->is<type::Variant>()) {
    ASSERT(ctx != nullptr);
    // TODO cleanup?
    auto alloc = IR::Val::Reg(Alloca(to), to);

    to->EmitAssign(v.type, std::move(v), alloc, ctx);
    return alloc;

  } else if (v.type->is<type::Pointer>()) {
    auto *ptee_type = v.type->as<type::Pointer>().pointee;
    if (ptee_type->is<type::Array>()) {
      auto &array_type = ptee_type->as<type::Array>();
      if (array_type.fixed_length && Ptr(array_type.data_type) == to) {
        Val v_copy  = v;
        v_copy.type = to;
        return v_copy;
      }
    }
  }

  if (to == type::Real && v.type == type::Int) {
    auto &cmd             = MakeCmd(type::Real, Op::CastIntToReal);
    cmd.cast_int_to_real_ = Cmd::CastIntToReal{{}, std::get<Register>(v.value)};
    return cmd.reg();
  } else if (to->is<type::Pointer>()) {
    auto &cmd     = MakeCmd(to, Op::CastPtr);
    cmd.cast_ptr_ = Cmd::CastPtr{
        {}, std::get<Register>(v.value), to->as<type::Pointer>().pointee};
    return cmd.reg();
  } else {
    UNREACHABLE();
  }
}

Register CreateStruct() { return MakeCmd(type::Type_, Op::CreateStruct).result; }

Register FinalizeStruct(Register r) {
  auto &cmd            = MakeCmd(type::Type_, Op::FinalizeStruct);
  cmd.finalize_struct_ = Cmd::FinalizeStruct::Make(r);
  return cmd.result;
}

Val VariantType(const Val &v) {
  auto &cmd         = MakeCmd(Ptr(type::Type_), Op::VariantType);
  cmd.variant_type_ = Cmd::VariantType::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val VariantValue(type::Type const *t, const Val &v) {
  auto &cmd         = MakeCmd(type::Ptr(t), Op::VariantValue);
  cmd.variant_type_ = Cmd::VariantType::Make(std::get<Register>(v.value));
  return cmd.reg();
}

void CreateStructField(Register struct_type,
                       RegisterOr<type::Type const *> type) {
  auto &cmd = MakeCmd(nullptr, Op::CreateStructField);
  cmd.create_struct_field_ =
      Cmd::CreateStructField::Make(struct_type, std::move(type));
}

void SetStructFieldName(Register struct_type, std::string_view field_name) {
  auto &cmd                  = MakeCmd(nullptr, Op::SetStructFieldName);
  cmd.set_struct_field_name_ = Cmd::SetStructFieldName::Make(struct_type, field_name);
}

Register Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));
  return std::get<IR::Register>(
      ASSERT_NOT_NULL(Func::Current)
          ->block(Func::Current->entry())
          .cmds_.emplace_back(type::Ptr(t), Op::Alloca)
          .reg()
          .value);
}

void SetReturn(size_t n, Val const &v2) {
  if (v2.type == type::Bool) { return SetReturnBool(n, v2); }
  if (v2.type == type::Char) { return SetReturnChar(n, v2); }
  if (v2.type == type::Int) { return SetReturnInt(n, v2); }
  if (v2.type == type::Real) { return SetReturnReal(n, v2); }
  if (v2.type == type::Type_) { return SetReturnType(n, v2); }
  if (v2.type->is<type::Enum>()) { return SetReturnEnum(n, v2); }
  if (v2.type->is<type::Flags>()) { return SetReturnFlags(n, v2); }
  if (v2.type->is<type::CharBuffer>()) { return SetReturnCharBuf(n, v2); }
  if (v2.type->is<type::Pointer>()) { return SetReturnAddr(n, v2); }
  if (v2.type->is<type::Function>()) { return SetReturnFunc(n, v2); }
  if (v2.type->is<type::Scope>()) { return SetReturnScope(n, v2); }
  if (v2.type == type::Module) { return SetReturnModule(n, v2); }
  if (v2.type == type::Generic) { return SetReturnGeneric(n, v2); }
  if (v2.type == type::Block || v2.type == type::OptBlock) {
    return SetReturnBlock(n, v2);
  }
  UNREACHABLE(v2.type->to_string());
}

Val PtrIncr(const Val &v1, const Val &v2) {
  ASSERT(v1.type, Is<type::Pointer>());
  ASSERT(v2.type == type::Int);
  i32 const *n = std::get_if<i32>(&v2.value);
  if (n && *n == 0) { return v1; }
  auto &cmd     = MakeCmd(v1.type, Op::PtrIncr);
  cmd.ptr_incr_ = Cmd::PtrIncr::Make(
      std::get<Register>(v1.value),
      n ? RegisterOr<i32>(*n) : RegisterOr<i32>(std::get<i32>(v2.value)));
  return cmd.reg();
}

Val XorFlags(type::Flags const *type, RegisterOr<FlagsVal> const &lhs,
             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) {
    return Val::Flags(type, lhs.val_ ^ rhs.val_);
  }
  auto &cmd      = MakeCmd(type, Op::XorFlags);
  cmd.xor_flags_ = Cmd::XorFlags::Make(lhs, rhs);
  return cmd.reg();
}

Val OrFlags(type::Flags const *type, RegisterOr<FlagsVal> const &lhs,
            RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) {
    return Val::Flags(type, lhs.val_ | rhs.val_);
  }
  auto &cmd     = MakeCmd(type, Op::OrFlags);
  cmd.or_flags_ = Cmd::OrFlags::Make(lhs, rhs);
  return cmd.reg();
}

Val AndFlags(type::Flags const *type, RegisterOr<FlagsVal> const &lhs,
             RegisterOr<FlagsVal> const &rhs) {
  if (!lhs.is_reg_ && !rhs.is_reg_) {
    return Val::Flags(type, lhs.val_ & rhs.val_);
  }
  auto &cmd      = MakeCmd(type, Op::AndFlags);
  cmd.and_flags_ = Cmd::AndFlags::Make(lhs, rhs);
  return cmd.reg();
}

Val Xor(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return XorBool(v1, v2); }
  if (v1.type->is<type::Flags>()) {
    return XorFlags(&v1.type->as<type::Flags>(), v1.reg_or<FlagsVal>(),
                    v2.reg_or<FlagsVal>());
  }
  UNREACHABLE();
}

Val Add(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return AddInt(v1, v2); }
  if (v1.type == type::Real) { return AddReal(v1, v2); }
  if (v1.type->is<type::CharBuffer>()) { return AddCharBuf(v1, v2); }
  if (v1.type == type::Code) { return AddCharBuf(v1, v2); }
  UNREACHABLE();
}

Val AddCodeBlock(const Val &v1, const Val &v2) { NOT_YET(); }

Val Sub(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return SubInt(v1, v2); }
  if (v1.type == type::Real) { return SubReal(v1, v2); }
  UNREACHABLE();
}

Val Mul(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return MulInt(v1, v2); }
  if (v1.type == type::Real) { return MulReal(v1, v2); }
  UNREACHABLE();
}

Val Div(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return DivInt(v1, v2); }
  if (v1.type == type::Real) { return DivReal(v1, v2); }
  UNREACHABLE();
}

Val Mod(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return ModInt(v1, v2); }
  if (v1.type == type::Real) { return ModReal(v1, v2); }
  UNREACHABLE();
}

void StoreBool(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreBool);
  cmd.store_bool_ =
      Cmd::StoreBool::Make(std::get<Register>(loc.value), val.reg_or<bool>());
}

void StoreChar(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreChar);
  cmd.store_char_ =
      Cmd::StoreChar::Make(std::get<Register>(loc.value), val.reg_or<char>());
}

void StoreInt(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreInt);
  cmd.store_int_ =
      Cmd::StoreInt::Make(std::get<Register>(loc.value), val.reg_or<i32>());
}

void StoreReal(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreReal);
  cmd.store_real_ =
      Cmd::StoreReal::Make(std::get<Register>(loc.value), val.reg_or<double>());
}

void StoreType(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreType);
  cmd.store_type_ = Cmd::StoreType::Make(std::get<Register>(loc.value),
                                         val.reg_or<type::Type const *>());
}

void StoreEnum(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreEnum);
  cmd.store_enum_ = Cmd::StoreEnum::Make(std::get<Register>(loc.value),
                                         val.reg_or<EnumVal>());
}

void StoreFlags(const Val &val, const Val &loc) {
  auto &cmd        = MakeCmd(nullptr, Op::StoreFlags);
  cmd.store_flags_ = Cmd::StoreFlags::Make(std::get<Register>(loc.value),
                                           val.reg_or<FlagsVal>());
}

void StoreAddr(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreAddr);
  cmd.store_addr_ = Cmd::StoreAddr::Make(std::get<Register>(loc.value),
                                         val.reg_or<IR::Addr>());
}

void SetReturnBool(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnBool);
  auto reg_or = v2.reg_or<bool>();
  cmd.set_return_bool_ = Cmd::SetReturnBool::Make(n, reg_or);
  if (reg_or.is_reg_) {
    Func::Current->references_[reg_or.reg_].insert(cmd.result);
  }
}

void SetReturnChar(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnChar);
  cmd.set_return_char_ = Cmd::SetReturnChar::Make(n, v2.reg_or<char>());
}

void SetReturnInt(size_t n, const Val &v2) {
  auto &cmd           = MakeCmd(nullptr, Op::SetReturnInt);
  cmd.set_return_int_ = Cmd::SetReturnInt::Make(n, v2.reg_or<i32>());
}

void SetReturnCharBuf(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnCharBuf);
  cmd.set_return_char_buf_ =
      Cmd::SetReturnCharBuf::Make(n, v2.reg_or<std::string_view>());
}

void SetReturnReal(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnReal);
  cmd.set_return_real_ = Cmd::SetReturnReal::Make(n, v2.reg_or<double>());
}

void SetReturnType(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnType);
  cmd.set_return_type_ =
      Cmd::SetReturnType::Make(n, v2.reg_or<type::Type const *>());
}

void SetReturnEnum(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnEnum);
  cmd.set_return_enum_ = Cmd::SetReturnEnum::Make(n, v2.reg_or<EnumVal>());
}

void SetReturnFlags(size_t n, const Val &v2) {
  auto &cmd             = MakeCmd(nullptr, Op::SetReturnFlags);
  cmd.set_return_flags_ = Cmd::SetReturnFlags::Make(n, v2.reg_or<FlagsVal>());
}

void SetReturnAddr(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnAddr);
  cmd.set_return_addr_ = Cmd::SetReturnAddr::Make(n, v2.reg_or<IR::Addr>());
}

void SetReturnFunc(size_t n, const Val &v2) {
  auto &cmd            = MakeCmd(nullptr, Op::SetReturnFunc);
  cmd.set_return_func_ = Cmd::SetReturnFunc::Make(
      n,
      std::visit(
          base::overloaded{
              [](IR::Func *f) -> RegisterOr<AnyFunc> { return AnyFunc{f}; },
              [](IR::Register r) -> RegisterOr<AnyFunc> { return r; },
              [](IR::ForeignFn f) -> RegisterOr<AnyFunc> { return AnyFunc{f}; },
              [n](auto &&) -> RegisterOr<AnyFunc> { UNREACHABLE(); }},
          v2.value));
}

void SetReturnScope(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnScope);
  cmd.set_return_scope_ =
      Cmd::SetReturnScope::Make(n, v2.reg_or<AST::ScopeLiteral *>());
}

void SetReturnModule(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnModule);
  cmd.set_return_module_ =
      Cmd::SetReturnModule::Make(n, v2.reg_or<Module const *>());
}

void SetReturnGeneric(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnGeneric);
  cmd.set_return_generic_ =
      Cmd::SetReturnGeneric::Make(n, v2.reg_or<AST::Function *>());
}

void SetReturnBlock(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnBlock);
  cmd.set_return_block_ =
      Cmd::SetReturnBlock::Make(n, v2.reg_or<BlockSequence>());
}

void Store(const Val &val, const Val &loc) {
  ASSERT(loc.type, Is<type::Pointer>());
  auto *ptee = loc.type->as<type::Pointer>().pointee;
  if (ptee == type::Bool) { return StoreBool(val, loc); }
  if (ptee == type::Char) { return StoreChar(val, loc); }
  if (ptee == type::Int) { return StoreInt(val, loc); }
  if (ptee == type::Real) { return StoreReal(val, loc); }
  if (ptee == type::Type_) { return StoreType(val, loc); }
  if (ptee->is<type::Enum>()) { return StoreEnum(val, loc); }
  if (ptee->is<type::Flags>()) { return StoreFlags(val, loc); }
  if (ptee->is<type::Pointer>()) { return StoreAddr(val, loc); }
  UNREACHABLE(loc.type);
}

Val Load(const Val &v) {
  auto *ptee = v.type->as<type::Pointer>().pointee;
  if (ptee == type::Bool) { return LoadBool(v); }
  if (ptee == type::Char) { return LoadChar(v); }
  if (ptee == type::Int) { return LoadInt(v); }
  if (ptee == type::Real) { return LoadReal(v); }
  if (ptee == type::Type_) { return LoadType(v); }
  if (ptee->is<type::Enum>()) { return LoadEnum(v); }
  if (ptee->is<type::Flags>()) { return LoadFlags(v); }
  if (ptee->is<type::Pointer>()) { return LoadAddr(v); }
  UNREACHABLE(v.type->to_string());
}

Val Print(const Val &v) {
  if (v.type == type::Bool) { return PrintBool(v); }
  if (v.type == type::Char) { return PrintChar(v); }
  if (v.type == type::Int) { return PrintInt(v); }
  if (v.type == type::Real) { return PrintReal(v); }
  if (v.type == type::Type_) { return PrintType(v); }
  if (v.type->is<type::Enum>()) { return PrintEnum(v); }
  if (v.type->is<type::Flags>()) { return PrintFlags(v); }
  if (v.type->is<type::Pointer>()) { return PrintAddr(v); }
  if (v.type->is<type::CharBuffer>()) { return PrintCharBuffer(v); }
  UNREACHABLE(v.type->to_string());
}

Val Index(const Val &v1, const Val &v2) {
  ASSERT(v1.type, Is<type::Pointer>());
  ASSERT(v2.type == type::Int);
  auto *array_type = &v1.type->as<type::Pointer>().pointee->as<type::Array>();
  // TODO this works but generates worse IR (both here and in llvm). It's worth
  // figuring out how to do this better.
  return PtrIncr(
      Cast(type::Ptr(array_type->data_type),
           array_type->fixed_length ? v1 : Load(ArrayData(v1)), nullptr),
      v2);
}

Val Lt(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return LtInt(v1, v2); }
  if (v1.type == type::Real) { return LtReal(v1, v2); }
  if (v1.type->is<type::Flags>()) { return LtFlags(v1, v2); }
  UNREACHABLE();
}

Val Le(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return LeInt(v1, v2); }
  if (v1.type == type::Real) { return LeReal(v1, v2); }
  if (v1.type->is<type::Flags>()) { return LeFlags(v1, v2); }
  UNREACHABLE();
}

Val Gt(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return GtInt(v1, v2); }
  if (v1.type == type::Real) { return GtReal(v1, v2); }
  if (v1.type->is<type::Flags>()) { return GtFlags(v1, v2); }
  UNREACHABLE();
}

Val Ge(const Val &v1, const Val &v2) {
  if (v1.type == type::Int) { return GeInt(v1, v2); }
  if (v1.type == type::Real) { return GeReal(v1, v2); }
  if (v1.type->is<type::Flags>()) { return GeFlags(v1, v2); }
  UNREACHABLE();
}

Val Eq(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return EqBool(v1, v2); }
  if (v1.type == type::Char) { return EqChar(v1, v2); }
  if (v1.type == type::Int) { return EqInt(v1, v2); }
  if (v1.type == type::Real) { return EqReal(v1, v2); }
  if (v1.type == type::Type_) { return EqType(v1, v2); }
  if (v1.type->is<type::Flags>()) { return EqFlags(v1, v2); }
  if (v1.type->is<type::Pointer>()) { return EqAddr(v1, v2); }

  BlockSequence const *val1 = std::get_if<BlockSequence>(&v1.value);
  BlockSequence const *val2 = std::get_if<BlockSequence>(&v2.value);
  if (val1 != nullptr && val2 != nullptr) { return Val::Bool(*val1 == *val2); }

  // TODO block sequence at runtime?
  UNREACHABLE();
}

Val Ne(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return NeBool(v1, v2); }
  if (v1.type == type::Char) { return NeChar(v1, v2); }
  if (v1.type == type::Int) { return NeInt(v1, v2); }
  if (v1.type == type::Real) { return NeReal(v1, v2); }
  if (v1.type == type::Type_) { return NeType(v1, v2); }
  if (v1.type->is<type::Pointer>()) { return NeAddr(v1, v2); }
  UNREACHABLE();
}

template <typename T>
static std::unique_ptr<PhiArgs<T>> MakePhiArgs(
    const std::unordered_map<BlockIndex, IR::Val> &val_map) {
  auto phi_args = std::make_unique<PhiArgs<T>>();
  for (const auto & [ block, val ] : val_map) {
    phi_args->map_.emplace(block, val.template reg_or<T>());
  }
  return phi_args;
}

CmdIndex Phi(type::Type const *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  MakeCmd(t, Op::Death);
  return cmd_index;
}

Val MakePhi(CmdIndex phi_index,
            const std::unordered_map<BlockIndex, IR::Val> &val_map) {
  auto &cmd = IR::Func::Current->Command(phi_index);
  cmd.type  = val_map.begin()->second.type;

  if (cmd.type == type::Bool) {
    auto phi_args = MakePhiArgs<bool>(val_map);
    cmd.op_code_  = Op::PhiBool;
    cmd.phi_bool_ = Cmd::PhiBool::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type == type::Char) {
    auto phi_args = MakePhiArgs<char>(val_map);
    cmd.op_code_  = Op::PhiChar;
    cmd.phi_char_ = Cmd::PhiChar::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type == type::Int) {
    auto phi_args = MakePhiArgs<i32>(val_map);
    cmd.op_code_  = Op::PhiInt;
    cmd.phi_int_  = Cmd::PhiInt::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type == type::Real) {
    auto phi_args = MakePhiArgs<double>(val_map);
    cmd.op_code_  = Op::PhiReal;
    cmd.phi_real_ = Cmd::PhiReal::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type == type::Type_) {
    auto phi_args = MakePhiArgs<type::Type const *>(val_map);
    cmd.op_code_  = Op::PhiType;
    cmd.phi_type_ = Cmd::PhiType::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type->is<type::Pointer>()) {
    auto phi_args = MakePhiArgs<IR::Addr>(val_map);
    cmd.op_code_  = Op::PhiAddr;
    cmd.phi_addr_ = Cmd::PhiAddr::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else if (cmd.type == type::Block || cmd.type == type::OptBlock) {
    auto phi_args  = MakePhiArgs<BlockSequence>(val_map);
    cmd.op_code_   = Op::PhiBlock;
    cmd.phi_block_ = Cmd::PhiBlock::Make(phi_args.get());
    IR::Func::Current->block(BasicBlock::Current)
        .phi_args_.push_back(std::move(phi_args));
  } else {
    NOT_YET(cmd.type->to_string());
  }
  return cmd.reg();
}

void Call(const Val &fn, LongArgs long_args) {
  ASSERT(long_args.type_ == nullptr);
  ASSERT(fn.type, Is<type::Function>());
  long_args.type_     = &fn.type->as<type::Function>();
  const auto &fn_type = fn.type->as<type::Function>();

  auto &block    = Func::Current->block(BasicBlock::Current);
  LongArgs *args = &block.long_args_.emplace_back(std::move(long_args));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  if (auto *r = std::get_if<Register>(&fn.value)) {
    cmd.call_ = Cmd::Call(*r, args, nullptr);
  } else if (auto *f = std::get_if<Func *>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args, nullptr);
  } else if (auto *f = std::get_if<ForeignFn>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args, nullptr);
  } else {
    UNREACHABLE();
  }
}
void Call(const Val &fn, LongArgs long_args, OutParams outs) {
  ASSERT(long_args.type_ == nullptr);
  ASSERT(fn.type, Is<type::Function>());
  long_args.type_     = &fn.type->as<type::Function>();
  const auto &fn_type = fn.type->as<type::Function>();

  auto &block = Func::Current->block(BasicBlock::Current);
  auto *args  = &block.long_args_.emplace_back(std::move(long_args));

  auto *outs_ptr = &block.outs_.emplace_back(std::move(outs));

  auto &cmd = MakeCmd(nullptr, Op::Call);
  if (auto *r = std::get_if<Register>(&fn.value)) {
    cmd.call_ = Cmd::Call(*r, args, outs_ptr);
  } else if (auto *f = std::get_if<Func *>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args, outs_ptr);
  } else if (auto *f = std::get_if<ForeignFn>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args, outs_ptr);
  } else {
    UNREACHABLE();
  }
}

void CondJump(const Val &cond, BlockIndex true_block, BlockIndex false_block) {
  if (auto *b = std::get_if<bool>(&cond.value)) {
    return UncondJump(*b ? true_block : false_block);
  }
  auto &cmd      = MakeCmd(nullptr, Op::CondJump);
  cmd.cond_jump_ = Cmd::CondJump{
      {}, std::get<Register>(cond.value), {false_block, true_block}};
}

void UncondJump(BlockIndex block) {
  auto &cmd        = MakeCmd(nullptr, Op::UncondJump);
  cmd.uncond_jump_ = Cmd::UncondJump{{}, block};
}

void ReturnJump() {
  auto &cmd        = MakeCmd(nullptr, Op::ReturnJump);
  cmd.return_jump_ = Cmd::ReturnJump{};
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
    case Op::AddCharBuf: return os << cmd.add_char_buf_.args_;
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
    case Op::EqBool: return os << cmd.eq_bool_.args_[0] << " " << cmd.eq_bool_.args_[1];
    case Op::EqChar: return os << cmd.eq_char_.args_;
    case Op::EqInt: return os << cmd.eq_int_.args_;
    case Op::EqReal: return os << cmd.eq_real_.args_;
    case Op::EqFlags: return os << cmd.eq_flags_.args_;
    case Op::EqType: return os << cmd.eq_type_.args_;
    case Op::EqAddr: return os << cmd.eq_addr_.args_;
    case Op::NeBool: return os << cmd.ne_bool_.args_[0] << " " << cmd.ne_bool_.args_[1];
    case Op::NeChar: return os << cmd.ne_char_.args_;
    case Op::NeInt: return os << cmd.ne_int_.args_;
    case Op::NeReal: return os << cmd.ne_real_.args_;
    case Op::NeFlags: return os << cmd.ne_flags_.args_;
    case Op::NeType: return os << cmd.ne_type_.args_;
    case Op::NeAddr: return os << cmd.ne_addr_.args_;
    case Op::XorBool: return os << cmd.xor_bool_.args_;
    case Op::XorFlags: return os << cmd.xor_flags_.args_;
    case Op::OrFlags: return os << cmd.or_flags_.args_;
    case Op::AndFlags: return os << cmd.and_flags_.args_;
    case Op::CreateStruct: return os;
    case Op::CreateStructField:
      return os << cmd.create_struct_field_.struct_ << " "
                << cmd.create_struct_field_.type_;
    case Op::SetStructFieldName:
      return os << cmd.set_struct_field_name_.struct_ << " "
                << cmd.set_struct_field_name_.name_;
    case Op::FinalizeStruct: return os << cmd.finalize_struct_.reg_;

    case Op::Malloc: return os << cmd.malloc_.arg_;
    case Op::Free: return os << cmd.free_.reg_;
    case Op::Alloca:
      return os << cmd.type->as<type::Pointer>().pointee->to_string();

    case Op::Arrow: return os << cmd.arrow_.args_;
    case Op::Array: return os << cmd.array_.type_;
    case Op::CreateTuple: return os;
    case Op::AppendToTuple:
      return os << cmd.append_to_tuple_.tup_ << " "
                << cmd.append_to_tuple_.arg_;
    case Op::FinalizeTuple:
      return os << cmd.finalize_tuple_.tup_;
    case Op::CreateVariant: return os;
    case Op::AppendToVariant:
      return os << cmd.append_to_variant_.var_ << " "
                << cmd.append_to_variant_.arg_;
    case Op::FinalizeVariant:
      return os << cmd.finalize_variant_.var_;
    case Op::VariantType: return os << cmd.variant_type_.reg_;
    case Op::VariantValue: return os << cmd.variant_value_.reg_;
    case Op::PtrIncr: return os << cmd.ptr_incr_.incr_;
    case Op::Field:
      return os << cmd.field_.ptr_ << " " << cmd.field_.struct_type_->to_string() << " "
                << cmd.field_.num_;
    case Op::CondJump:
      return os << cmd.cond_jump_.blocks_[0] << " " << cmd.cond_jump_.blocks_[1];
    case Op::UncondJump: return os << cmd.uncond_jump_.block_;
    case Op::ReturnJump: return os;
    case Op::Call:
      switch (cmd.call_.which_active_) {
        case 0x00: os << cmd.call_.reg_; break;
        case 0x01: os << cmd.call_.fn_; break;
        case 0x02: os << cmd.call_.foreign_fn_.name_; break;
      }
      os << cmd.call_.long_args_->to_string();
      if (cmd.call_.outs_) {
        for (const auto &out : cmd.call_.outs_->outs_) {
          if (out.is_loc_) { os << "*"; }
          os << out.reg_;
        }
      }

      return os;
    case Op::BlockSeq: NOT_YET();
    case Op::BlockSeqContains: return os << cmd.block_seq_contains_.lit_;

    case Op::CastIntToReal: return os << cmd.cast_int_to_real_.reg_;

    case Op::CastPtr: return os << cmd.cast_ptr_.type_;

    case Op::AddCodeBlock: NOT_YET();
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
      return os << cmd.store_type_.addr_ << " " << cmd.store_type_.val_;
    case Op::StoreEnum:
      return os << cmd.store_enum_.addr_ << " " << cmd.store_enum_.val_;
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
    case Op::SetReturnModule: return os << cmd.set_return_module_.val_;
    case Op::SetReturnGeneric: return os << cmd.set_return_generic_.val_;
    case Op::SetReturnBlock: return os << cmd.set_return_block_.val_;
    case Op::PhiBool: return os << cmd.phi_bool_.args_;
    case Op::PhiChar: return os << cmd.phi_char_.args_;
    case Op::PhiInt: return os << cmd.phi_int_.args_;
    case Op::PhiReal: return os << cmd.phi_real_.args_;
    case Op::PhiType: return os << cmd.phi_type_.args_;
    case Op::PhiBlock: return os << cmd.phi_block_.args_;
    case Op::PhiAddr: return os << cmd.phi_addr_.args_;
    case Op::Death: return os;
  }
  UNREACHABLE();
}
}  // namespace IR
