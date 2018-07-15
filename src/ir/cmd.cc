#include "cmd.h"

#include <cmath>
#include <iostream>
#include "base/container/vector.h"

#include "ir/func.h"
#include "type/all.h"

namespace IR {
using base::check::Is;
BlockIndex BasicBlock::Current;

static Cmd &MakeNewCmd(const type::Type *t, Op op) {
  return ASSERT_NOT_NULL(Func::Current)
      ->block(BasicBlock::Current)
      .cmds_.emplace_back(t, op, base::vector<IR::Val>{});
}

Val Trunc(const Val &v) {
  if (const i32 *n = std::get_if<i32>(&v.value)) {
    return Val::Char(static_cast<char>(*n));
  }

  auto &cmd  = MakeNewCmd(type::Char, Op::Trunc);
  cmd.trunc_ = Cmd::Trunc::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Extend(const Val &v) {
  if (const char *c = std::get_if<char>(&v.value)) {
    return Val::Int(static_cast<i32>(*c));
  }

  auto &cmd   = MakeNewCmd(type::Int, Op::Extend);
  cmd.extend_ = Cmd::Extend::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Bytes(const Val &v) {
  auto &cmd         = MakeNewCmd(type::Int, Op::Bytes);
  const Register *r = std::get_if<Register>(&v.value);
  cmd.bytes_        = Cmd::Bytes::Make(r ? RegisterOr<const type::Type *>(*r)
                                  : RegisterOr<const type::Type *>(
                                        std::get<const type::Type *>(v.value)));
  return cmd.reg();
}

Val Align(const Val &v) {
  auto &cmd         = MakeNewCmd(type::Int, Op::Align);
  Register const *r = std::get_if<Register>(&v.value);
  cmd.align_        = Cmd::Align::Make(r ? RegisterOr<const type::Type *>(*r)
                                  : RegisterOr<const type::Type *>(
                                        std::get<const type::Type *>(v.value)));
  return cmd.reg();
}

Val Not(const Val &v) {
  if (const bool *b = std::get_if<bool>(&v.value)) { return Val::Bool(!*b); }
  auto &cmd = MakeNewCmd(type::Bool, Op::Not);
  cmd.not_  = Cmd::Not::Make(std::get<Register>(v.value));
  return cmd.reg();
}


// TODO do you really want to support this? How can array allocation be
// customized?
Val Malloc(const type::Type *t, const Val &v) {
  auto &cmd         = MakeNewCmd(type::Ptr(t), Op::Malloc);
  Register const *r = std::get_if<Register>(&v.value);
  cmd.malloc_       = Cmd::Malloc::Make(r ? RegisterOr<i32>(*r)
                                    : RegisterOr<i32>(std::get<i32>(v.value)));
  return cmd.reg();
}

void Free(const Val &v) {
  auto &cmd = MakeNewCmd(v.type, Op::Free);
  cmd.free_ = Cmd::Free::Make(std::get<Register>(v.value));
}

Val NegInt(const Val &v) {
  if (const i32 *n = std::get_if<i32>(&v.value)) { return Val::Int(-*n); }
  auto &cmd    = MakeNewCmd(type::Bool, Op::NegInt);
  cmd.neg_int_ = Cmd::NegInt::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val NegReal(const Val &v) {
  if (const double *r = std::get_if<double>(&v.value)) { return Val::Real(-*r); }
  auto &cmd     = MakeNewCmd(type::Bool, Op::NegReal);
  cmd.neg_real_ = Cmd::NegReal::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val ArrayLength(const Val &v) {
  auto &cmd = MakeNewCmd(type::Ptr(type::Int), Op::ArrayLength);
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

  auto &cmd = MakeNewCmd(type::Ptr(array_type->data_type), Op::ArrayData);
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

  auto &cmd = MakeNewCmd(type::Type_, Op::Ptr);
  cmd.ptr_  = Cmd::Ptr::Make(std::get<Register>(v.value));
  return cmd.reg();
}

#define DEFINE_CMD1(Name, name, arg_type, RetType)                             \
  Val Name(Val const &v) {                                                     \
    auto &cmd = MakeNewCmd(RetType, Op::Name);                                 \
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

  auto &cmd         = MakeNewCmd(nullptr /* TODO */, Op::AddCharBuf);
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
    auto &cmd = MakeNewCmd(type::RetType, Op::Name);                           \
    cmd.name  = Cmd::Name::Make(                                               \
        x1 ? RegisterOr<arg_type>(*x1)                                        \
           : RegisterOr<arg_type>(std::get<Register>(v1.value)),              \
        x2 ? RegisterOr<arg_type>(*x2)                                        \
           : RegisterOr<arg_type>(std::get<Register>(v2.value)));             \
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

Val Array(const Val &v1, const Val &v2) {
  ASSERT(v2.type == type::Type_);

  i32 const *m               = std::get_if<i32>(&v1.value);
  type::Type const *const *t = std::get_if<const type::Type *>(&v2.value);
  if (t) {
    if (m) { return Val::Type(type::Arr(*t, *m)); }
    if (v1 == Val::None()) { return Val::Type(type::Arr(*t)); }
  }

  auto &cmd = MakeNewCmd(type::Type_, Op::Array);
  cmd.array_ = Cmd::Array::Make(
      m ? RegisterOr<i32>(*m)
        : v1 == Val::None() ? RegisterOr<i32>(-1)
                            : RegisterOr<i32>(std::get<Register>(v1.value)),
      t ? RegisterOr<type::Type const *>(*t)
        : RegisterOr<type::Type const *>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val Tup(base::vector<Val> vals) {
  LOG << vals;
  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(std::move(vals)));
  auto &cmd = MakeNewCmd(type::Type_, Op::Tup);
  cmd.tup_  = Cmd::Tup{{}, args.get()};
  return cmd.reg();
}

Val Variant(base::vector<Val> vals) {
  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(std::move(vals)));
  auto &cmd    = MakeNewCmd(type::Type_, Op::Variant);
  cmd.variant_ = Cmd::Variant{{}, args.get()};
  return cmd.reg();
}

Val XorBool(const Val &v1, const Val &v2) {
  bool const *x1 = std::get_if<bool>(&v1.value);
  bool const *x2 = std::get_if<bool>(&v2.value);
  if (x1) { return *x1 ? Not(v2) : v2; }
  if (x2) { return *x2 ? Not(v1) : v1; }
  auto &cmd     = MakeNewCmd(type::Bool, Op::XorBool);
  cmd.xor_bool_ =
      Cmd::XorBool::Make(x1 ? RegisterOr<bool>(*x1)
                            : RegisterOr<bool>(std::get<Register>(v1.value)),
                         x2 ? RegisterOr<bool>(*x2)
                            : RegisterOr<bool>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val OrBool(const Val &v1, const Val &v2) {
  bool const *x1 = std::get_if<bool>(&v1.value);
  bool const *x2 = std::get_if<bool>(&v2.value);
  if (x1) { return *x1 ? IR::Val::Bool(true) : v2; }
  if (x2) { return *x2 ? IR::Val::Bool(true) : v1; }
  auto &cmd = MakeNewCmd(type::Bool, Op::OrBool);
  cmd.or_bool_ =
      Cmd::OrBool::Make(x1 ? RegisterOr<bool>(*x1)
                           : RegisterOr<bool>(std::get<Register>(v1.value)),
                        x2 ? RegisterOr<bool>(*x2)
                           : RegisterOr<bool>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val AndBool(const Val &v1, const Val &v2) {
  bool const *x1 = std::get_if<bool>(&v1.value);
  bool const *x2 = std::get_if<bool>(&v2.value);
  if (x1) { return *x1 ? v2 : IR::Val::Bool(false); }
  if (x2) { return *x2 ? v1 : IR::Val::Bool(false); }
  auto &cmd = MakeNewCmd(type::Bool, Op::AndBool);
  cmd.and_bool_ =
      Cmd::AndBool::Make(x1 ? RegisterOr<bool>(*x1)
                            : RegisterOr<bool>(std::get<Register>(v1.value)),
                         x2 ? RegisterOr<bool>(*x2)
                            : RegisterOr<bool>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val Field(const Val &v, size_t n) {
  const type::Type *result_type = type::Ptr(v.type->as<type::Pointer>()
                                                .pointee->as<type::Struct>()
                                                .fields_.at(n)
                                                .type);
  auto &cmd                     = MakeNewCmd(result_type, Op::Field);
  cmd.field_ = Cmd::Field::Make(std::get<Register>(v.value), n);
  return cmd.reg();
}

Cmd::Cmd(const type::Type *t, Op op, base::vector<Val> arg_vec)
    : args(std::move(arg_vec)), op_code_(op), type(t) {
  ASSERT(Func::Current != nullptr);
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  result = Register(t != nullptr ? Func::Current->num_regs_++
                                 : -(++Func::Current->num_voids_));

  Func::Current->reg_to_cmd_.emplace(result, cmd_index);
  if (t == nullptr) { return; }

  Func::Current->references_[result];  // Make sure this entry exists
  for (const auto &val : args) {
    if (auto *reg = std::get_if<Register>(&val.value)) {
      Func::Current->references_[*reg].push_back(result);
    }
  }
}

static Val MakeCmd(const type::Type *t, Op op, base::vector<Val> vals) {
  auto &cmds = ASSERT_NOT_NULL(Func::Current)->block(BasicBlock::Current).cmds_;
  Cmd c{t, op, std::move(vals)};
  cmds.emplace_back(std::move(c));
  return t == nullptr ? IR::Val::None() : cmds.back().reg();
}


extern Val MakeBlockSeq(const base::vector<Val> &blocks);

Val BlockSeq(base::vector<Val> blocks) {
  if (std::all_of(blocks.begin(), blocks.end(), [](const IR::Val &v) {
        return std::holds_alternative<IR::BlockSequence>(v.value);
      })) {
    return MakeBlockSeq(blocks);
  }
  auto *t = blocks.back().type;
  return MakeCmd(t, Op::BlockSeq, std::move(blocks));
}

Val Cast(const type::Type *to, Val v, Context *ctx) {
  if (v.type == to) {
    // TODO lvalue/rvalue?
    return v;

  } else if (i32 *n = std::get_if<i32>(&v.value); n && to == type::Real) {
    return Val::Real(static_cast<double>(*n));

  } else if (to->is<type::Variant>()) {
    ASSERT(ctx != nullptr);
    // TODO cleanup?
    auto alloc = Alloca(to);

    to->EmitAssign(v.type, std::move(v), alloc, ctx);
    return alloc;

  } else if (v.type->is<type::Pointer>()) {
    auto *ptee_type = v.type->as<type::Pointer>().pointee;
    if (ptee_type->is<type::Array>()) {
      auto &array_type = ptee_type->as<type::Array>();
      if (array_type.fixed_length && Ptr(array_type.data_type) == to) {
        v.type = to;
        return v;
      }
    }
  }
  return MakeCmd(to, Op::Cast, base::vector<IR::Val>{std::move(v)});
}

Val CreateStruct() { return MakeNewCmd(type::Type_, Op::CreateStruct).reg(); }

IR::Val FinalizeStruct(const Val &v) {
  auto &cmd = MakeNewCmd(type::Type_, Op::CreateStruct);
  cmd.finalize_struct_ = Cmd::FinalizeStruct::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val VariantType(const Val& v) {
  auto &cmd = MakeNewCmd(Ptr(type::Type_), Op::VariantType);
  cmd.variant_type_ = Cmd::VariantType::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val VariantValue(type::Type const *t, const Val &v) {
  auto &cmd = MakeNewCmd(type::Ptr(t), Op::VariantValue);
  cmd.variant_type_ = Cmd::VariantType::Make(std::get<Register>(v.value));
  return cmd.reg();
}

void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val) {
  Cmd cmd(nullptr, Op::InsertField,
          {std::move(struct_type), Val::CharBuf(field_name), std::move(type),
           std::move(init_val)});
  ASSERT_NOT_NULL(Func::Current)
      ->block(BasicBlock::Current)
      .cmds_.push_back(std::move(cmd));
}

Val Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));
  return ASSERT_NOT_NULL(Func::Current)
      ->block(Func::Current->entry())
      .cmds_.emplace_back(type::Ptr(t), Op::Alloca, base::vector<IR::Val>{})
      .reg();
}

void SetReturn(size_t r, Val v2) {
  // TODO ***maybe*** later optimize a return register
  MakeCmd(nullptr, Op::SetReturn,
          base::vector<IR::Val>{std::move(v2), IR::Func::Current->Return(r)});
}

void Store(Val v1, Val v2) {
  ASSERT(v2.type, Is<type::Pointer>());
  MakeCmd(nullptr, Op::Store,
          base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val PtrIncr(const Val &v1, const Val &v2) {
  ASSERT(v1.type, Is<type::Pointer>());
  ASSERT(v2.type == type::Int);
  i32 const *n = std::get_if<i32>(&v2.value);
  if (n && *n == 0) { return v1; }
  auto &cmd  = MakeNewCmd(v1.type, Op::PtrIncr);
  cmd.ptr_incr_ = Cmd::PtrIncr::Make(
      std::get<Register>(v1.value),
      n ? RegisterOr<i32>(*n) : RegisterOr<i32>(std::get<i32>(v2.value)));
  return cmd.reg();
}

Val XorFlags(const Val &v1, const Val &v2) { NOT_YET(); }
Val OrFlags(const Val &v1, const Val &v2) { NOT_YET(); }
Val AndFlags(const Val &v1, const Val &v2) { NOT_YET(); }

Val Xor(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return XorBool(v1, v2); }
  if (v1.type->is<type::Flags>()) { return XorFlags(v1, v2); }
  UNREACHABLE();
}

Val Or(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return OrBool(v1, v2); }
  if (v1.type->is<type::Flags>()) { return OrFlags(v1, v2); }
  UNREACHABLE();
}

Val And(const Val &v1, const Val &v2) {
  if (v1.type == type::Bool) { return AndBool(v1, v2); }
  if (v1.type->is<type::Flags>()) { return AndFlags(v1, v2); }
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

Val Load(const Val& v) {
  auto *ptee = v.type->as<type::Pointer>().pointee;
  if (ptee == type::Bool) { return LoadBool(v); }
  if (ptee == type::Char) { return LoadChar(v); }
  if (ptee == type::Int) { return LoadInt(v); }
  if (ptee == type::Real) { return LoadReal(v); }
  if (ptee == type::Type_) { return LoadType(v); }
  if (ptee->is<type::Enum>()) { return LoadEnum(v); }
  if (ptee->is<type::Flags>()) { return LoadFlags(v); }
  if (ptee->is<type::Pointer>()) { return LoadAddr(v); }
  UNREACHABLE(v.type);
}

Val Print(const Val& v) {
  if (v.type == type::Bool) { return PrintBool(v); }
  if (v.type == type::Char) { return PrintChar(v); }
  if (v.type == type::Int) { return PrintInt(v); }
  if (v.type == type::Real) { return PrintReal(v); }
  if (v.type == type::Type_) { return PrintType(v); }
  if (v.type->is<type::Enum>()) { return PrintEnum(v); }
  if (v.type->is<type::Flags>()) { return PrintFlags(v); }
  if (v.type->is<type::Pointer>()) { return PrintAddr(v); }
  if (v.type->is<type::CharBuffer>()) { return PrintCharBuffer(v); }
  UNREACHABLE(v.type);
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

Val BlockSeqContains(Val v, AST::BlockLiteral *lit) {
  return MakeCmd(type::Bool, Op::BlockSeqContains,
                 base::vector<IR::Val>{std::move(v), IR::Val::Block(lit)});
}

Val Eq(const Val &v1, const Val &v2) {
  if (const bool *b = std::get_if<bool>(&v1.value)) {
    return *b ? v2 : Not(v2);
  }
  if (const bool *b = std::get_if<bool>(&v2.value)) {
    return *b ? v1 : Not(v1);
  }

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
  if (const bool *b = std::get_if<bool>(&v1.value)) { return *b ? Not(v2) : v2; }
  if (const bool *b = std::get_if<bool>(&v2.value)) { return *b ? Not(v1) : v1; }

  if (v1.type == type::Char) { return NeChar(v1, v2); }
  if (v1.type == type::Int) { return NeInt(v1, v2); }
  if (v1.type == type::Real) { return NeReal(v1, v2); }
  if (v1.type == type::Type_) { return NeType(v1, v2); }
  if (v1.type->is<type::Pointer>()) { return NeAddr(v1, v2); }
  UNREACHABLE();
}

CmdIndex Phi(const type::Type *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};

  Cmd cmd(t, Op::Phi, {});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));

  return cmd_index;
}

Val Call(const Val &fn, base::vector<Val> vals, base::vector<Val> result_locs) {
  ASSERT(fn.type, Is<type::Function>());
  vals.insert(vals.end(), std::make_move_iterator(result_locs.begin()),
              std::make_move_iterator(result_locs.end()));

  // TODO either fix the output type here or do it at the execution site. Not
  // sure which makes sense. "Fix" means that if a function returns a struct or
  // has multiple return values, we actually need to represent this internally
  // with out-params, so a function looks like it returns something, but
  // actually we pass in 'result_locs' which are assigned to.
  // Long-term we should do this consistently even for small types, because for
  // multiple return values, we really could return them in multiple registers
  // rather than allocating stack space.
  const auto &fn_type = fn.type->as<type::Function>();
  const type::Type *output_type =
      (fn_type.output.size() == 1 && !fn_type.output[0]->is_big())
          ? fn_type.output[0]
          : type::Void();

  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(std::move(vals)));
  auto &cmd = MakeNewCmd(output_type, Op::Call);

  if (auto *r = std::get_if<Register>(&fn.value)) {
    cmd.call_ = Cmd::Call(*r, args.get());
  } else if (auto *f = std::get_if<Func *>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args.get());
  } else if (auto *f = std::get_if<ForeignFn>(&fn.value)) {
    cmd.call_ = Cmd::Call(*f, args.get());
  } else {
    UNREACHABLE();
  }
  return cmd.reg();
}

void CondJump(const Val &cond, BlockIndex true_block, BlockIndex false_block) {
  if (auto *b = std::get_if<bool>(&cond.value)) {
    return UncondJump(*b ? true_block : false_block);
  }
  auto &cmd      = MakeNewCmd(nullptr, Op::CondJump);
  cmd.cond_jump_ = Cmd::CondJump{
      {}, std::get<Register>(cond.value), {false_block, true_block}};
}

void UncondJump(BlockIndex block) {
  auto &cmd        = MakeNewCmd(nullptr, Op::UncondJump);
  cmd.uncond_jump_ = Cmd::UncondJump{{}, block};
}

void ReturnJump() {
  auto &cmd        = MakeNewCmd(nullptr, Op::ReturnJump);
  cmd.return_jump_ = Cmd::ReturnJump{};
}

void Cmd::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ');
  if (type != nullptr) { std::cerr << reg().to_string() << " = "; }
  switch (op_code_) {
    case Op::Trunc: std::cerr << "trunc"; break;
    case Op::Extend: std::cerr << "extend"; break;
    case Op::Bytes: std::cerr << "bytes"; break;
    case Op::Align: std::cerr << "align"; break;
    case Op::Not: std::cerr << "not"; break;
    case Op::NegInt: std::cerr << "neg-int"; break;
    case Op::NegReal: std::cerr << "neg-real"; break;
    case Op::ArrayLength: std::cerr << "array-length"; break;
    case Op::ArrayData: std::cerr << "array-data"; break;
    case Op::Ptr: std::cerr << "ptr"; break;
    case Op::LoadBool: std::cerr << "load-bool"; break;
    case Op::LoadChar: std::cerr << "load-char"; break;
    case Op::LoadInt: std::cerr << "load-int"; break;
    case Op::LoadReal: std::cerr << "load-real"; break;
    case Op::LoadType: std::cerr << "load-type"; break;
    case Op::LoadEnum: std::cerr << "load-enum"; break;
    case Op::LoadFlags: std::cerr << "load-flags"; break;
    case Op::LoadAddr: std::cerr << "load-addr"; break;
    case Op::PrintBool: std::cerr << "print-bool"; break;
    case Op::PrintChar: std::cerr << "print-char"; break;
    case Op::PrintInt: std::cerr << "print-int"; break;
    case Op::PrintReal: std::cerr << "print-real"; break;
    case Op::PrintType: std::cerr << "print-type"; break;
    case Op::PrintEnum: std::cerr << "print-enum"; break;
    case Op::PrintFlags: std::cerr << "print-flags"; break;
    case Op::PrintAddr: std::cerr << "print-addr"; break;
    case Op::PrintCharBuffer: std::cerr << "print-char-buffer"; break;
    case Op::AddInt: std::cerr << "add-int"; break;
    case Op::AddReal: std::cerr << "add-real"; break;
    case Op::AddCharBuf: std::cerr << "add-char-buf"; break;
    case Op::SubInt: std::cerr << "sub-int"; break;
    case Op::SubReal: std::cerr << "sub-real"; break;
    case Op::MulInt: std::cerr << "mul-int"; break;
    case Op::MulReal: std::cerr << "mul-real"; break;
    case Op::DivInt: std::cerr << "div-int"; break;
    case Op::DivReal: std::cerr << "div-real"; break;
    case Op::ModInt: std::cerr << "mod-int"; break;
    case Op::ModReal: std::cerr << "mod-real"; break;
    case Op::LtInt: std::cerr << "lt-int"; break;
    case Op::LtReal: std::cerr << "lt-real"; break;
    case Op::LtFlags: std::cerr << "lt-flags"; break;
    case Op::LeInt: std::cerr << "le-int"; break;
    case Op::LeReal: std::cerr << "le-real"; break;
    case Op::LeFlags: std::cerr << "le-flags"; break;
    case Op::GtInt: std::cerr << "gt-int"; break;
    case Op::GtReal: std::cerr << "gt-real"; break;
    case Op::GtFlags: std::cerr << "gt-flags"; break;
    case Op::GeInt: std::cerr << "ge-int"; break;
    case Op::GeReal: std::cerr << "ge-real"; break;
    case Op::GeFlags: std::cerr << "ge-flags"; break;
    case Op::EqBool: std::cerr << "eq-bool"; break;
    case Op::EqChar: std::cerr << "eq-char"; break;
    case Op::EqInt: std::cerr << "eq-int"; break;
    case Op::EqReal: std::cerr << "eq-real"; break;
    case Op::EqFlags: std::cerr << "eq-flags"; break;
    case Op::EqType: std::cerr << "eq-type"; break;
    case Op::EqAddr: std::cerr << "eq-addr"; break;
    case Op::NeBool: std::cerr << "ne-bool"; break;
    case Op::NeChar: std::cerr << "ne-char"; break;
    case Op::NeInt: std::cerr << "ne-int"; break;
    case Op::NeReal: std::cerr << "ne-real"; break;
    case Op::NeFlags: std::cerr << "ne-flags"; break;
    case Op::NeType: std::cerr << "ne-type"; break;
    case Op::NeAddr: std::cerr << "ne-addr"; break;
    case Op::XorBool: std::cerr << "xor-bool"; break;
    case Op::XorFlags: std::cerr << "xor-flags"; break;
    case Op::OrBool: std::cerr << "or-bool"; break;
    case Op::OrFlags: std::cerr << "or-flags"; break;
    case Op::AndBool: std::cerr << "and-bool"; break;
    case Op::AndFlags: std::cerr << "and-flags"; break;
    case Op::CreateStruct: std::cerr << "create-struct"; break;
    case Op::InsertField: std::cerr << "insert-field"; break;
    case Op::FinalizeStruct: std::cerr << "finalize-struct"; break;
    case Op::Malloc: std::cerr << "malloc"; break;
    case Op::Free: std::cerr << "free"; break;
    case Op::Alloca: std::cerr << "alloca"; break;
    case Op::Arrow: std::cerr << "arrow"; break;
    case Op::Array: std::cerr << "array-type"; break;
    case Op::Variant: std::cerr << "variant"; break;
    case Op::Tup: std::cerr << "tup"; break;
    case Op::VariantType: std::cerr << "variant-type"; break;
    case Op::VariantValue: std::cerr << "variant-value"; break;
    case Op::PtrIncr: std::cerr << "ptr-incr"; break;
    case Op::Field: std::cerr << "field"; break;
    case Op::CondJump: std::cerr << "cond"; break;
    case Op::UncondJump: std::cerr << "uncond"; break;
    case Op::ReturnJump: std::cerr << "return"; break;

    case Op::AddCodeBlock: std::cerr << "add-codeblock"; break;
    case Op::Store: std::cerr << "store"; break;
    case Op::SetReturn: std::cerr << "set-ret"; break;
    case Op::Phi: std::cerr << "phi"; break;
    case Op::Call: std::cerr << "call"; break;
    case Op::Contextualize: std::cerr << "contextualize"; break;
    case Op::Cast: std::cerr << "cast"; break;
    case Op::BlockSeq: std::cerr << "block-seq"; break;
    case Op::BlockSeqContains: std::cerr << "block-seq-contains"; break;
  }

  if (args.empty()) {
    std::cerr << std::endl;
    return;
  }
  std::cerr << ": " << args[0].to_string();
  for (size_t i = 1; i < args.size(); ++i) {
    std::cerr << ", " << args[i].to_string();
  }
  std::cerr << std::endl;
}

void BasicBlock::dump(size_t indent) const {
  for (const auto &cmd : cmds_) { cmd.dump(indent); }
}

void Func::dump() const {
  std::cerr << name() << ": " << type_->to_string();
  for (size_t i = 0; i < blocks_.size(); ++i) {
    std::cerr << "\n block #" << i << std::endl;
    blocks_[i].dump(2);
  }
}

// TODO this may not be necessary anymore? I can just make the phi later?
void Func::SetArgs(CmdIndex cmd_index, base::vector<Val> args) {
  auto &cmd = Command(cmd_index);
  ASSERT(cmd.op_code_ == Op::Phi);
  cmd.args = std::move(args);
}

}  // namespace IR
