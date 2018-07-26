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

  auto &cmd  = MakeCmd(type::Type_, Op::Array);
  cmd.array_ = Cmd::Array::Make(
      m ? RegisterOr<i32>(*m)
        : v1 == Val::None() ? RegisterOr<i32>(-1)
                            : RegisterOr<i32>(std::get<Register>(v1.value)),
      t ? RegisterOr<type::Type const *>(*t)
        : RegisterOr<type::Type const *>(std::get<Register>(v2.value)));
  return cmd.reg();
}

Val Tup(base::vector<Val> vals) {
  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(std::move(vals)));
  auto &cmd = MakeCmd(type::Type_, Op::Tup);
  cmd.tup_  = Cmd::Tup{{}, args.get()};
  return cmd.reg();
}

Val Variant(base::vector<Val> vals) {
  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(std::move(vals)));
  auto &cmd    = MakeCmd(type::Type_, Op::Variant);
  cmd.variant_ = Cmd::Variant{{}, args.get()};
  return cmd.reg();
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

Val OrBool(const Val &v1, const Val &v2) {
  bool const *x1 = std::get_if<bool>(&v1.value);
  bool const *x2 = std::get_if<bool>(&v2.value);
  if (x1) { return *x1 ? IR::Val::Bool(true) : v2; }
  if (x2) { return *x2 ? IR::Val::Bool(true) : v1; }
  auto &cmd = MakeCmd(type::Bool, Op::OrBool);
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
  auto &cmd = MakeCmd(type::Bool, Op::AndBool);
  cmd.and_bool_ =
      Cmd::AndBool::Make(x1 ? RegisterOr<bool>(*x1)
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

Cmd::Cmd(const type::Type *t, Op op) : op_code_(op), type(t) {
  ASSERT(Func::Current != nullptr);
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  if (t == nullptr) {
    result = Register{-++Func::Current->num_voids_};
    Func::Current->reg_to_cmd_.emplace(result, cmd_index);
    return;
  }

  auto arch    = Architecture::InterprettingMachine();
  auto reg_val = arch.MoveForwardToAlignment(t, Func::Current->reg_size_);
  result       = Register(reg_val);
  Func::Current->reg_size_ = reg_val + arch.bytes(t);
  ++Func::Current->num_regs_;

  Func::Current->reg_to_cmd_.emplace(result, cmd_index);


  Func::Current->references_[result];  // Make sure this entry exists
  // TODO references_
}

extern Val MakeBlockSeq(const base::vector<Val> &blocks);

Val BlockSeq(const base::vector<Val> &blocks) {
  if (std::all_of(blocks.begin(), blocks.end(), [](const IR::Val &v) {
        return std::holds_alternative<IR::BlockSequence>(v.value);
      })) {
    return MakeBlockSeq(blocks);
  }
  auto &cmd  = MakeCmd(blocks.back().type, Op::BlockSeq);
  auto &args = Func::Current->block(BasicBlock::Current)
                   .call_args_.emplace_back(
                       std::make_unique<base::vector<IR::Val>>(blocks));
  cmd.block_seq_ = Cmd::BlockSeq{{}, args.get()};
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
    auto alloc = Alloca(to);

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

Val CreateStruct() {
  return MakeCmd(type::Type_, Op::CreateStruct).reg();
}

IR::Val FinalizeStruct(const Val &v) {
  auto &cmd            = MakeCmd(type::Type_, Op::CreateStruct);
  cmd.finalize_struct_ = Cmd::FinalizeStruct::Make(std::get<Register>(v.value));
  return cmd.reg();
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

void InsertField(Val struct_type, std::string field_name, Val type,
                 Val init_val) {
  auto &cmd = MakeCmd(nullptr, Op::InsertField);
  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(
              std::make_unique<base::vector<IR::Val>>(base::vector<Val>{
                  std::move(struct_type), Val::CharBuf(field_name),
                  std::move(type), std::move(init_val)}));
  cmd.insert_field_.args_ = args.get();
}

Val Alloca(const type::Type *t) {
  ASSERT(t, Not(Is<type::Tuple>()));
  return ASSERT_NOT_NULL(Func::Current)
      ->block(Func::Current->entry())
      .cmds_.emplace_back(type::Ptr(t), Op::Alloca)
      .reg();
}

void SetReturn(size_t n, Val v2) {
  // TODO this is hacky
  n += IR::Func::Current->type_->input.size();
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

template <typename T>
static RegisterOr<T> GetRegOr(const Val &v) {
  auto *x = std::get_if<T>(&v.value);
  if (x) { return RegisterOr<T>(*x); }
  return RegisterOr<T>(std::get<Register>(v.value));
}

void StoreBool(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreBool);
  cmd.store_bool_ =
      Cmd::StoreBool::Make(std::get<Register>(loc.value), GetRegOr<bool>(val));
}

void StoreChar(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreChar);
  cmd.store_char_ =
      Cmd::StoreChar::Make(std::get<Register>(loc.value), GetRegOr<char>(val));
}

void StoreInt(const Val &val, const Val &loc) {
  auto &cmd = MakeCmd(nullptr, Op::StoreInt);
  cmd.store_int_ =
      Cmd::StoreInt::Make(std::get<Register>(loc.value), GetRegOr<i32>(val));
}

void StoreReal(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreReal);
  cmd.store_real_ = Cmd::StoreReal::Make(std::get<Register>(loc.value),
                                         GetRegOr<double>(val));
}

void StoreType(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreType);
  cmd.store_type_ = Cmd::StoreType::Make(std::get<Register>(loc.value),
                                         GetRegOr<type::Type const *>(val));
}

void StoreEnum(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreEnum);
  cmd.store_enum_ = Cmd::StoreEnum::Make(std::get<Register>(loc.value),
                                         GetRegOr<EnumVal>(val));
}

void StoreFlags(const Val &val, const Val &loc) {
  auto &cmd        = MakeCmd(nullptr, Op::StoreFlags);
  cmd.store_flags_ = Cmd::StoreFlags::Make(std::get<Register>(loc.value),
                                           GetRegOr<FlagsVal>(val));
}

void StoreAddr(const Val &val, const Val &loc) {
  auto &cmd       = MakeCmd(nullptr, Op::StoreAddr);
  cmd.store_addr_ = Cmd::StoreAddr::Make(std::get<Register>(loc.value),
                                         GetRegOr<IR::Addr>(val));
}

void SetReturnBool(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnBool);
  cmd.set_return_bool_ =
      Cmd::SetReturnBool::Make(Register(n), GetRegOr<bool>(v2));
}

void SetReturnChar(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnChar);
  cmd.set_return_char_ =
      Cmd::SetReturnChar::Make(Register(n), GetRegOr<char>(v2));
}

void SetReturnInt(size_t n, const Val &v2) {
  auto &cmd           = MakeCmd(nullptr, Op::SetReturnInt);
  cmd.set_return_int_ = Cmd::SetReturnInt::Make(Register(n), GetRegOr<i32>(v2));
}

void SetReturnCharBuf(size_t n, const Val &v2) {
  auto &cmd           = MakeCmd(nullptr, Op::SetReturnCharBuf);
  cmd.set_return_char_buf_ =
      Cmd::SetReturnCharBuf::Make(Register(n), GetRegOr<std::string_view>(v2));
}

void SetReturnReal(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnReal);
  cmd.set_return_real_ =
      Cmd::SetReturnReal::Make(Register(n), GetRegOr<double>(v2));
}

void SetReturnType(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnType);
  cmd.set_return_type_ =
      Cmd::SetReturnType::Make(Register(n), GetRegOr<type::Type const *>(v2));
}

void SetReturnEnum(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnEnum);
  cmd.set_return_enum_ =
      Cmd::SetReturnEnum::Make(Register(n), GetRegOr<EnumVal>(v2));
}

void SetReturnFlags(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnFlags);
  cmd.set_return_flags_ =
      Cmd::SetReturnFlags::Make(Register(n), GetRegOr<FlagsVal>(v2));
}

void SetReturnAddr(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnAddr);
  cmd.set_return_addr_ =
      Cmd::SetReturnAddr::Make(Register(n), GetRegOr<IR::Addr>(v2));
}

void SetReturnFunc(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnFunc);
  cmd.set_return_func_ =
      Cmd::SetReturnFunc::Make(Register(n), GetRegOr<IR::Func *>(v2));
}

void SetReturnScope(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnScope);
  cmd.set_return_scope_ =
      Cmd::SetReturnScope::Make(Register(n), GetRegOr<AST::ScopeLiteral *>(v2));
}

void SetReturnModule(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnModule);
  cmd.set_return_module_ =
      Cmd::SetReturnModule::Make(Register(n), GetRegOr<Module const *>(v2));
}

void SetReturnGeneric(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnGeneric);
  cmd.set_return_generic_ =
      Cmd::SetReturnGeneric::Make(Register(n), GetRegOr<AST::Function *>(v2));
}

void SetReturnBlock(size_t n, const Val &v2) {
  auto &cmd = MakeCmd(nullptr, Op::SetReturnBlock);
  cmd.set_return_block_ =
      Cmd::SetReturnBlock::Make(Register(n), GetRegOr<BlockSequence>(v2));
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
  UNREACHABLE(v.type);
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
  if (const bool *b = std::get_if<bool>(&v1.value)) {
    return *b ? Not(v2) : v2;
  }
  if (const bool *b = std::get_if<bool>(&v2.value)) {
    return *b ? Not(v1) : v1;
  }

  if (v1.type == type::Char) { return NeChar(v1, v2); }
  if (v1.type == type::Int) { return NeInt(v1, v2); }
  if (v1.type == type::Real) { return NeReal(v1, v2); }
  if (v1.type == type::Type_) { return NeType(v1, v2); }
  if (v1.type->is<type::Pointer>()) { return NeAddr(v1, v2); }
  UNREACHABLE();
}

std::pair<CmdIndex, base::vector<IR::Val> *> Phi(const type::Type *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};

  auto &args =
      Func::Current->block(BasicBlock::Current)
          .call_args_.emplace_back(std::make_unique<base::vector<IR::Val>>());
  auto &cmd = MakeCmd(t, Op::Phi);
  cmd.phi_  = Cmd::Phi::Make(args.get());
  return std::pair(cmd_index, args.get());
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
  auto &cmd = MakeCmd(output_type, Op::Call);

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

void Cmd::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ');
  if (this->type != nullptr) { std::cerr << result << " = "; }
  switch (op_code_) {
    case Op::Trunc: std::cerr << "trunc " << trunc_.reg_; break;
    case Op::Extend: std::cerr << "extend " << extend_.reg_; break;
    case Op::Bytes: std::cerr << "bytes " << bytes_.arg_; break;
    case Op::Align: std::cerr << "align " << align_.arg_; break;
    case Op::Not: std::cerr << "not " << not_.reg_; break;
    case Op::NegInt: std::cerr << "neg-int " << neg_int_.reg_; break;
    case Op::NegReal: std::cerr << "neg-real " << neg_real_.reg_; break;
    case Op::ArrayLength:
      std::cerr << "array-length " << array_length_.arg_;
      break;
    case Op::ArrayData: std::cerr << "array-data " << array_data_.arg_; break;
    case Op::Ptr: std::cerr << "ptr " << ptr_.reg_; break;
    case Op::LoadBool: std::cerr << "load-bool " << load_bool_.arg_; break;
    case Op::LoadChar: std::cerr << "load-char " << load_char_.arg_; break;
    case Op::LoadInt: std::cerr << "load-int " << load_int_.arg_; break;
    case Op::LoadReal: std::cerr << "load-real " << load_real_.arg_; break;
    case Op::LoadType: std::cerr << "load-type " << load_type_.arg_; break;
    case Op::LoadEnum: std::cerr << "load-enum " << load_enum_.arg_; break;
    case Op::LoadFlags: std::cerr << "load-flags " << load_type_.arg_; break;
    case Op::LoadAddr: std::cerr << "load-addr " << load_addr_.arg_; break;
    case Op::PrintBool: std::cerr << "print-bool " << print_bool_.arg_; break;
    case Op::PrintChar: std::cerr << "print-char " << print_char_.arg_; break;
    case Op::PrintInt: std::cerr << "print-int " << print_int_.arg_; break;
    case Op::PrintReal: std::cerr << "print-real " << print_real_.arg_; break;
    case Op::PrintType: std::cerr << "print-type " << print_type_.arg_; break;
    case Op::PrintEnum: std::cerr << "print-enum " << print_enum_.arg_; break;
    case Op::PrintFlags:
      std::cerr << "print-flags " << print_flags_.arg_;
      break;
    case Op::PrintAddr: std::cerr << "print-addr" << print_addr_.arg_; break;
    case Op::PrintCharBuffer:
      std::cerr << "print-char-buffer" << print_char_buffer_.arg_;
      break;
    case Op::AddInt: std::cerr << "add-int " << add_int_.args_; break;
    case Op::AddReal: std::cerr << "add-real " << add_real_.args_; break;
    case Op::AddCharBuf:
      std::cerr << "add-char-buf " << add_char_buf_.args_;
      break;
    case Op::SubInt: std::cerr << "sub-int " << sub_int_.args_; break;
    case Op::SubReal: std::cerr << "sub-real " << sub_real_.args_; break;
    case Op::MulInt: std::cerr << "mul-int " << mul_int_.args_; break;
    case Op::MulReal: std::cerr << "mul-real " << mul_real_.args_; break;
    case Op::DivInt: std::cerr << "div-int " << div_int_.args_; break;
    case Op::DivReal: std::cerr << "div-real " << div_real_.args_; break;
    case Op::ModInt: std::cerr << "mod-int " << mod_int_.args_; break;
    case Op::ModReal: std::cerr << "mod-real " << mod_real_.args_; break;
    case Op::LtInt: std::cerr << "lt-int " << lt_int_.args_; break;
    case Op::LtReal: std::cerr << "lt-real " << lt_real_.args_; break;
    case Op::LtFlags: std::cerr << "lt-flags " << lt_flags_.args_; break;
    case Op::LeInt: std::cerr << "le-int " << le_int_.args_; break;
    case Op::LeReal: std::cerr << "le-real " << le_real_.args_; break;
    case Op::LeFlags: std::cerr << "le-flags " << le_flags_.args_; break;
    case Op::GtInt: std::cerr << "gt-int " << gt_int_.args_; break;
    case Op::GtReal: std::cerr << "gt-real " << gt_real_.args_; break;
    case Op::GtFlags: std::cerr << "gt-flags " << gt_flags_.args_; break;
    case Op::GeInt: std::cerr << "ge-int " << ge_int_.args_; break;
    case Op::GeReal: std::cerr << "ge-real " << ge_real_.args_; break;
    case Op::GeFlags: std::cerr << "ge-flags " << ge_flags_.args_; break;
    case Op::EqBool: std::cerr << "eq-bool " << eq_bool_.args_; break;
    case Op::EqChar: std::cerr << "eq-char " << eq_char_.args_; break;
    case Op::EqInt: std::cerr << "eq-int " << eq_int_.args_; break;
    case Op::EqReal: std::cerr << "eq-real " << eq_real_.args_; break;
    case Op::EqFlags: std::cerr << "eq-flags " << eq_flags_.args_; break;
    case Op::EqType: std::cerr << "eq-type " << eq_type_.args_; break;
    case Op::EqAddr: std::cerr << "eq-addr " << eq_addr_.args_; break;
    case Op::NeBool: std::cerr << "ne-bool " << ne_bool_.args_; break;
    case Op::NeChar: std::cerr << "ne-char " << ne_char_.args_; break;
    case Op::NeInt: std::cerr << "ne-int " << ne_int_.args_; break;
    case Op::NeReal: std::cerr << "ne-real " << ne_real_.args_; break;
    case Op::NeFlags: std::cerr << "ne-flags " << ne_flags_.args_; break;
    case Op::NeType: std::cerr << "ne-type " << ne_type_.args_; break;
    case Op::NeAddr: std::cerr << "ne-addr " << ne_addr_.args_; break;
    case Op::XorBool: std::cerr << "xor-bool " << xor_bool_.args_; break;
    case Op::XorFlags: std::cerr << "xor-flags " << xor_flags_.args_; break;
    case Op::OrBool: std::cerr << "or-bool " << or_bool_.args_; break;
    case Op::OrFlags: std::cerr << "or-flags " << or_flags_.args_; break;
    case Op::AndBool: std::cerr << "and-bool " << and_bool_.args_; break;
    case Op::AndFlags: std::cerr << "and-flags " << and_flags_.args_; break;
    case Op::CreateStruct: std::cerr << "create-struct "; break;
    case Op::InsertField:
      std::cerr << "insert-field " << insert_field_.args_;
      break;
    case Op::FinalizeStruct:
      std::cerr << "finalize-struct " << finalize_struct_.reg_;
      break;
    case Op::Malloc: std::cerr << "malloc " << malloc_.arg_; break;
    case Op::Free: std::cerr << "free " << free_.reg_; break;
    case Op::Alloca: std::cerr << "alloca " << this->type->to_string(); break;
    case Op::Arrow: std::cerr << "arrow " << arrow_.args_; break;
    case Op::Array:
      std::cerr << "array " << array_.len_ << " " << array_.type_;
      break;
    case Op::Variant:
      std::cerr << "variant";
      for (const auto &arg : *variant_.args_) {
        std::cerr << " " << arg.to_string();
      }
      break;
    case Op::Tup:
      std::cerr << "tup";
      for (const auto &arg : *tup_.args_) {
        std::cerr << " " << arg.to_string();
      }
      break;
    case Op::VariantType:
      std::cerr << "variant-type " << variant_type_.reg_;
      break;
    case Op::VariantValue:
      std::cerr << "variant-value " << variant_value_.reg_;
      break;
    case Op::PtrIncr:
      std::cerr << "ptr-incr " << ptr_incr_.ptr_ << " " << ptr_incr_.incr_;
      break;
    case Op::Field:
      std::cerr << "field " << field_.ptr_ << " " << field_.num_;
      break;
    case Op::CondJump:
      std::cerr << "cond " << cond_jump_.cond_ << " " << cond_jump_.blocks_[0]
                << " " << cond_jump_.blocks_[1];
      break;
    case Op::UncondJump: std::cerr << "uncond " << uncond_jump_.block_; break;
    case Op::ReturnJump: std::cerr << "return"; break;
    case Op::Call:
      std::cerr << "call";
      for (const auto &arg : *call_.args_) {
        std::cerr << " " << arg.to_string();
      };
      break;
    case Op::BlockSeq:
      std::cerr << "block-seq";
      for (const auto &arg : *block_seq_.args_) {
        std::cerr << " " << arg.to_string();
      };
      break;
    case Op::BlockSeqContains:
      std::cerr << "block-seq-contains " << block_seq_contains_.reg_ << " "
                << block_seq_contains_.lit_;
      break;
    case Op::CastIntToReal:
      std::cerr << "cast-int-to-real " << cast_int_to_real_.reg_;
      break;
    case Op::CastPtr:
      std::cerr << "cast-ptr " << cast_ptr_.reg_ << " " << cast_ptr_.type_;
      break;

    case Op::AddCodeBlock: std::cerr << "add-codeblock"; break;
    case Op::Contextualize: std::cerr << "contextualize"; break;

    case Op::StoreBool:
      std::cerr << "store-bool " << store_bool_.addr_ << " "
                << store_bool_.val_;
      break;
    case Op::StoreChar:
      std::cerr << "store-char " << store_char_.addr_ << " "
                << store_char_.val_;
      break;
    case Op::StoreInt:
      std::cerr << "store-int " << store_int_.addr_ << " " << store_int_.val_;
      break;
    case Op::StoreReal:
      std::cerr << "store-real " << store_real_.addr_ << " "
                << store_real_.val_;
      break;
    case Op::StoreType:
      std::cerr << "store-type " << store_type_.addr_ << " "
                << store_type_.val_;
      break;
    case Op::StoreEnum:
      std::cerr << "store-enum " << store_enum_.addr_ << " "
                << store_enum_.val_;
      break;
    case Op::StoreFlags:
      std::cerr << "store-flags " << store_flags_.addr_ << " "
                << store_flags_.val_;
      break;
    case Op::StoreAddr:
      std::cerr << "store-addr " << store_addr_.addr_ << " "
                << store_addr_.val_;
      break;

    case Op::SetReturnBool: std::cerr << "set-ret-bool"; break;
    case Op::SetReturnChar: std::cerr << "set-ret-char"; break;
    case Op::SetReturnInt:
      std::cerr << "set-ret-int " << set_return_int_.reg_ << " "
                << set_return_int_.val_;
      break;
    case Op::SetReturnReal: std::cerr << "set-ret-real"; break;
    case Op::SetReturnType: std::cerr << "set-ret-type"; break;
    case Op::SetReturnEnum: std::cerr << "set-ret-enum"; break;
    case Op::SetReturnFlags: std::cerr << "set-ret-flags"; break;
    case Op::SetReturnCharBuf: std::cerr << "set-ret-char-buf"; break;
    case Op::SetReturnAddr: std::cerr << "set-ret-addr"; break;
    case Op::SetReturnFunc: std::cerr << "set-ret-func"; break;
    case Op::SetReturnScope: std::cerr << "set-ret-scope"; break;
    case Op::SetReturnModule: std::cerr << "set-ret-module"; break;
    case Op::SetReturnGeneric: std::cerr << "set-ret-generic"; break;
    case Op::SetReturnBlock: std::cerr << "set-ret-block"; break;
    case Op::Phi:
      std::cerr << "phi";
      for (const auto &arg : *phi_.args_) {
        std::cerr << " " << arg.to_string();
      };
      break;
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
}  // namespace IR
