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

Val Trunc(Val v) {
  if (i32 *n = std::get_if<i32>(&v.value)) {
    return Val::Char(static_cast<char>(*n));
  }

  auto &cmd  = MakeNewCmd(type::Char, Op::Trunc);
  cmd.trunc_ = Cmd::Trunc::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Extend(Val v) {
  if (char *c = std::get_if<char>(&v.value)) {
    return Val::Int(static_cast<i32>(*c));
  }

  auto &cmd   = MakeNewCmd(type::Int, Op::Extend);
  cmd.extend_ = Cmd::Extend::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val Bytes(Val v) {
  auto &cmd = MakeNewCmd(type::Int, Op::Bytes);
  if (Register *r = std::get_if<Register>(&v.value)) {
    cmd.bytes_ = Cmd::Bytes::Make(RegisterOr<const type::Type *>(*r));
  } else {
    cmd.bytes_ = Cmd::Bytes::Make(
        RegisterOr<const type::Type *>(std::get<const type::Type *>(v.value)));
  }
  return cmd.reg();
}

Val Align(Val v) {
  auto &cmd = MakeNewCmd(type::Int, Op::Align);
  if (Register *r = std::get_if<Register>(&v.value)) {
    cmd.align_ = Cmd::Align::Make(RegisterOr<const type::Type *>(*r));
  } else {
    cmd.align_ = Cmd::Align::Make(
        RegisterOr<const type::Type *>(std::get<const type::Type *>(v.value)));
  }
  return cmd.reg();
}

Val Not(Val v) {
  if (bool *b = std::get_if<bool>(&v.value)) { return Val::Bool(!*b); }
  auto &cmd = MakeNewCmd(type::Bool, Op::Not);
  cmd.not_  = Cmd::Not::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val NegInt(Val v) {
  if (i32 *n = std::get_if<i32>(&v.value)) { return Val::Int(-*n); }
  auto &cmd    = MakeNewCmd(type::Bool, Op::NegInt);
  cmd.neg_int_ = Cmd::NegInt::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val NegReal(Val v) {
  if (double *r = std::get_if<double>(&v.value)) { return Val::Real(-*r); }
  auto &cmd     = MakeNewCmd(type::Bool, Op::NegReal);
  cmd.neg_real_ = Cmd::NegReal::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val ArrayLength(Val v) {
  auto &cmd = MakeNewCmd(type::Ptr(type::Int), Op::ArrayLength);
  Register *r = std::get_if<Register>(&v.value);
  cmd.array_length_ = Cmd::ArrayLength::Make(
      r ? RegisterOr<IR::Addr>(*r)
        : RegisterOr<IR::Addr>(std::get<IR::Addr>(v.value)));
  return cmd.reg();
}

Val ArrayData(Val v) {
  ASSERT(v.type, Is<type::Pointer>());
  auto *ptee = v.type->as<type::Pointer>().pointee;
  ASSERT(ptee, Is<type::Array>());
  auto *array_type = &ptee->as<type::Array>();
  ASSERT(!array_type->fixed_length);

  auto &cmd       = MakeNewCmd(type::Ptr(array_type->data_type), Op::ArrayData);
  Register *r     = std::get_if<Register>(&v.value);
  cmd.array_data_ = Cmd::ArrayData::Make(
      r ? RegisterOr<IR::Addr>(*r)
        : RegisterOr<IR::Addr>(std::get<IR::Addr>(v.value)));
  return cmd.reg();
}

Val Ptr(Val v) {
  ASSERT(v.type == type::Type_);
  if (const type::Type **t = std::get_if<const type::Type *>(&v.value)) {
    return Val::Type(type::Ptr(*t));
  }

  auto &cmd = MakeNewCmd(type::Type_, Op::Ptr);
  cmd.ptr_  = Cmd::Ptr::Make(std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadBool(Val v) {
  auto &cmd      = MakeNewCmd(type::Bool, Op::LoadBool);
  Register *r    = std::get_if<Register>(&v.value);
  cmd.load_bool_ = Cmd::LoadBool::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadChar(Val v) {
  auto &cmd      = MakeNewCmd(type::Char, Op::LoadChar);
  Register *r    = std::get_if<Register>(&v.value);
  cmd.load_char_ = Cmd::LoadChar::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadInt(Val v) {
  auto &cmd     = MakeNewCmd(type::Int, Op::LoadInt);
  Register *r   = std::get_if<Register>(&v.value);
  cmd.load_int_ = Cmd::LoadInt::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadReal(Val v) {
  auto &cmd      = MakeNewCmd(type::Real, Op::LoadReal);
  Register *r    = std::get_if<Register>(&v.value);
  cmd.load_real_ = Cmd::LoadReal::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadType(Val v) {
  auto &cmd      = MakeNewCmd(type::Type_, Op::LoadType);
  Register *r    = std::get_if<Register>(&v.value);
  cmd.load_type_ = Cmd::LoadType::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadEnum(Val v) {
  auto &cmd   = MakeNewCmd(v.type->as<type::Pointer>().pointee, Op::LoadEnum);
  Register *r = std::get_if<Register>(&v.value);
  cmd.load_enum_ = Cmd::LoadEnum::Make(r ? *r : std::get<Register>(v.value));
  return cmd.reg();
}

Val LoadFlags(Val v) {
  auto &cmd   = MakeNewCmd(v.type->as<type::Pointer>().pointee, Op::LoadFlags);
  Register *r = std::get_if<Register>(&v.value);
  cmd.load_flags_ = Cmd::LoadFlags::Make(r ? *r : std::get<Register>(v.value));
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

Val Field(Val v, size_t n) {
  ASSERT(v.type, Is<type::Pointer>());
  const type::Type *result_type = type::Ptr(v.type->as<type::Pointer>()
                                                .pointee->as<type::Struct>()
                                                .fields_.at(n)
                                                .type);
  Cmd cmd(result_type, Op::Field,
          base::vector<IR::Val>{std::move(v), Val::Int(static_cast<i32>(n))});
  auto reg = cmd.reg();
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return reg;
}

static Val MakeCmd(const type::Type *t, Op op, base::vector<Val> vals) {
  auto &cmds = ASSERT_NOT_NULL(Func::Current)->block(BasicBlock::Current).cmds_;
  Cmd c{t, op, std::move(vals)};
  cmds.emplace_back(std::move(c));
  return t == nullptr ? IR::Val::None() : cmds.back().reg();
}

Val Malloc(const type::Type *t, Val v) {
  ASSERT(v.type == type::Int);
  return MakeCmd(type::Ptr(t), Op::Malloc, base::vector<IR::Val>{std::move(v)});
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

void Print(Val v) {
  MakeCmd(nullptr, Op::Print, base::vector<IR::Val>{std::move(v)});
}
void Free(Val v) {
  ASSERT(v.type, Is<type::Pointer>());
  MakeCmd(nullptr, Op::Free, base::vector<IR::Val>{std::move(v)});
}

Val CreateStruct() {
  Cmd cmd(type::Type_, Op::CreateStruct, {});
  ASSERT_NOT_NULL(Func::Current)->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}

IR::Val FinalizeStruct(Val v) {
  return MakeCmd(type::Type_, Op::FinalizeStruct,
                 base::vector<IR::Val>{std::move(v)});
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
  Cmd cmd(type::Ptr(t), Op::Alloca, {});
  Func::Current->block(Func::Current->entry()).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}

Val VariantType(Val v) {
  return MakeCmd(type::Ptr(type::Type_), Op::VariantType,
                 base::vector<IR::Val>{std::move(v)});
}
Val VariantValue(const type::Type *t, Val v) {
  return MakeCmd(type::Ptr(t), Op::VariantValue,
                 base::vector<IR::Val>{std::move(v)});
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

Val PtrIncr(Val v1, Val v2) {
  ASSERT(v1.type, Is<type::Pointer>());
  ASSERT(v2.type == type::Int);
  if (i32 *n = std::get_if<i32>(&v2.value)) {
    if (*n == 0) { return v1; }
  }
  return MakeCmd(v1.type, Op::PtrIncr,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Xor(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? Not(v2) : v2; }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? Not(v1) : v1; }
  if (FlagsVal *e1 = std::get_if<FlagsVal>(&v1.value),
      *e2          = std::get_if<FlagsVal>(&v2.value);
      e1 != nullptr && e2 != nullptr) {
    return Val::Flags(&v1.type->as<type::Flags>(), e1->value ^ e2->value);
  }
  return MakeCmd(v1.type, Op::Xor,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Or(Val v1, Val v2) {
  if (FlagsVal *e1 = std::get_if<FlagsVal>(&v1.value),
      *e2          = std::get_if<FlagsVal>(&v2.value);
      e1 != nullptr && e2 != nullptr) {
    return Val::Flags(&v1.type->as<type::Flags>(), e1->value | e2->value);
  }
  return MakeCmd(v1.type, Op::Or,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val And(Val v1, Val v2) {
  if (FlagsVal *e1 = std::get_if<FlagsVal>(&v1.value),
      *e2          = std::get_if<FlagsVal>(&v2.value);
      e1 != nullptr && e2 != nullptr) {
    return Val::Flags(&v1.type->as<type::Flags>(), e1->value & e2->value);
  }
  return MakeCmd(v1.type, Op::And,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

#define CONSTANT_PROPOGATION(cpp_type, fn, result_type)                        \
  do {                                                                         \
    cpp_type *val1 = std::get_if<cpp_type>(&v1.value);                         \
    cpp_type *val2 = std::get_if<cpp_type>(&v2.value);                         \
    if (val1 != nullptr && val2 != nullptr) {                                  \
      return Val::result_type(fn(*val1, *val2));                               \
    }                                                                          \
  } while (false)

Val Add(Val v1, Val v2) {
  if (auto *s1 = std::get_if<std::string_view>(&v1.value),
      *s2      = std::get_if<std::string_view>(&v2.value);
      s1 != nullptr && s2 != nullptr) {
    return IR::Val::CharBuf(std::string(*s1) + std::string(*s2));
  }

  CONSTANT_PROPOGATION(i32, std::plus<i32>{}, Int);
  CONSTANT_PROPOGATION(double, std::plus<double>{}, Real);
  CONSTANT_PROPOGATION(char, std::plus<char>{}, Char);

  if (auto *cb1 = std::get_if<AST::CodeBlock>(&v1.value),
      *cb2      = std::get_if<AST::CodeBlock>(&v2.value);
      cb1 != nullptr && cb2 != nullptr) {
    AST::CodeBlock block;
    // TODO is this std::get<Statements> call safe?
    block.content_ = AST::Statements::Merge(base::vector<AST::Statements>{
        {std::get<AST::Statements>(std::move(*cb1).content_),
         std::get<AST::Statements>(std::move(*cb2).content_)}});
    return Val::CodeBlock(std::move(block));
  }

  return MakeCmd(v1.type, Op::Add,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Sub(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::minus<i32>{}, Int);
  CONSTANT_PROPOGATION(double, std::minus<double>{}, Real);
  CONSTANT_PROPOGATION(char, std::minus<char>{}, Char);
  return MakeCmd(v1.type, Op::Sub,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Mul(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::multiplies<i32>{}, Int);
  CONSTANT_PROPOGATION(double, std::multiplies<double>{}, Real);
  return MakeCmd(v1.type, Op::Mul,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Div(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::divides<i32>{}, Int);
  CONSTANT_PROPOGATION(double, std::divides<double>{}, Real);
  return MakeCmd(v1.type, Op::Div,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Mod(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::modulus<i32>{}, Int);
  CONSTANT_PROPOGATION(double, std::fmod, Real);
  return MakeCmd(v1.type, Op::Mod,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Arrow(Val v1, Val v2) {
  CONSTANT_PROPOGATION(
      const type::Type *,
      [](const type::Type *lhs, const type::Type *rhs) {
        base::vector<const type::Type *> lhs_vec =
            (lhs->is<type::Tuple>()) ? lhs->as<type::Tuple>().entries_
                                     : base::vector<const type::Type *>{lhs};
        base::vector<const type::Type *> rhs_vec =
            (rhs->is<type::Tuple>()) ? rhs->as<type::Tuple>().entries_
                                     : base::vector<const type::Type *>{rhs};
        return type::Func(std::move(lhs_vec), std::move(rhs_vec));
      },
      Type);
  return MakeCmd(type::Type_, Op::Arrow,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Tup(base::vector<Val> args) {
  Cmd cmd(type::Type_, Op::Tup, std::move(args));
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}

Val Variant(base::vector<Val> args) {
  Cmd cmd(type::Type_, Op::Variant, std::move(args));
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}

Val Load(Val v) {
  if (v.type->as<type::Pointer>().pointee == type::Bool) {
    return LoadBool(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee == type::Char) {
    return LoadChar(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee == type::Int) {
    return LoadInt(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee == type::Real) {
    return LoadReal(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee == type::Type_) {
    return LoadType(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee->is<type::Enum>()) {
    return LoadEnum(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee->is<type::Flags>()) {
    return LoadFlags(std::move(v));
  } else if (v.type->as<type::Pointer>().pointee->is<type::Pointer>()) {
    NOT_YET(v.type);
  } else {
    UNREACHABLE(v.type);
  }
}

Val Array(Val v1, Val v2) {
  ASSERT(v2.type == type::Type_);

  if (const type::Type **t = std::get_if<const type::Type *>(&v2.value)) {
    if (i32 *m = std::get_if<i32>(&v1.value)) {
      return Val::Type(type::Arr(*t, *m));
    }
    if (i32 *n = std::get_if<i32>(&v1.value)) {
      return Val::Type(type::Arr(*t, *n));
    }
    if (v1 == Val::None()) { return Val::Type(type::Arr(*t)); }
  }

  return MakeCmd(type::Type_, Op::Array,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Index(Val v1, Val v2) {
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

Val Lt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::less<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::less<double>{}, Bool);
  CONSTANT_PROPOGATION(FlagsVal,
                       [](FlagsVal lhs, FlagsVal rhs) {
                         return lhs.value != rhs.value &&
                                ((lhs.value | rhs.value) == rhs.value);
                       },
                       Bool);
  return MakeCmd(type::Bool, Op::Lt,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Le(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::less_equal<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::less_equal<double>{}, Bool);
  CONSTANT_PROPOGATION(FlagsVal,
                       [](FlagsVal lhs, FlagsVal rhs) {
                         return (lhs.value | rhs.value) == rhs.value;
                       },
                       Bool);
  return MakeCmd(type::Bool, Op::Le,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Gt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::greater<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::greater<double>{}, Bool);
  CONSTANT_PROPOGATION(FlagsVal,
                       [](FlagsVal lhs, FlagsVal rhs) {
                         return lhs.value != rhs.value &&
                                ((lhs.value | rhs.value) == lhs.value);
                       },
                       Bool);
  return MakeCmd(type::Bool, Op::Gt,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Ge(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::greater_equal<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::greater_equal<double>{}, Bool);
  CONSTANT_PROPOGATION(FlagsVal,
                       [](FlagsVal lhs, FlagsVal rhs) {
                         return (lhs.value | rhs.value) == rhs.value;
                       },
                       Bool);
  return MakeCmd(type::Bool, Op::Ge,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val BlockSeqContains(Val v, AST::BlockLiteral *lit) {
  return MakeCmd(type::Bool, Op::BlockSeqContains,
                 base::vector<IR::Val>{std::move(v), IR::Val::Block(lit)});
}

Val Eq(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? v2 : Not(v2); }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? v1 : Not(v1); }

  CONSTANT_PROPOGATION(char, std::equal_to<char>{}, Bool);
  CONSTANT_PROPOGATION(i32, std::equal_to<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::equal_to<double>{}, Bool);
  CONSTANT_PROPOGATION(const type::Type *, std::equal_to<const type::Type *>{},
                       Bool);

  CONSTANT_PROPOGATION(Addr, std::equal_to<Addr>{}, Bool);
  CONSTANT_PROPOGATION(BlockSequence, std::equal_to<BlockSequence>{}, Bool);
  CONSTANT_PROPOGATION(
      FlagsVal,
      [](FlagsVal lhs, FlagsVal rhs) { return lhs.value == rhs.value; }, Bool);
  return MakeCmd(type::Bool, Op::Eq,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}

Val Ne(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? Not(v2) : v2; }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? Not(v1) : v1; }

  CONSTANT_PROPOGATION(char, std::not_equal_to<char>{}, Bool);
  CONSTANT_PROPOGATION(i32, std::not_equal_to<i32>{}, Bool);
  CONSTANT_PROPOGATION(double, std::not_equal_to<double>{}, Bool);
  CONSTANT_PROPOGATION(const type::Type *,
                       std::not_equal_to<const type::Type *>{}, Bool);
  CONSTANT_PROPOGATION(Addr, std::not_equal_to<Addr>{}, Bool);
  CONSTANT_PROPOGATION(
      FlagsVal,
      [](FlagsVal lhs, FlagsVal rhs) { return lhs.value != rhs.value; }, Bool);
  return MakeCmd(type::Bool, Op::Ne,
                 base::vector<IR::Val>{std::move(v1), std::move(v2)});
}
#undef CONSTANT_PROPOGATION

CmdIndex Phi(const type::Type *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};

  Cmd cmd(t, Op::Phi, {});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));

  return cmd_index;
}

Val Call(Val fn, base::vector<Val> vals, base::vector<Val> result_locs) {
  ASSERT(fn.type, Is<type::Function>());
  vals.insert(vals.end(), std::make_move_iterator(result_locs.begin()),
              std::make_move_iterator(result_locs.end()));
  vals.push_back(fn);

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
  Cmd cmd(output_type, Op::Call, std::move(vals));
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
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

    case Op::Load: std::cerr << "load"; break;
    case Op::Malloc: std::cerr << "malloc"; break;
    case Op::Free: std::cerr << "free"; break;
    case Op::Add: std::cerr << "add"; break;
    case Op::Sub: std::cerr << "sub"; break;
    case Op::Mul: std::cerr << "mul"; break;
    case Op::Div: std::cerr << "div"; break;
    case Op::Mod: std::cerr << "mod"; break;
    case Op::Lt: std::cerr << "lt"; break;
    case Op::Le: std::cerr << "le"; break;
    case Op::Eq: std::cerr << "eq"; break;
    case Op::Ne: std::cerr << "ne"; break;
    case Op::Ge: std::cerr << "ge"; break;
    case Op::Gt: std::cerr << "gt"; break;
    case Op::Xor: std::cerr << "xor"; break;
    case Op::Or: std::cerr << "or"; break;
    case Op::And: std::cerr << "and"; break;
    case Op::Print: std::cerr << "print"; break;
    case Op::CondJump: std::cerr << "cond"; break;
    case Op::UncondJump: std::cerr << "uncond"; break;
    case Op::ReturnJump: std::cerr << "return"; break;
    case Op::CreateStruct: std::cerr << "create-struct"; break;
    case Op::InsertField: std::cerr << "insert-field"; break;
    case Op::FinalizeStruct: std::cerr << "finalize-struct"; break;
    case Op::Store: std::cerr << "store"; break;
    case Op::SetReturn: std::cerr << "set-ret"; break;
    case Op::Variant: std::cerr << "variant"; break;
    case Op::Tup: std::cerr << "tup"; break;
    case Op::PtrIncr: std::cerr << "ptr-incr"; break;
    case Op::Phi: std::cerr << "phi"; break;
    case Op::Field: std::cerr << "field"; break;
    case Op::Call: std::cerr << "call"; break;
    case Op::Arrow: std::cerr << "arrow"; break;
    case Op::Array: std::cerr << "array-type"; break;
    case Op::Alloca: std::cerr << "alloca"; break;
    case Op::Contextualize: std::cerr << "contextualize"; break;
    case Op::VariantType: std::cerr << "variant-type"; break;
    case Op::VariantValue: std::cerr << "variant-value"; break;
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

void CondJump(Val cond, BlockIndex true_block, BlockIndex false_block) {
  ASSERT(Func::Current != nullptr);
  Cmd cmd(nullptr, Op::CondJump,
          {cond, Val::BasicBlock(true_block), Val::BasicBlock(false_block)});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
}
void UncondJump(BlockIndex block) {
  ASSERT(Func::Current != nullptr);
  Cmd cmd(nullptr, Op::UncondJump, {Val::BasicBlock(block)});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
}
void ReturnJump() {
  ASSERT(Func::Current != nullptr);
  Cmd cmd(nullptr, Op::ReturnJump, {});
  Func::Current->block(BasicBlock::Current).cmds_.push_back(std::move(cmd));
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
