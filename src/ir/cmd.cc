#include "ir.h"

#include "../type/type.h"
#include "property.h"
#include <cmath>
#include <iostream>

namespace IR {
BlockIndex Block::Current;
Func *Func::Current{nullptr};

template <bool IsPhi = false>
static void RecordReferences(Func *fn, const CmdIndex &ci,
                             const std::vector<Val> &args) {
  for (const auto &cmd_arg : args) {
    std::visit(
        base::overloaded{
            [fn, &ci](Register reg) {
              fn->references_[fn->reg_map_ AT(reg)].push_back(ci);
            },
            [fn, &ci](BlockIndex bi) {
              if constexpr (IsPhi) {
                size_t num_cmds    = fn->block(bi).cmds_.size();
                i32 index_on_block = static_cast<i32>(num_cmds) - 1;
                fn->references_[CmdIndex{bi, index_on_block}].push_back(ci);
              }
            },
            [](auto &&) {},
        },
        cmd_arg.value);
  }
}
Cmd::Cmd(Type *t, Op op, std::vector<Val> arg_vec)
    : args(std::move(arg_vec)), op_code_(op), type(t) {
  CmdIndex cmd_index{
      Block::Current,
      static_cast<i32>(Func::Current->block(Block::Current).cmds_.size())};
  result = Register(t != nullptr ? Func::Current->num_regs_++
                                 : -(++Func::Current->num_voids_));
  Func::Current->reg_map_[result] = cmd_index;
  RecordReferences(Func::Current, cmd_index, args);
}

Val Field(Val v, size_t n) {
  ASSERT_TYPE(Pointer, v.type);
  Type *result_type =
      Ptr(v.type->as<Pointer>().pointee->as<Struct>().field_type AT(n));
  Cmd cmd(result_type, Op::Field, std::vector{std::move(v), Val::Uint(n)});
  auto reg = cmd.reg();
  Func::Current->block(Block::Current).cmds_.push_back(std::move(cmd));
  return reg;
}

#define MAKE_VOID(op)                                                          \
  ASSERT_NE(Func::Current, nullptr);                                           \
  Cmd cmd(nullptr, op, {std::move(v)});                                        \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);

#define MAKE_VOID2(op)                                                         \
  ASSERT_NE(Func::Current, nullptr);                                           \
  Cmd cmd(nullptr, op, {std::move(v1), std::move(v2)});                        \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);

#define MAKE_AND_RETURN(type, op)                                              \
  ASSERT_NE(Func::Current, nullptr);                                           \
  Cmd cmd(type, op, {std::move(v)});                                           \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);                   \
  return cmd.reg()

#define MAKE_AND_RETURN2(type, op)                                             \
  Cmd cmd(type, op, {std::move(v1), std::move(v2)});                           \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);                   \
  return cmd.reg()

Val Malloc(Type *t, Val v) {
  ASSERT_EQ(v.type, ::Uint);
  MAKE_AND_RETURN(Ptr(t), Op::Malloc);
}

Val Extend(Val v) {
  if (char *c = std::get_if<char>(&v.value)) {
    return Val::Uint(static_cast<u64>(*c));
  }
  MAKE_AND_RETURN(Char, Op::Extend);
}

Val Trunc(Val v) {
  if (u64 *n = std::get_if<u64>(&v.value)) {
    return Val::Char(static_cast<char>(*n));
  }
  MAKE_AND_RETURN(Char, Op::Trunc);
}

Val Neg(Val v) {
  if (bool *b = std::get_if<bool>(&v.value)) { return Val::Bool(!*b); }
  if (i32 *n = std::get_if<i32>(&v.value)) { return Val::Int(-*n); }
  if (double *r = std::get_if<double>(&v.value)) { return Val::Real(-*r); }
  MAKE_AND_RETURN(v.type, Op::Neg);
}

void Print(Val v) { MAKE_VOID(Op::Print); }
void Free(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  MAKE_VOID(Op::Free);
}

Val Alloca(Type *t) {
  ASSERT_NE(t, ::Void);
  Cmd cmd(Ptr(t), Op::Alloca, {});
  Func::Current->block(Func::Current->entry()).cmds_.push_back(cmd);
  return cmd.reg();
}

Val Contextualize(base::owned_ptr<AST::CodeBlock> code,
                  std::vector<IR::Val> v) {
  ASSERT_NE(code, nullptr);
  v.push_back(IR::Val::CodeBlock(std::move(code)));
  MAKE_AND_RETURN(::Code, Op::Contextualize);
}

Val VariantType(Val v) { MAKE_AND_RETURN(Ptr(Type_), Op::VariantType); }
Val VariantValue(Type *t, Val v) { MAKE_AND_RETURN(Ptr(t), Op::VariantValue); }

Val Load(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  MAKE_AND_RETURN(v.type->as<Pointer>().pointee, Op::Load);
}

Val ArrayLength(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  auto *ptee = v.type->as<Pointer>().pointee;
  ASSERT_TYPE(::Array, ptee);
  ASSERT(!ptee->as<::Array>().fixed_length,
         "Pointee type is " + ptee->to_string());
  MAKE_AND_RETURN(Ptr(Uint), Op::ArrayLength);
}

Val ArrayData(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  auto *ptee = v.type->as<Pointer>().pointee;
  ASSERT_TYPE(::Array, ptee);
  auto *array_type = &ptee->as<::Array>();
  ASSERT(!array_type->fixed_length, "");
  MAKE_AND_RETURN(Ptr(Ptr(array_type->data_type)), Op::ArrayData);
}

void SetReturn(ReturnValue r, Val v2) {
  Val v1 = Val::Ret(r, v2.type);
  MAKE_VOID2(Op::SetReturn);
}

void Store(Val v1, Val v2) {
  if (auto *rv = std::get_if<ReturnValue>(&v2.value)) {
    SetReturn(*rv, v1);
  } else {
    ASSERT_TYPE(Pointer, v2.type);
    MAKE_VOID2(Op::Store);
  }
}

Val PtrIncr(Val v1, Val v2) {
  ASSERT_TYPE(Pointer, v1.type);
  ASSERT_EQ(v2.type, ::Uint);
  MAKE_AND_RETURN2(v1.type, Op::PtrIncr);
}

Val Ptr(Val v) {
  ASSERT_EQ(v.type, Type_);
  if (Type **t = std::get_if<Type *>(&v.value)) { return Val::Type(::Ptr(*t)); }
  MAKE_AND_RETURN(Type_, Op::Ptr);
}

Val Xor(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? Neg(v2) : v2; }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? Neg(v1) : v1; }
  MAKE_AND_RETURN2(Bool, Op::Xor);
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
  CONSTANT_PROPOGATION(i32, std::plus<i32>{}, Int);
  CONSTANT_PROPOGATION(u64, std::plus<u64>{}, Uint);
  CONSTANT_PROPOGATION(double, std::plus<double>{}, Real);
  CONSTANT_PROPOGATION(char, std::plus<char>{}, Char);

  if (base::owned_ptr<AST::CodeBlock> *cb1 =
          std::get_if<base::owned_ptr<AST::CodeBlock>>(&v1.value),
      *cb2 = std::get_if<base::owned_ptr<AST::CodeBlock>>(&v2.value);
      cb1 != nullptr && cb2 != nullptr) {
    auto block   = base::make_owned<AST::CodeBlock>();
    block->stmts = AST::Statements::Merge(
        std::vector{std::move((*cb1)->stmts), std::move((*cb2)->stmts)});
    return Val::CodeBlock(std::move(block));
  }

  if (EnumVal *e1 = std::get_if<EnumVal>(&v1.value),
      *e2         = std::get_if<EnumVal>(&v2.value);
      e1 != nullptr && e2 != nullptr) {
    return Val::Enum(&v1.type->as<Enum>(), e1->value + e2->value);
  }

  MAKE_AND_RETURN2(v1.type, Op::Add);
}

Val Sub(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::minus<i32>{}, Int);
  CONSTANT_PROPOGATION(u64, std::minus<u64>{}, Uint);
  CONSTANT_PROPOGATION(double, std::minus<double>{}, Real);
  CONSTANT_PROPOGATION(char, std::minus<char>{}, Char);
  MAKE_AND_RETURN2(v1.type, Op::Sub);
}

Val Mul(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::multiplies<i32>{}, Int);
  CONSTANT_PROPOGATION(u64, std::multiplies<u64>{}, Uint);
  CONSTANT_PROPOGATION(double, std::multiplies<double>{}, Real);
  MAKE_AND_RETURN2(v1.type, Op::Mul);
}

Val Div(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::divides<i32>{}, Int);
  CONSTANT_PROPOGATION(u64, std::divides<u64>{}, Uint);
  CONSTANT_PROPOGATION(double, std::divides<double>{}, Real);
  MAKE_AND_RETURN2(v1.type, Op::Div);
}

Val Mod(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::modulus<i32>{}, Int);
  CONSTANT_PROPOGATION(u64, std::modulus<u64>{}, Uint);
  CONSTANT_PROPOGATION(double, std::fmod, Real);
  MAKE_AND_RETURN2(v1.type, Op::Mod);
}

Val Arrow(Val v1, Val v2) {
  CONSTANT_PROPOGATION(Type *, ::Func, Type);
  MAKE_AND_RETURN2(Type_, Op::Arrow);
}

Val Variant(std::vector<Val> args) {
  Cmd cmd(Type_, Op::Variant, std::move(args));
  Func::Current->block(Block::Current).cmds_.push_back(std::move(cmd));
  return cmd.reg();
}

Val Array(Val v1, Val v2) {
  ASSERT(v1.type == nullptr || v1.type == Uint || v1.type == Int, "");
  ASSERT_EQ(v2.type, Type_);

  if (Type **t = std::get_if<Type *>(&v2.value)) {
    if (u64 *m = std::get_if<u64>(&v1.value)) {
      return Val::Type(::Arr(*t, *m));
    }
    if (i32 *n = std::get_if<i32>(&v1.value)) {
      return Val::Type(::Arr(*t, *n));
    }
    if (v1 == Val::None()) { return Val::Type(::Arr(*t)); }
  }

  // TODO decide if Int vs Uint is allowed
  MAKE_AND_RETURN2(Type_, Op::Array);
}

Val Index(Val v1, Val v2) {
  ASSERT_TYPE(Pointer, v1.type);
  ASSERT_EQ(v2.type, ::Uint);
  auto *array_type = &v1.type->as<Pointer>().pointee->as<::Array>();
  IR::Val ptr = array_type->fixed_length ? v1 : Load(ArrayData(v1));
  ptr.type    = Ptr(array_type->data_type);
  return PtrIncr(ptr, v2);
}

Val Lt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::less<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::less<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::less<double>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Lt);
}

Val Le(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::less_equal<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::less_equal<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::less_equal<double>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Le);
}

Val Gt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::greater<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::greater<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::greater<double>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Gt);
}

Val Ge(Val v1, Val v2) {
  CONSTANT_PROPOGATION(i32, std::greater_equal<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::greater_equal<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::greater_equal<double>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Ge);
}

Val Eq(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? v2 : Neg(v2); }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? v1 : Neg(v1); }

  CONSTANT_PROPOGATION(char, std::equal_to<char>{}, Bool);
  CONSTANT_PROPOGATION(i32, std::equal_to<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::equal_to<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::equal_to<double>{}, Bool);
  CONSTANT_PROPOGATION(Type *, std::equal_to<Type *>{}, Bool);
  CONSTANT_PROPOGATION(Addr, std::equal_to<Addr>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Eq);
}

Val Ne(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? Neg(v2) : v2; }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? Neg(v1) : v1; }

  CONSTANT_PROPOGATION(char, std::not_equal_to<char>{}, Bool);
  CONSTANT_PROPOGATION(i32, std::not_equal_to<i32>{}, Bool);
  CONSTANT_PROPOGATION(u64, std::not_equal_to<u64>{}, Bool);
  CONSTANT_PROPOGATION(double, std::not_equal_to<double>{}, Bool);
  CONSTANT_PROPOGATION(Type *, std::not_equal_to<Type *>{}, Bool);
  CONSTANT_PROPOGATION(Addr, std::not_equal_to<Addr>{}, Bool);
  MAKE_AND_RETURN2(::Bool, Op::Ne);
}
#undef CONSTANT_PROPOGATION

Val Cast(Val v1, Val v2) {
  // v1 = result_type, v2 = val
  ASSERT_EQ(v1.type, Type_);
  MAKE_AND_RETURN2(std::get<::Type *>(v1.value), Op::Cast);
}

#undef MAKE_AND_RETURN2
#undef MAKE_AND_RETURN
#undef MAKE_VOID2
#undef MAKE_VOID

CmdIndex Phi(Type *t) {
  CmdIndex cmd_index{
      Block::Current,
      static_cast<i32>(Func::Current->block(Block::Current).cmds_.size())};

  Cmd cmd(t, Op::Phi, {});
  Func::Current->block(Block::Current).cmds_.push_back(cmd);

  return cmd_index;
}

Val Call(Val fn, std::vector<Val> vals, std::vector<Val> result_locs) {
  ASSERT_TYPE(Function, fn.type);
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
  Type *output_type = Tup(fn.type->as<Function>().output)->is_big()
                          ? Void
                          : Tup(fn.type->as<Function>().output);
  Cmd cmd(output_type, Op::Call, std::move(vals));
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
  return cmd.reg();
}

void Cmd::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ');
  if (type != nullptr) { std::cerr << reg().to_string() << " = "; }
  switch (op_code_) {
  case Op::Malloc: std::cerr << "malloc"; break;
  case Op::Free: std::cerr << "free"; break;
  case Op::Extend: std::cerr << "extend"; break;
  case Op::Trunc: std::cerr << "trunc"; break;
  case Op::Neg: std::cerr << "neg"; break;
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
  case Op::Print: std::cerr << "print"; break;
  case Op::CondJump: std::cerr << "cond"; break;
  case Op::UncondJump: std::cerr << "uncond"; break;
  case Op::ReturnJump: std::cerr << "return"; break;
  case Op::Load: std::cerr << "load"; break;
  case Op::Store: std::cerr << "store"; break;
  case Op::Variant: std::cerr << "variant"; break;
  case Op::ArrayLength: std::cerr << "array-length"; break;
  case Op::ArrayData: std::cerr << "array-data"; break;
  case Op::PtrIncr: std::cerr << "ptr-incr"; break;
  case Op::Ptr: std::cerr << "ptr"; break;
  case Op::Phi: std::cerr << "phi"; break;
  case Op::Field: std::cerr << "field"; break;
  case Op::Nop: std::cerr << "nop"; break;
  case Op::Call: std::cerr << "call"; break;
  case Op::Cast: std::cerr << "cast"; break;
  case Op::SetReturn: std::cerr << "set-ret"; break;
  case Op::Arrow: std::cerr << "arrow"; break;
  case Op::Array: std::cerr << "array-type"; break;
  case Op::Alloca: std::cerr << "alloca"; break;
  case Op::Contextualize: std::cerr << "contextualize"; break;
  case Op::VariantType: std::cerr << "variant-type"; break;
  case Op::VariantValue: std::cerr << "variant-value"; break;
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
  ASSERT(Func::Current, "");
  Cmd cmd(nullptr, Op::CondJump,
          {cond, Val::Block(true_block), Val::Block(false_block)});
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
}
void UncondJump(BlockIndex block) {
  ASSERT(Func::Current, "");
  Cmd cmd(nullptr, Op::UncondJump, {Val::Block(block)});
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
}
void ReturnJump() {
  ASSERT(Func::Current, "");
  Cmd cmd(nullptr, Op::ReturnJump, {});
  Func::Current->return_blocks_.insert(Block::Current);
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
}

void Block::dump(size_t indent) const {
  for (const auto &cmd : cmds_) { cmd.dump(indent); }
}

void Func::dump() const {
  std::cerr << (name == "" ? "(anon)" : name) << ": " << *type_;
  for (size_t i = 0; i < blocks_.size(); ++i) {
    std::cerr << "\n block #" << i << std::endl;
    blocks_[i].dump(2);
  }
}

ExecContext::Frame::Frame(Func *fn, const std::vector<Val> &arguments)
    : fn_(fn), current_(fn_->entry()), prev_(fn_->entry()),
      regs_(fn->num_regs_, Val::None()) {
  size_t num_inputs = fn->ir_type->input.size();
  ASSERT_LE(num_inputs, arguments.size());
  ASSERT_LE(num_inputs, regs_.size());
  size_t i = 0;
  for (; i < num_inputs; ++i) { regs_[i] = arguments[i]; }
  for (; i < arguments.size(); ++i) { rets_.push_back(arguments[i]); }

  if (rets_.empty() && fn->type_->output.size() == 1) {
    // This is the case of a simple return type (i.e., type can be held in
    // register).
    rets_.push_back(IR::Val::None());
  }
}

// TODO this may not be necessary anymore? I can just make the phi later?
void Func::SetArgs(CmdIndex cmd_index, std::vector<Val> args) {
  auto &cmd = Command(cmd_index);
  ASSERT(cmd.op_code_ == Op::Phi, "");
  cmd.args = std::move(args);
  RecordReferences</* IsPhi = */ true>(Func::Current, cmd_index, cmd.args);
}

} // namespace IR
