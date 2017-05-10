#include "ir.h"

#include "../type/type.h"

namespace IR {
BlockIndex Block::Current;
Func *Func::Current;

Cmd::Cmd(Type *t, Op op, std::vector<Val> args)
    : args(std::move(args)), op_code(op) {
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  result = IR::Val::Reg(reg, t);
}

Val SetReturn(size_t n, Val v) {
  Cmd cmd(Void, Op::SetReturn, {IR::Val::Uint(n), std::move(v)});
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Field(Val v, size_t n) {
  ASSERT(v.type->is_pointer(), "");
  auto ptee_type = static_cast<Pointer *>(v.type)->pointee;
  ASSERT(ptee_type->is_struct(), "");

  Cmd cmd(Ptr(static_cast<Struct *>(ptee_type)->field_type[n]), Op::Field,
          {std::move(v), IR::Val::Uint(n)});
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

#define MAKE_AND_RETURN(type, op)                                              \
  Cmd cmd(type, op, {std::move(v)});                                           \
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);   \
  return cmd.result

#define MAKE_AND_RETURN2(type, op)                                             \
  Cmd cmd(type, op, {std::move(v1), std::move(v2)});                           \
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);   \
  return cmd.result

Val Malloc(Type *t, Val v) {
  ASSERT(v.type == ::Uint, "");
  MAKE_AND_RETURN(t, Op::Malloc);
}

Val Extend(Val v) { MAKE_AND_RETURN(Char, Op::Extend); }
Val Trunc(Val v) { MAKE_AND_RETURN(Char, Op::Trunc); }
Val Neg(Val v) { MAKE_AND_RETURN(v.type, Op::Neg); }
Val Print(Val v) { MAKE_AND_RETURN(Void, Op::Print); }
Val Free(Val v) {
  ASSERT(v.type->is_pointer(), "");
  MAKE_AND_RETURN(Void, Op::Free);
}

Val Alloca(Type *t) {
  ASSERT(t != ::Void, "");
  Cmd cmd(Ptr(t), Op::Alloca, {});
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Load(Val v) {
  ASSERT(v.type->is_pointer(), v.to_string());
  MAKE_AND_RETURN(static_cast<Pointer *>(v.type)->pointee, Op::Load);
}

Val ArrayLength(Val v) {
  ASSERT(v.type->is_array() && !static_cast<::Array *>(v.type)->fixed_length,
         "");
  MAKE_AND_RETURN(Ptr(Uint), Op::ArrayLength);
}

Val ArrayData(Val v) {
  ASSERT(v.type->is_array() && !static_cast<::Array *>(v.type)->fixed_length,
         "");
  MAKE_AND_RETURN(Ptr(static_cast<::Array *>(v.type)->data_type),
                  Op::ArrayData);
}

Val Store(Val v1, Val v2) {
  ASSERT(v2.type->is_pointer(), "");
  MAKE_AND_RETURN2(Void, Op::Store);
}

Val PtrIncr(Val v1, Val v2) {
  ASSERT(v1.type->is_pointer(), "");
  ASSERT(v2.type == ::Uint, "");
  MAKE_AND_RETURN2(v1.type, Op::PtrIncr);
}

Val And(Val v1, Val v2) { MAKE_AND_RETURN2(Bool, Op::And); }
Val Or(Val v1, Val v2) { MAKE_AND_RETURN2(Bool, Op::Or); }
Val Xor(Val v1, Val v2) { MAKE_AND_RETURN2(Bool, Op::Xor); }

Val Add(Val v1, Val v2) { MAKE_AND_RETURN2(v1.type, Op::Add); }
Val Sub(Val v1, Val v2) { MAKE_AND_RETURN2(v1.type, Op::Sub); }
Val Mul(Val v1, Val v2) { MAKE_AND_RETURN2(v1.type, Op::Mul); }
Val Div(Val v1, Val v2) { MAKE_AND_RETURN2(v1.type, Op::Div); }
Val Mod(Val v1, Val v2) { MAKE_AND_RETURN2(v1.type, Op::Mod); }
Val Arrow(Val v1, Val v2) { MAKE_AND_RETURN2(Type_, Op::Arrow); }

Val Array(Val v1, Val v2) {
  // TODO decide if Int vs Uint is allowed
  ASSERT((v1.type == Uint || v1.type == Int) && v2.type == Type_, "");
  MAKE_AND_RETURN2(Type_, Op::Array);
}

Val Access(Val v1, Val v2) {
  // v1 = index, v2 = val
  ASSERT(v2.type->is_array(), "");
  MAKE_AND_RETURN2(Ptr(static_cast<::Array *>(v2.type)->data_type), Op::Access);
}

Val Lt(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Lt); }
Val Le(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Le); }
Val Eq(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Eq); }
Val Ne(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Ne); }
Val Ge(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Ge); }
Val Gt(Val v1, Val v2) { MAKE_AND_RETURN2(::Bool, Op::Gt); }

Val Cast(Val v1, Val v2) {
  // v1 = result_type, v2 = val
  ASSERT(v1.type == Type_, "");
  MAKE_AND_RETURN2(v1.as_type, Op::Cast);
}

#undef MAKE_AND_RETURN2
#undef MAKE_AND_RETURN

Val Phi(Type *t) {
  Cmd cmd(t, Op::Phi, {});
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Call(Val fn, std::vector<Val> vals) {
  ASSERT(fn.type->is_function(), "");
  vals.push_back(fn);
  Cmd cmd(static_cast<Function *>(fn.type)->output, Op::Call, std::move(vals));
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

void Cmd::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ') << result.to_string() << " = ";
  switch (op_code) {
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
    case Op::And: std::cerr << "and"; break;
    case Op::Or: std::cerr << "or"; break;
    case Op::Xor: std::cerr << "xor"; break;
    case Op::Print: std::cerr << "print"; break;
    case Op::Load: std::cerr << "load"; break;
    case Op::Store: std::cerr << "store"; break;
    case Op::ArrayLength: std::cerr << "array_length"; break;
    case Op::ArrayData: std::cerr << "array_data"; break;
    case Op::PtrIncr: std::cerr << "ptr_incr"; break;
    case Op::Phi: std::cerr << "phi"; break;
    case Op::Field: std::cerr << "field"; break;
    case Op::Access: std::cerr << "access"; break;
    case Op::Nop: std::cerr << "nop"; break;
    case Op::Call: std::cerr << "call"; break;
    case Op::Cast: std::cerr << "cast"; break;
    case Op::SetReturn: std::cerr << "set-ret"; break;
    case Op::Arrow: std::cerr << "arrow"; break;
    case Op::Array: std::cerr << "array-type"; break;
    case Op::Alloca: std::cerr << "alloca"; break;
  }

  if (args.empty()) { return; }
  std::cerr << ": " << args[0].to_string();
  for (size_t i = 1; i < args.size(); ++i) {
    std::cerr << ", " << args[i].to_string();
  }
  std::cerr << std::endl;
}

void Block::dump(size_t indent) const {
  for (const auto& cmd : cmds_) { cmd.dump(indent); }
  jmp_.dump(indent);
}

void Jump::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ');
  switch (type) {
  case Type::Uncond:
    std::cerr << "jmp #" << block_index.value << std::endl;
    break;
  case Type::Cond:
    std::cerr << "cond " << cond_data.cond.to_string() << std::endl
              << "T => #" << cond_data.true_block.value << "F => #"
              << cond_data.false_block.value << std::endl;
    break;
  case Type::Ret:
    std::cerr << "return." << std::endl;
    break;
  }
}

void Func::dump() const {
  std::cerr << (name == "" ? "(anon)" : name) << ": " << *type;
  for (size_t i = 0; i < blocks_.size(); ++i) {
    std::cerr << "\n block #" << i << std::endl;
    blocks_[i].dump(2);
  }
}

} // namespace IR
