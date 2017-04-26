#include "ir.h"

#include "../type/type.h"

namespace IR {
Val Malloc(Type *t, Val v) {
  ASSERT(v.type == ::Uint, "");
  Cmd cmd;
  cmd.result.type = t;
  cmd.op_code = Op::Malloc;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index = IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Free(Val v) {
  ASSERT(v.type->is_pointer(), "");
  Cmd cmd;
  cmd.result.type = Void;
  cmd.op_code = Op::Free;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Field(Val v, size_t n) {
  ASSERT(v.type->is_pointer(), "");
  auto ptee_type = static_cast<Pointer *>(v.type)->pointee;
  ASSERT(ptee_type->is_struct(), "");

  Cmd cmd;
  cmd.result.type = Ptr(static_cast<Struct *>(ptee_type)->field_type[n]);
  cmd.op_code = Op::Field;
  cmd.args = {std::move(v), IR::Val::Uint(n)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Neg(Val v) {
  Cmd cmd;
  cmd.result.type = v.type;
  cmd.op_code = Op::Neg;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Print(Val v) {
  Cmd cmd;
  cmd.result.type = Void;
  cmd.op_code = Op::Print;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Load(Val v) {
  ASSERT(v.type->is_pointer(), "");
  Cmd cmd;
  cmd.result.type = static_cast<Pointer *>(v.type)->pointee;
  cmd.op_code = Op::Load;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Store(Val val, Val loc) {
  ASSERT(loc.type->is_pointer(), "");
  Cmd cmd;
  cmd.result.type = static_cast<Pointer *>(val.type)->pointee;
  cmd.op_code = Op::Store;
  cmd.args = {std::move(val), std::move(loc)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val ArrayLength(Val v) {
  ASSERT(v.type->is_array() && !static_cast<Array *>(v.type)->fixed_length, "");
  Cmd cmd;
  cmd.result.type = Ptr(Uint);
  cmd.op_code = Op::ArrayLength;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val ArrayData(Val v) {
  ASSERT(v.type->is_array() && !static_cast<Array *>(v.type)->fixed_length, "");
  Cmd cmd;
  cmd.result.type = Ptr(static_cast<Array *>(v.type)->data_type);
  cmd.op_code = Op::ArrayData;
  cmd.args = {std::move(v)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val PtrIncr(Val v1, Val v2) {
  ASSERT(v1.type->is_pointer(), "");
  ASSERT(v2.type == ::Uint, "");
  Cmd cmd;
  cmd.result.type = v1.type;
  cmd.op_code = Op::PtrIncr;
  cmd.args = {std::move(v1), std::move(v2)};
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

Val Phi(Type *t) {
  Cmd cmd;
  cmd.result.type = t;
  cmd.op_code = Op::Phi;
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

#define MAKE_AND_RETURN(name, v1, v2, result_type)                             \
  Cmd cmd;                                                                     \
  cmd.result.type = result_type;                                               \
  cmd.op_code = Op::name;                                                      \
  cmd.args = {std::move(v1), std::move(v2)};                                   \
  RegIndex reg;                                                                \
  reg.block_index = IR::Block::Current;                                        \
  reg.instr_index =                                                            \
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();       \
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);   \
  return IR::Val::Reg(reg, cmd.result.type)

#define MAKE_AND_RETURN_BOOL_OP(name, v1, v2)                                  \
  ASSERT(v1.type == ::Bool && v2.type == Bool, "");                            \
  MAKE_AND_RETURN(name, v1, v2, Bool)
Val And(Val v1, Val v2) { MAKE_AND_RETURN_BOOL_OP(And, v1, v2); }
Val Or(Val v1, Val v2) { MAKE_AND_RETURN_BOOL_OP(Or, v1, v2); }
Val Xor(Val v1, Val v2) { MAKE_AND_RETURN_BOOL_OP(Xor, v1, v2); }
#undef MAKE_AND_RETURN_BOOL_OP

#define MAKE_AND_RETURN_OP(name, v1, v2)                                       \
  ASSERT(v1.type == v2.type, "");                                              \
  MAKE_AND_RETURN(name, v1, v2, (v1).type)
Val Add(Val v1, Val v2) { MAKE_AND_RETURN_OP(Add, v1, v2); }
Val Sub(Val v1, Val v2) { MAKE_AND_RETURN_OP(Sub, v1, v2); }
Val Mul(Val v1, Val v2) { MAKE_AND_RETURN_OP(Mul, v1, v2); }
Val Div(Val v1, Val v2) { MAKE_AND_RETURN_OP(Div, v1, v2); }
Val Mod(Val v1, Val v2) { MAKE_AND_RETURN_OP(Mod, v1, v2); }
#undef MAKE_AND_RETURN_OP

Val Access(Val index, Val val) {
  ASSERT(val.type->is_array(), "");
  auto ptr_type = Ptr(static_cast<Array *>(val.type)->data_type);
  MAKE_AND_RETURN(Access, index, val, ptr_type);
}

#define MAKE_AND_RETURN_CMP(name, v1, v2)                                      \
  ASSERT(v1.type == v2.type, "");                                              \
  MAKE_AND_RETURN(name, v1, v2, Bool)
Val Lt(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Lt, v1, v2); }
Val Le(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Le, v1, v2); }
Val Eq(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Eq, v1, v2); }
Val Ne(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Ne, v1, v2); }
Val Ge(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Ge, v1, v2); }
Val Gt(Val v1, Val v2) { MAKE_AND_RETURN_CMP(Gt, v1, v2); }
#undef MAKE_AND_RETURN_CMP
#undef MAKE_AND_RETURN

Val Call(Val fn, std::vector<Val> vals) {
  // TODO non-void functions?
  ASSERT(fn.type->is_function(), "");
  Cmd cmd;
  cmd.result.type = fn.type;
  cmd.op_code = Op::Call;
  cmd.args = std::move(vals);
  RegIndex reg;
  reg.block_index = IR::Block::Current;
  reg.instr_index =
      IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.size();
  IR::Func::Current->blocks_[IR::Block::Current.value].cmds_.push_back(cmd);
  return IR::Val::Reg(reg, cmd.result.type);
}

void Cmd::dump(size_t indent) {
  std::cerr << std::string(indent, ' ');
  switch (op_code) {
    case Op::Malloc: std::cerr << "malloc"; break;
    case Op::Free: std::cerr << "free"; break;
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
  }

  if (args.empty()) { return; }
  std::cerr << ": " << args[0].to_string();
  for (size_t i = 1; i < args.size(); ++i) {
    std::cerr << ", " << args[i].to_string();
  }
}
} // namespace IR
