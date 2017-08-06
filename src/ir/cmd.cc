#include "ir.h"

#include "../type/type.h"

namespace IR {
BlockIndex Block::Current;
Func *Func::Current;

Cmd::Cmd(Type *t, Op op, std::vector<Val> args)
    : args(std::move(args)), op_code(op) {
  auto reg_index = RegIndex{Func::Current->num_cmds_++};
  Func::Current->reg_map_[reg_index] =
      std::make_pair(Block::Current,
                     Func::Current->blocks_[Block::Current.value].cmds_.size());
  result = Val::Reg(reg_index, t);
}

Val SetReturn(size_t n, Val v) {
  Cmd cmd(Void, Op::SetReturn, {Val::Uint(n), std::move(v)});
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Field(Val v, size_t n) {
  ASSERT(v.type->is<Pointer>(), v.type->to_string());
  auto ptee_type = ptr_cast<Pointer>(v.type)->pointee;
  Cmd cmd(Ptr(ptr_cast<Struct>(ptee_type)->field_type[n]), Op::Field,
          {std::move(v), Val::Uint(n)});
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

#define MAKE_AND_RETURN(type, op)                                              \
  ASSERT(Func::Current, "");                                                   \
  Cmd cmd(type, op, {std::move(v)});                                           \
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);           \
  return cmd.result

#define MAKE_AND_RETURN2(type, op)                                             \
  Cmd cmd(type, op, {std::move(v1), std::move(v2)});                           \
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);           \
  return cmd.result

Val Malloc(Type *t, Val v) {
  ASSERT_EQ(v.type, ::Uint);
  MAKE_AND_RETURN(t, Op::Malloc);
}

Val Extend(Val v) { MAKE_AND_RETURN(Char, Op::Extend); }
Val Trunc(Val v) { MAKE_AND_RETURN(Char, Op::Trunc); }
Val Neg(Val v) { MAKE_AND_RETURN(v.type, Op::Neg); }
Val Print(Val v) { MAKE_AND_RETURN(Void, Op::Print); }
Val Free(Val v) {
  ASSERT(v.type->is<Pointer>(), "");
  MAKE_AND_RETURN(Void, Op::Free);
}

Val Alloca(Type *t) {
  ASSERT_NE(t, ::Void);
  Cmd cmd(Ptr(t), Op::Alloca, {});
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Contextualize(AST::CodeBlock *code, std::vector<IR::Val> args) {
  ASSERT(code != nullptr, "");
  args.push_back(IR::Val::CodeBlock(code));
  Cmd cmd(::Code, Op::Contextualize, std::move(args));
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Load(Val v) {
  ASSERT(v.type->is<Pointer>(), v.to_string());
  MAKE_AND_RETURN(ptr_cast<Pointer>(v.type)->pointee, Op::Load);
}

Val ArrayLength(Val v) {
  ASSERT(v.type->is<Pointer>(), "");
  auto *ptee = ptr_cast<Pointer>(v.type)->pointee;
  ASSERT(ptee->is<::Array>(), "");
  ASSERT(!ptr_cast<::Array>(ptee)->fixed_length, "");
  MAKE_AND_RETURN(Ptr(Uint), Op::ArrayLength);
}

Val ArrayData(Val v) {
  ASSERT(v.type->is<Pointer>(), "");
  auto *ptee = ptr_cast<Pointer>(v.type)->pointee;
  ASSERT(ptee->is<::Array>(), "");
  auto *array_type = ptr_cast<::Array>(ptee);
  ASSERT(!array_type->fixed_length, "");
  MAKE_AND_RETURN(Ptr(Ptr(array_type->data_type)), Op::ArrayData);
}

Val Store(Val v1, Val v2) {
  ASSERT(v2.type->is<Pointer>(), "");
  MAKE_AND_RETURN2(Void, Op::Store);
}

Val PtrIncr(Val v1, Val v2) {
  ASSERT(v1.type->is<Pointer>(), "");
  ASSERT_EQ(v2.type, ::Uint);
  MAKE_AND_RETURN2(v1.type, Op::PtrIncr);
}

Val Ptr(Val v) {
  ASSERT_EQ(v.type, Type_);
  MAKE_AND_RETURN(Type_, Op::Ptr);
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
  ASSERT(v1.type == nullptr || v1.type == Uint || v1.type == Int, "");
  ASSERT_EQ(v2.type, Type_);
  MAKE_AND_RETURN2(Type_, Op::Array);
}

Val Index(Val v1, Val v2) {
  ASSERT(v1.type->is<::Pointer>(), "");
  ASSERT(ptr_cast<Pointer>(v1.type)->pointee->is<::Array>(), "");
  ASSERT_EQ(v2.type, ::Uint);
  MAKE_AND_RETURN2(
      Ptr(ptr_cast<::Array>(ptr_cast<Pointer>(v1.type)->pointee)->data_type),
      Op::Index);
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
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
  return cmd.result;
}

Val Call(Val fn, std::vector<Val> vals) {
  ASSERT(fn.type->is<Function>(), "");
  vals.push_back(fn);
  Cmd cmd(static_cast<Function *>(fn.type)->output, Op::Call, std::move(vals));
  Func::Current->blocks_[Block::Current.value].cmds_.push_back(cmd);
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
  case Op::Index: std::cerr << "index"; break;
  case Op::Load: std::cerr << "load"; break;
  case Op::Store: std::cerr << "store"; break;
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

void Block::dump(size_t indent) const {
  for (const auto &cmd : cmds_) { cmd.dump(indent); }
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
  case Type::Ret: std::cerr << "return." << std::endl; break;
  case Type::None: std::cerr << "none." << std::endl; break;
  }
}

void Func::dump() const {
  std::cerr << (name == "" ? "(anon)" : name) << ": " << *type;
  for (size_t i = 0; i < blocks_.size(); ++i) {
    std::cerr << "\n block #" << i << std::endl;
    blocks_[i].dump(2);
  }
}

ExecContext::Frame::Frame(Func *fn, std::vector<Val> arguments)
    : fn_(fn), current_(fn_->entry()), prev_(fn_->entry()),
      regs_(fn_->num_cmds_, IR::Val::None()), args_(std::move(arguments)),
      rets_(1, IR::Val::None()) {}

Cmd &Func::Command(RegIndex reg) {
  auto iter = reg_map_.find(reg);
  ASSERT(iter != reg_map_.end(), "");
  auto &block_and_index = iter->second;
  return blocks_[block_and_index.first.value].cmds_[block_and_index.second];
}

void Func::SetArgs(RegIndex reg, std::vector<IR::Val> args) {
  Command(reg).args = std::move(args);
}

} // namespace IR
