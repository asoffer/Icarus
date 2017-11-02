#include "ir.h"

#include "../type/type.h"
#include "property.h"
#include <cmath>

namespace IR {
BlockIndex Block::Current;
Func *Func::Current{nullptr};
Cmd::Cmd(Type *t, Op op, std::vector<Val> arg_vec)
    : args(std::move(arg_vec)), op_code(op) {
  CmdIndex cmd_index{
      Block::Current,
      static_cast<i32>(Func::Current->block(Block::Current).cmds_.size())};

  if (t == nullptr) {
    result = Register{std::numeric_limits<i32>::min()};
  } else {
    result                          = Register(Func::Current->num_regs_++);
    type                            = t;
    Func::Current->reg_map_[result] = cmd_index;
  }

  bool has_dependencies = false;

  // TODO deal with specialness of phi-nodes
  if (op == Op::Phi) { has_dependencies = true; }

  for (const auto &fn_arg : args) {
    if (fn_arg.value.is<Register>()) {
      has_dependencies = true;
      Func::Current->references_[fn_arg.value.as<Register>()].push_back(
           cmd_index);
    }
  }
  if (!has_dependencies) {
    IR::Func::Current->no_dependencies_.push_back(cmd_index);
  }
}

Val Field(Val v, size_t n) {
  ASSERT_TYPE(Pointer, v.type);
  auto ptee_type = ptr_cast<Pointer>(v.type)->pointee;
  Cmd cmd(Ptr(ptr_cast<Struct>(ptee_type)->field_type[n]), Op::Field,
          {std::move(v), Val::Uint(n)});
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
  return cmd.reg();
}

#define MAKE_VOID(op)                                                          \
  ASSERT(Func::Current, "");                                                   \
  Cmd cmd(nullptr, op, {std::move(v)});                                        \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);

#define MAKE_VOID2(op)                                                         \
  ASSERT(Func::Current, "");                                                   \
  Cmd cmd(nullptr, op, {std::move(v1), std::move(v2)});                        \
  Func::Current->block(Block::Current).cmds_.push_back(cmd);

#define MAKE_AND_RETURN(type, op)                                              \
  ASSERT(Func::Current, "");                                                   \
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
  if (v.value.is<char>()) {
    return Val::Uint(static_cast<u64>(v.value.as<char>()));
  } else {
    MAKE_AND_RETURN(Char, Op::Extend);
  }
}

Val Trunc(Val v) {
  if (v.value.is<u64>()) {
    return Val::Char(static_cast<char>(v.value.as<u64>()));
  } else {
    MAKE_AND_RETURN(Char, Op::Trunc);
  }
}

Val Neg(Val v) {
  if (v.value.is<bool>()) {
    return Val::Bool(!v.value.as<bool>());
  } else if (v.value.is<i32>()) {
    return Val::Int(-v.value.as<i32>());
  } else if (v.value.is<double>()) {
    return Val::Real(-v.value.as<double>());
  } else {
    MAKE_AND_RETURN(v.type, Op::Neg);
  }
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

Val Contextualize(AST::CodeBlock *code, std::vector<IR::Val> args) {
  ASSERT(code != nullptr, "");
  args.push_back(IR::Val::CodeBlock(code));
  Cmd cmd(::Code, Op::Contextualize, std::move(args));
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
  return cmd.reg();
}

Val Load(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  MAKE_AND_RETURN(ptr_cast<Pointer>(v.type)->pointee, Op::Load);
}

Val ArrayLength(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  auto *ptee = ptr_cast<Pointer>(v.type)->pointee;
  ASSERT_TYPE(::Array, ptee);
  ASSERT(!ptr_cast<::Array>(ptee)->fixed_length,
         "Pointee type is " + ptee->to_string());
  MAKE_AND_RETURN(Ptr(Uint), Op::ArrayLength);
}

Val ArrayData(Val v) {
  ASSERT_TYPE(Pointer, v.type);
  auto *ptee = ptr_cast<Pointer>(v.type)->pointee;
  ASSERT_TYPE(::Array, ptee);
  auto *array_type = ptr_cast<::Array>(ptee);
  ASSERT(!array_type->fixed_length, "");
  MAKE_AND_RETURN(Ptr(Ptr(array_type->data_type)), Op::ArrayData);
}

void SetReturn(ReturnValue r, Val v2) {
  Val v1 = Val::Ret(r, v2.type);
  MAKE_VOID2(Op::SetReturn);
}

void Store(Val v1, Val v2) {
  if (v2.value.is<ReturnValue>()) {
    SetReturn(v2.value.as<ReturnValue>(), v1);
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
  if (v.value.is<Type *>()) {
    return Val::Type(::Ptr(v.value.as<Type *>()));
  } else {
    MAKE_AND_RETURN(Type_, Op::Ptr);
  }
}

Val And(Val v1, Val v2) {
  if (v1.value.is<bool>()) {
    return v1.value.as<bool>() ? v2 : Val::Bool(false);
  } else if (v2.value.is<bool>()) {
    return v2.value.as<bool>() ? v1 : Val::Bool(false);
  } else {
    MAKE_AND_RETURN2(Bool, Op::And);
  }
}

Val Or(Val v1, Val v2) {
  if (v1.value.is<bool>()) {
    return v1.value.as<bool>() ? Val::Bool(true) : v2;
  } else if (v2.value.is<bool>()) {
    return v2.value.as<bool>() ? Val::Bool(true) : v1;
  } else {
    MAKE_AND_RETURN2(Bool, Op::Or);
  }
}

Val Xor(Val v1, Val v2) {
  if (v1.value.is<bool>()) {
    return v1.value.as<bool>() ? Neg(v2) : v2;
  } else if (v2.value.is<bool>()) {
    return v2.value.as<bool>() ? Neg(v1) : v1;
  } else {
    MAKE_AND_RETURN2(Bool, Op::Xor);
  }
}

Val Add(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Int(v1.value.as<i32>() + v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Uint(v1.value.as<u64>() + v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Real(v1.value.as<double>() + v2.value.as<double>());
  } else if (v1.value.is<char>() && v2.value.is<char>()) {
    return Val::Char(
        static_cast<char>(v1.value.as<char>() + v2.value.as<char>()));
  } else if (v1.value.is<AST::CodeBlock *>() &&
             v2.value.is<AST::CodeBlock *>()) {
    // TODO leaks
    // Contextualize is definitely wrong and probably not safe. We really want
    // a copy. All Refs should be resolved by this point already.

    auto block   = base::make_owned<AST::CodeBlock>();
    block->stmts = base::move<AST::Statements>(AST::Statements::Merge({
        ptr_cast<AST::Statements>(v1.value.as<AST::CodeBlock *>()
                                      ->stmts->contextualize({})
                                      .release()),
        ptr_cast<AST::Statements>(v2.value.as<AST::CodeBlock *>()
                                      ->stmts->contextualize({})
                                      .release()),
    }));

    return Val::CodeBlock(block.release());
  } else if (v1.type->is<Enum>()) {
    return Val::Enum(ptr_cast<Enum>(v1.type), v1.value.as<EnumVal>().value +
                                                  v2.value.as<EnumVal>().value);
  } else {
    MAKE_AND_RETURN2(v1.type, Op::Add);
  }
}

Val Sub(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Int(v1.value.as<i32>() - v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Uint(v1.value.as<u64>() - v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Real(v1.value.as<double>() - v2.value.as<double>());
  } else if (v1.value.is<char>() && v2.value.is<char>()) {
    return Val::Char(
        static_cast<char>(v1.value.as<char>() - v1.value.as<char>()));
  } else {
    MAKE_AND_RETURN2(v1.type, Op::Sub);
  }
}

Val Mul(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Int(v1.value.as<i32>() * v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Uint(v1.value.as<u64>() * v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Real(v1.value.as<double>() * v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(v1.type, Op::Mul);
  }
}

Val Div(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Int(v1.value.as<i32>() / v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Uint(v1.value.as<u64>() / v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Real(v1.value.as<double>() / v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(v1.type, Op::Div);
  }
}

Val Mod(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Int(v1.value.as<i32>() % v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Uint(v1.value.as<u64>() % v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Real(fmod(v1.value.as<double>(), v2.value.as<double>()));
  } else {
    MAKE_AND_RETURN2(v1.type, Op::Mod);
  }
}

Val Arrow(Val v1, Val v2) {
  if (v1.value.is<Type *>() && v2.value.is<Type *>()) {
    return Val::Type(::Func(v1.value.as<Type *>(), v2.value.as<Type *>()));
  } else {
    MAKE_AND_RETURN2(Type_, Op::Arrow);
  }
}

Val Array(Val v1, Val v2) {
  ASSERT(v1.type == nullptr || v1.type == Uint || v1.type == Int, "");
  ASSERT_EQ(v2.type, Type_);

  if (v2.value.is<Type *>() && v1.value.is<u64>()) {
    return Val::Type(::Arr(v2.value.as<Type *>(), v1.value.as<u64>()));
  } else if (v2.value.is<Type *>() && v1.value.is<i32>()) {
    return Val::Type(::Arr(v2.value.as<Type *>(), v1.value.as<i32>()));
  } else if (v2.value.is<Type *>() && v1 == Val::None()) {
    return Val::Type(::Arr(v2.value.as<Type *>()));
  } else {
    // TODO decide if Int vs Uint is allowed
    MAKE_AND_RETURN2(Type_, Op::Array);
  }
}

Val Index(Val v1, Val v2) {
  ASSERT_TYPE(Pointer, v1.type);
  auto *ptee = ptr_cast<Pointer>(v1.type)->pointee;
  ASSERT_TYPE(::Array, ptee);
  ASSERT_EQ(v2.type, ::Uint);
  auto *array_type = ptr_cast<::Array>(ptee);

  IR::Val ptr = array_type->fixed_length ? v1 : Load(ArrayData(v1));
  ptr.type    = Ptr(array_type->data_type);
  return PtrIncr(ptr, v2);
}

Val Lt(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() < v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() < v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() < v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Lt);
  }
}

Val Le(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() <= v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() <= v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() <= v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Le);
  }
}

Val Gt(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() > v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() > v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() > v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Lt);
  }
}

Val Ge(Val v1, Val v2) {
  if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() >= v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() >= v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() >= v2.value.as<double>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Le);
  }
}

Val Eq(Val v1, Val v2) {
  if (v1.value.is<bool>()) {
    return v1.value.as<bool>() ? v2 : Neg(v2);
  } else if (v2.value.is<bool>()) {
    return v2.value.as<bool>() ? v1 : Neg(v1);
  } else if (v1.value.is<char>() && v2.value.is<char>()) {
    return Val::Bool(v1.value.as<char>() == v2.value.as<char>());
  } else if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() == v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() == v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() == v2.value.as<double>());
  } else if (v1.value.is<Type *>() && v2.value.is<Type *>()) {
    return Val::Bool(v1.value.as<Type *>() == v2.value.as<Type *>());
  } else if (v1.value.is<Addr>() && v2.value.is<Addr>()) {
    return Val::Bool(v1.value.as<Addr>() == v2.value.as<Addr>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Eq);
  }
}

Val Ne(Val v1, Val v2) {
  if (v1.value.is<bool>()) {
    return v1.value.as<bool>() ? Neg(v2) : v2;
  } else if (v2.value.is<bool>()) {
    return v2.value.as<bool>() ? Neg(v1) : v1;
  } else if (v1.value.is<char>() && v2.value.is<char>()) {
    return Val::Bool(v1.value.as<char>() != v2.value.as<char>());
  } else if (v1.value.is<i32>() && v2.value.is<i32>()) {
    return Val::Bool(v1.value.as<i32>() != v2.value.as<i32>());
  } else if (v1.value.is<u64>() && v2.value.is<u64>()) {
    return Val::Bool(v1.value.as<u64>() != v2.value.as<u64>());
  } else if (v1.value.is<double>() && v2.value.is<double>()) {
    return Val::Bool(v1.value.as<double>() != v2.value.as<double>());
  } else if (v1.value.is<Type *>() && v2.value.is<Type *>()) {
    return Val::Bool(v1.value.as<Type *>() != v2.value.as<Type *>());
  } else if (v1.value.is<Addr>() && v2.value.is<Addr>()) {
    return Val::Bool(v1.value.as<Addr>() != v2.value.as<Addr>());
  } else {
    MAKE_AND_RETURN2(::Bool, Op::Ne);
  }
}

Val Cast(Val v1, Val v2) {
  // v1 = result_type, v2 = val
  ASSERT_EQ(v1.type, Type_);
  MAKE_AND_RETURN2(v1.value.as<::Type *>(), Op::Cast);
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

Val Call(Val fn, std::vector<Val> vals) {
  ASSERT_TYPE(Function, fn.type);
  vals.push_back(fn);
  Cmd cmd(static_cast<Function *>(fn.type)->output, Op::Call, std::move(vals));
  Func::Current->block(Block::Current).cmds_.push_back(cmd);
  return cmd.reg();
}

void Cmd::dump(size_t indent) const {
  std::cerr << std::string(indent, ' ');
  if (type != nullptr) { std::cerr << reg().to_string() << " = "; }
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
  case Type::Uncond: std::cerr << "jmp #" << block_index << std::endl; break;
  case Type::Cond:
    std::cerr << "cond " << cond_data.cond.to_string() << std::endl
              << "T => #" << cond_data.true_block << "F => #"
              << cond_data.false_block << std::endl;
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

ExecContext::Frame::Frame(Func *fn, const std::vector<Val> &arguments)
    : fn_(fn), current_(fn_->entry()), prev_(fn_->entry()),
      regs_(fn->num_regs_, Val::None()), rets_(1, Val::None()) {
  for (size_t i = 0; i < arguments.size(); ++i) { regs_[i] = arguments[i]; }
}

void Func::SetArgs(CmdIndex cmd_index, std::vector<Val> args) {
  // TODO this should only be called for phi nodes
  // TODO recompute dependencies.
  Command(cmd_index).args = std::move(args);
}

} // namespace IR
