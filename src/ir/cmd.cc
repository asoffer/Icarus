#include "ir.h"

#include "../type/type.h"
#include "property.h"
#include <cmath>

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
            [](auto) {},
        },
        cmd_arg.value);
  }
}

Cmd::Cmd(Type *t, Op op, std::vector<Val> arg_vec)
    : args(std::move(arg_vec)), op_code_(op) {
  CmdIndex cmd_index{
      Block::Current,
      static_cast<i32>(Func::Current->block(Block::Current).cmds_.size())};

  result = Register(t != nullptr ? Func::Current->num_regs_++
                                 : -(++Func::Current->num_voids_));
  type                            = t;
  Func::Current->reg_map_[result] = cmd_index;
  RecordReferences(Func::Current, cmd_index, args);
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

#define CONSTANT_PROPOGATION(op)                                               \
  do {                                                                         \
    if (i32 *n1 = std::get_if<i32>(&v1.value),                                 \
        *n2     = std::get_if<i32>(&v2.value);                                 \
        n1 != nullptr && n2 != nullptr) {                                      \
      return Val::Int(*n1 op * n2);                                            \
    }                                                                          \
    if (u64 *m1 = std::get_if<u64>(&v1.value),                                 \
        *m2     = std::get_if<u64>(&v2.value);                                 \
        m1 != nullptr && m2 != nullptr) {                                      \
      return Val::Uint(*m1 op * m2);                                           \
    }                                                                          \
    if (double *r1 = std::get_if<double>(&v1.value),                           \
        *r2        = std::get_if<double>(&v2.value);                           \
        r1 != nullptr && r2 != nullptr) {                                      \
      return Val::Real(*r1 op * r2);                                           \
    }                                                                          \
  } while (false)

Val Add(Val v1, Val v2) {
  CONSTANT_PROPOGATION(+);
  if (char *c1 = std::get_if<char>(&v1.value),
      *c2      = std::get_if<char>(&v2.value);
      c1 != nullptr && c2 != nullptr) {
    return Val::Char(static_cast<char>(*c1 + *c2));
  }

  if (AST::CodeBlock **cb1 = std::get_if<AST::CodeBlock *>(&v1.value),
      **cb2                = std::get_if<AST::CodeBlock *>(&v2.value);
      cb1 != nullptr && cb2 != nullptr) {
    // TODO leaks
    // Contextualize is definitely wrong and probably not safe. We really want
    // a copy. All Refs should be resolved by this point already.

    auto block   = base::make_owned<AST::CodeBlock>();
    block->stmts = base::move<
        AST::Statements>(AST::Statements::Merge(std::vector{
        &(*cb1)->stmts->contextualize({}).release()->as<AST::Statements>(),
        &(*cb2)->stmts->contextualize({}).release()->as<AST::Statements>()}));
    return Val::CodeBlock(block.release());
  }

  if (EnumVal *e1 = std::get_if<EnumVal>(&v1.value),
      *e2         = std::get_if<EnumVal>(&v2.value);
      e1 != nullptr && e2 != nullptr) {
    return Val::Enum(&v1.type->as<Enum>(), e1->value + e2->value);
  }

  MAKE_AND_RETURN2(v1.type, Op::Add);
}

Val Sub(Val v1, Val v2) {
  CONSTANT_PROPOGATION(-);

  if (char *c1 = std::get_if<char>(&v1.value),
      *c2      = std::get_if<char>(&v2.value);
      c1 != nullptr && c2 != nullptr) {
    return Val::Char(static_cast<char>(*c1 - *c2));
  }

  MAKE_AND_RETURN2(v1.type, Op::Sub);
}

Val Mul(Val v1, Val v2) {
  CONSTANT_PROPOGATION(*);
  MAKE_AND_RETURN2(v1.type, Op::Mul);
}

Val Div(Val v1, Val v2) {
  CONSTANT_PROPOGATION(/);
  MAKE_AND_RETURN2(v1.type, Op::Div);
}

Val Mod(Val v1, Val v2) {
  if (i32 *n1 = std::get_if<i32>(&v1.value), *n2 = std::get_if<i32>(&v2.value);
      n1 != nullptr && n2 != nullptr) {
    return Val::Int(*n1 % *n2);
  }

  if (u64 *m1 = std::get_if<u64>(&v1.value), *m2 = std::get_if<u64>(&v2.value);
      m1 != nullptr && m2 != nullptr) {
    return Val::Uint(*m1 % *m2);
  }

  if (double *r1 = std::get_if<double>(&v1.value),
      *r2        = std::get_if<double>(&v2.value);
      r1 != nullptr && r2 != nullptr) {
    return Val::Real(fmod(*r1, *r2));
  }

  MAKE_AND_RETURN2(v1.type, Op::Mod);
}

Val Arrow(Val v1, Val v2) {
  if (Type **t1 = std::get_if<Type *>(&v1.value),
      **t2      = std::get_if<Type *>(&v2.value);
      t1 != nullptr && t2 != nullptr) {
    return Val::Type(::Func(*t1, *t2));
  }
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
    if (u64 *n = std::get_if<u64>(&v1.value)) {
      return Val::Type(::Arr(*t, *n));
    }
    if (v1 == Val::None()) { return Val::Type(::Arr(*t)); }
  }

  // TODO decide if Int vs Uint is allowed
  MAKE_AND_RETURN2(Type_, Op::Array);
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
#undef CONSTANT_PROPOGATION

#define CONSTANT_PROPOGATION(op)                                               \
  do {                                                                         \
    if (i32 *n1 = std::get_if<i32>(&v1.value),                                 \
        *n2     = std::get_if<i32>(&v2.value);                                 \
        n1 != nullptr && n2 != nullptr) {                                      \
      return Val::Bool(*n1 op * n2);                                           \
    }                                                                          \
    if (u64 *m1 = std::get_if<u64>(&v1.value),                                 \
        *m2     = std::get_if<u64>(&v2.value);                                 \
        m1 != nullptr && m2 != nullptr) {                                      \
      return Val::Bool(*m1 op * m2);                                           \
    }                                                                          \
    if (double *r1 = std::get_if<double>(&v1.value),                           \
        *r2        = std::get_if<double>(&v2.value);                           \
        r1 != nullptr && r2 != nullptr) {                                      \
      return Val::Bool(*r1 op * r2);                                           \
    }                                                                          \
  } while (false)

Val Lt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(<);
  MAKE_AND_RETURN2(::Bool, Op::Lt);
}

Val Le(Val v1, Val v2) {
  CONSTANT_PROPOGATION(<=);
  MAKE_AND_RETURN2(::Bool, Op::Le);
}

Val Gt(Val v1, Val v2) {
  CONSTANT_PROPOGATION(>);
  MAKE_AND_RETURN2(::Bool, Op::Gt);
}

Val Ge(Val v1, Val v2) {
  CONSTANT_PROPOGATION(>=);
  MAKE_AND_RETURN2(::Bool, Op::Ge);
}

Val Eq(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? v2 : Neg(v2); }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? v1 : Neg(v1); }

  if (char *c1 = std::get_if<char>(&v1.value),
      *c2      = std::get_if<char>(&v2.value);
      c1 != nullptr && c2 != nullptr) {
    return Val::Bool(*c1 == *c2);
  }

  CONSTANT_PROPOGATION(==);

  if (Type **t1 = std::get_if<Type *>(&v1.value),
      **t2      = std::get_if<Type *>(&v2.value);
      t1 != nullptr && t2 != nullptr) {
    return Val::Bool(*t1 == *t2);
  }

  if (Addr *a1 = std::get_if<Addr>(&v1.value),
      *a2      = std::get_if<Addr>(&v2.value);
      a1 != nullptr && a2 != nullptr) {
    return Val::Bool(*a1 == *a2);
  }

  MAKE_AND_RETURN2(::Bool, Op::Eq);
}

Val Ne(Val v1, Val v2) {
  if (bool *b = std::get_if<bool>(&v1.value)) { return *b ? Neg(v2) : v2; }
  if (bool *b = std::get_if<bool>(&v2.value)) { return *b ? Neg(v1) : v1; }

  char *c1 = std::get_if<char>(&v1.value), *c2 = std::get_if<char>(&v2.value);
  if (c1 != nullptr && c2 != nullptr) { return Val::Bool(*c1 != *c2); }

  CONSTANT_PROPOGATION(!=);

  if (Type **t1 = std::get_if<Type *>(&v1.value),
      **t2      = std::get_if<Type *>(&v2.value);
      t1 != nullptr && t2 != nullptr) {
    return Val::Bool(*t1 != *t2);
  }

  if (Addr *a1 = std::get_if<Addr>(&v1.value),
      *a2      = std::get_if<Addr>(&v2.value);
      a1 != nullptr && a2 != nullptr) {
    return Val::Bool(*a1 != *a2);
  }

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

// TODO this may not be necessary anymore? I can just make the phi later?
void Func::SetArgs(CmdIndex cmd_index, std::vector<Val> args) {
  auto &cmd = Command(cmd_index);
  ASSERT(cmd.op_code_ == Op::Phi, "");
  cmd.args = std::move(args);
  RecordReferences</* IsPhi = */ true>(Func::Current, cmd_index, cmd.args);
}

} // namespace IR
