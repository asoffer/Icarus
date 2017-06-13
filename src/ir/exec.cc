#include "ir.h"

#include <cmath>
#include <memory>

#include "../type/type.h"
#include "../ast/ast.h"
#include "../scope.h"

std::vector<IR::Val> global_vals;

static AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr) {
  std::vector<Error> errors;
  expr->verify_types(&errors);
  auto fn_ptr = new AST::FunctionLiteral;

  // TODO should these be at global scope? or a separate REPL scope?
  fn_ptr->type               = Func(Void, expr->type);
  fn_ptr->fn_scope           = Scope::Global->add_child<FnScope>();
  fn_ptr->fn_scope->fn_type  = (Function *)fn_ptr->type;
  fn_ptr->scope_             = expr->scope_;
  fn_ptr->statements         = new AST::Statements;
  fn_ptr->statements->scope_ = fn_ptr->fn_scope;
  fn_ptr->return_type_expr   = new AST::Terminal(
      expr->loc, Language::Terminal::Type, Type_, IR::Val::Type(expr->type));
  AST::Unop *ret             = nullptr;

  if (expr->type != Void) {
    ret             = new AST::Unop;
    ret->scope_     = fn_ptr->fn_scope;
    ret->operand    = expr;
    ret->op         = Language::Operator::Return;
    ret->precedence = Language::precedence(Language::Operator::Return);
    fn_ptr->statements->statements.push_back(ret);
  } else {
    fn_ptr->statements->statements.push_back(expr);
  }

  return fn_ptr;
}

void ReplEval(AST::Expression *expr) {
  auto fn = std::make_unique<IR::Func>(Func(Void, Void));
  CURRENT_FUNC(fn.get()) {
    IR::Block::Current = fn->entry();
    std::vector<Error> errors;
    auto expr_val = expr->VerifyAndEmitIR(&errors);
    if (!errors.empty()) {
      std::cerr << "There were " << errors.size() << " errors.";
      return;
    }

    if (expr->type != Void) { expr->type->EmitRepr(expr_val); }
    IR::Jump::Return();
  }

  fn->Execute({});
}

IR::Val Evaluate(AST::Expression *expr) {
  IR::Func *fn = nullptr;
  auto fn_ptr = WrapExprIntoFunction(expr);
  std::vector<Error> errors;
  CURRENT_FUNC(nullptr) { fn = fn_ptr->VerifyAndEmitIR(&errors).as_func; }

  auto result = fn->Execute({});

  if (expr->type == Void) {
    fn_ptr->statements->statements[0] = nullptr;
  } else {
    auto ret = fn_ptr->statements->statements.front();
    ASSERT(ret->is_unop(), "");
    auto unop = static_cast<AST::Unop *>(ret);
    ASSERT(unop->op == Language::Operator::Return, "");
    unop->operand = nullptr;
  }
  delete fn_ptr;
  ASSERT(!result.empty(), "");
  return result[0]; // TODO multiple outputs?
}

namespace IR {
ExecContext::ExecContext(const Func *fn)
    : current_fn(fn), current_block{0}, stack_(1) {}

BlockIndex ExecContext::ExecuteBlock() {
  for (const auto &cmd : current_fn->blocks_[current_block.value].cmds_) {
    auto result = ExecuteCmd(cmd);
    if (cmd.result.kind == Val::Kind::Reg) {
      this->reg(cmd.result.as_reg) = result;
    }
  }

  switch (current_fn->blocks_[current_block.value].jmp_.type) {
  case Jump::Type::Uncond:
    return current_fn->blocks_[current_block.value].jmp_.block_index;
  case Jump::Type::Cond: {
    Val cond_val =
        current_fn->blocks_[current_block.value].jmp_.cond_data.cond;
    ASSERT(cond_val.type == Bool, "");
    Resolve(&cond_val);
    return cond_val.as_bool
               ? current_fn->blocks_[current_block.value]
                     .jmp_.cond_data.true_block
               : current_fn->blocks_[current_block.value]
                     .jmp_.cond_data.false_block;
  } break;
  case Jump::Type::Ret:
    return BlockIndex{-1};
  }
  UNREACHABLE;
}

void ExecContext::Resolve(Val* v) const {
  switch (v->kind) {
  case Val::Kind::Arg:
    ASSERT(args_.size() > v->as_arg, "");
    *v = arg(v->as_arg);
    return;
  case Val::Kind::Reg: *v = reg(v->as_reg); return;
  case Val::Kind::Frame: *v = stack_from_frame(*v); return;
  case Val::Kind::Global: return;
  case Val::Kind::Heap: return;
  case Val::Kind::Const: return;
  case Val::Kind::None: return;
  }
}

Val ExecContext::ExecuteCmd(const Cmd& cmd) {
  std::vector<Val> resolved = cmd.args;
  for (auto& r : resolved) { Resolve(&r); }

  switch (cmd.op_code) {
  case Op::Neg:
    if (resolved[0].type == Bool) {
      return Val::Bool(!resolved[0].as_bool);
    } else if (resolved[0].type == Int) {
      return Val::Int(-resolved[0].as_int);
    } else if (resolved[0].type == Real) {
      return Val::Real(-resolved[0].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Add:
    if (resolved[0].type == Char) {
      return Val::Char(
          static_cast<char>(resolved[0].as_char + resolved[1].as_char));
    } else if (resolved[0].type == Int) {
      return Val::Int(resolved[0].as_int + resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Uint(resolved[0].as_uint + resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Real(resolved[0].as_real + resolved[1].as_real);
    } else {
      cmd.dump(0);
      for (auto &r : resolved) { std::cerr << r.to_string() << std::endl; }
      UNREACHABLE;
    }
  case Op::Sub:
    if (resolved[0].type == Char) {
      return Val::Char(
          static_cast<char>(resolved[0].as_char - resolved[1].as_char));
    } else if (resolved[0].type == Int) {
      return Val::Int(resolved[0].as_int - resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Uint(resolved[0].as_uint - resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Real(resolved[0].as_real - resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Mul:
    if (resolved[0].type == Int) {
      return Val::Int(resolved[0].as_int * resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Uint(resolved[0].as_uint * resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Real(resolved[0].as_real * resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Div:
    if (resolved[0].type == Int) {
      return Val::Int(resolved[0].as_int / resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Uint(resolved[0].as_uint / resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Real(resolved[0].as_real / resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Mod:
    if (resolved[0].type == Int) {
      return Val::Int(resolved[0].as_int % resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Uint(resolved[0].as_uint % resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Real(fmod(resolved[0].as_real, resolved[1].as_real));
    } else {
      UNREACHABLE;
    }
  case Op::Arrow:
    if (resolved[0].type == Type_) {
      return Val::Type(::Func(resolved[0].as_type, resolved[1].as_type));
    } else {
      UNREACHABLE;
    }
  case Op::Array:
    if (resolved[0].type == Uint) {
      return Val::Type(
          ::Arr(resolved[1].as_type, static_cast<size_t>(resolved[0].as_uint)));
    } else if (resolved[0].type == Int) {
      return Val::Type(
          ::Arr(resolved[1].as_type, static_cast<size_t>(resolved[0].as_int)));
    } else {
      UNREACHABLE;
    }
  case Op::Cast:
    if (resolved[1].type == Int) {
      if (resolved[0].as_type == Int) {
        return resolved[1];
      } else if (resolved[0].as_type == Uint) {
        return IR::Val::Uint(static_cast<u64>(resolved[1].as_int));
      } else if (resolved[0].as_type == Real) {
        return IR::Val::Real(static_cast<double>(resolved[1].as_int));
      } else {
        NOT_YET;
      }
    } else if (resolved[1].type == Uint) {
      if (resolved[0].as_type == Uint) {
        return resolved[1];
      } else if (resolved[0].as_type == Int) {
        return IR::Val::Uint(static_cast<i64>(resolved[1].as_uint));
      } else if (resolved[0].as_type == Real) {
        return IR::Val::Real(static_cast<double>(resolved[1].as_uint));
      } else {
        NOT_YET;
      }
    } else {
      NOT_YET;
    }
  case Op::And:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool & resolved[1].as_bool);
    } else {
      UNREACHABLE;
    }
  case Op::Or:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool | resolved[1].as_bool);
    } else {
      UNREACHABLE;
    }
  case Op::Xor:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool ^ resolved[1].as_bool);
    } else {
      UNREACHABLE;
    }
  case Op::Lt:
    if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int < resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint < resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real < resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Le:
    if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int <= resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint <= resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real <= resolved[1].as_real);
    } else if (resolved[0].type == Char) {
      return Val::Bool(resolved[0].as_char <= resolved[1].as_char);
    } else {
      UNREACHABLE;
    }
  case Op::Eq:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool == resolved[1].as_bool);
    } else if (resolved[0].type == Char) {
      return Val::Bool(resolved[0].as_char == resolved[1].as_char);
    } else if (resolved[0].type == U16) {
      return Val::Bool(resolved[0].as_u16 == resolved[1].as_u16);
    } else if (resolved[0].type == U32) {
      return Val::Bool(resolved[0].as_u32 == resolved[1].as_u32);
    } else if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int == resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint == resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real == resolved[1].as_real);
    } else if (resolved[0].type == Type_) {
      return Val::Bool(resolved[0].as_type == resolved[1].as_type);
    } else {
      UNREACHABLE;
    }
  case Op::Ne:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool != resolved[1].as_bool);
    } else if (resolved[0].type == Char) {
      return Val::Bool(resolved[0].as_char != resolved[1].as_char);
    } else if (resolved[0].type == U16) {
      return Val::Bool(resolved[0].as_u16 != resolved[1].as_u16);
    } else if (resolved[0].type == U32) {
      return Val::Bool(resolved[0].as_u32 != resolved[1].as_u32);
    } else if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int != resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint != resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real != resolved[1].as_real);
    } else if (resolved[0].type == Type_) {
      return Val::Bool(resolved[0].as_type != resolved[1].as_type);
    } else {
      UNREACHABLE;
    }
  case Op::Ge:
    if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int >= resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint >= resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real >= resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::Gt:
    if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int > resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint > resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real > resolved[1].as_real);
    } else {
      UNREACHABLE;
    }
  case Op::SetReturn: {
    rets_[resolved[0].as_uint] = resolved[1];
    return IR::Val::None();
  }
  case Op::Extend:
    return Val::Uint(static_cast<u64>(resolved[0].as_char));
  case Op::Trunc:
    return Val::Char(static_cast<char>(resolved[0].as_uint));
  case Op::Call: {
    IR::Val fn = resolved.back();
    resolved.pop_back();
    // TODO multiple returns?
    auto results = fn.as_func->Execute(std::move(resolved));
    ASSERT(!results.empty(), "");
    return results[0];
  } break;
  case Op::Print:
    if (resolved[0].type == Int) {
      std::cerr << resolved[0].as_int;
    } else if (resolved[0].type == Uint) {
      std::cerr << resolved[0].as_uint;
    } else if (resolved[0].type == Bool) {
      std::cerr << (resolved[0].as_bool ? "true" : "false");
    } else if (resolved[0].type == Char) {
      std::cerr << resolved[0].as_char;
    } else if (resolved[0].type == Real) {
      std::cerr << resolved[0].as_real;
    } else if (resolved[0].type == Type_) {
      std::cerr << resolved[0].as_type->to_string();
    } else if (resolved[0].type == Code_) {
      std::cerr << *resolved[0].as_code;
    } else {
      NOT_YET;
    }
    return IR::Val::None();
  case Op::Load:
    switch (resolved[0].kind) {
    case Val::Kind::Global:
      return global_vals[resolved[0].as_global_addr];
    case Val::Kind::Frame: {
      StackEntry& entry = stack_.back().locals_[resolved[0].as_frame_addr];
      if (entry.type == Int) {
        return IR::Val::Int(*static_cast<i64 *>(entry.data));
      } else if (entry.type == Bool) {
        return IR::Val::Bool(*static_cast<bool *>(entry.data));
      } else {
        NOT_YET;
      }
    }
    default:
      NOT_YET;
    }
  case Op::Store:
    switch (resolved[1].kind) {
    case Val::Kind::Global:
      global_vals[resolved[1].as_global_addr] = resolved[0];
      return IR::Val::None();
    case Val::Kind::Frame: {
      ASSERT(stack_.back().locals_.size() > resolved[1].as_frame_addr, "");
      StackEntry& entry = stack_.back().locals_[resolved[1].as_frame_addr];
      if (entry.type == Int) {
        *static_cast<i64 *>(entry.data) = resolved[0].as_int;
      } else if (entry.type == Bool) {
        *static_cast<bool *>(entry.data) = resolved[0].as_bool;
      } else {
        cmd.dump(0);
        std::cerr << *entry.type << std::endl;
        NOT_YET;
      }
    }
      return IR::Val::None();
    default:
      NOT_YET;
    }
  case Op::Phi:
    for (size_t i = 0; i < resolved.size(); i += 2) {
      if (prev_block == resolved[i].as_block) {
        return resolved[i + 1];
      }
    }
    UNREACHABLE;
  case Op::Alloca: {
    auto val =
        IR::Val::FrameAddr(stack_.back().locals_.size(), cmd.result.type);
    ASSERT(cmd.result.type->is_pointer(), "");
    stack_.back().locals_.emplace_back(
        static_cast<Pointer *>(cmd.result.type)->pointee);
    return val;
  }
  default:
    cmd.dump(10);
    NOT_YET;
  }
  UNREACHABLE;
}

StackEntry::StackEntry(Type *t) : type(t) { data = malloc(t->bytes()); }
StackEntry::~StackEntry() { free(data); }

std::vector<Val> Func::Execute(std::vector<Val> arguments) const {
  auto ctx = ExecContext(this);
  ctx.args_ = std::move(arguments);
  // Type *output_type = static_cast<Function *>(type)->output;
  // tuples for output returns?
  // TODO these should be in the ctor
  ctx.rets_.resize(1, IR::Val::None()); // 1 for now beacuse not handling tuples
  ctx.regs_.resize(blocks_.size());
  for (size_t i = 0; i < blocks_.size(); ++i) {
    ctx.regs_[i].resize(blocks_[i].cmds_.size(), IR::Val::None());
  }
  // TODO args
  while (true) {
    auto block_index = ctx.ExecuteBlock();
    if (block_index.is_none()) {
      return std::move(ctx.rets_);
    } else if (block_index.value >= 0) {
      ctx.prev_block = ctx.current_block;
      ctx.current_block = block_index;
    } else {
      UNREACHABLE;
    }
  }
}
} // namespace IR
