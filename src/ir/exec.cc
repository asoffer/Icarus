#include "ir.h"

#include <cmath>

#include "../type/type.h"
#include "../ast/ast.h"
#include "../scope.h"

static AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr) {
  expr->verify_types();
  auto fn_ptr = new AST::FunctionLiteral;

  fn_ptr->type               = Func(Void, expr->type);
  fn_ptr->fn_scope->fn_type  = (Function *)fn_ptr->type;
  fn_ptr->scope_             = expr->scope_;
  fn_ptr->statements         = new AST::Statements;
  fn_ptr->statements->scope_ = fn_ptr->fn_scope;
  fn_ptr->return_type_expr   = new AST::DummyTypeExpr(expr->loc, expr->type);
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

IR::Val Evaluate(AST::Expression *expr) {
  IR::Func *fn = nullptr;
  auto fn_ptr = WrapExprIntoFunction(expr);
  CURRENT_FUNC(nullptr) { fn = fn_ptr->EmitAnonymousIR().as_func; }

  IR::LocalStack stack;
  auto result = fn->Execute(&stack, {});

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
  return result[0]; // TODO multiple outputs?
}

namespace IR {
ExecContext::ExecContext(const Func *fn) : current_fn(fn), current_block{0} {}

BlockIndex ExecContext::ExecuteBlock() {
  for (const auto &cmd : current_fn->blocks_[current_block.value].cmds_) {
    ExecuteCmd(cmd);
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
  case Val::Kind::Arg: *v = arg(v->as_arg); return;
  case Val::Kind::Reg: *v = reg(v->as_reg); return;
  case Val::Kind::Frame: *v = stack_from_frame(*v); return;
  case Val::Kind::Global: return; // TODO is this correct? unlikely
  case Val::Kind::Heap: return;
  case Val::Kind::Const: return;
  case Val::Kind::None: return;
  }
}

Val ExecContext::ExecuteCmd(const Cmd& cmd) {
  std::vector<Val> resolved = cmd.args;
  for (auto& a : args_) { Resolve(&a); }

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
    while (rets_.size() <= resolved[0].as_uint) {
      rets_.push_back(IR::Val::None());
    }

    rets_[resolved[0].as_uint] = resolved[1];
    std::cerr << resolved[1].to_string();
    return IR::Val::None();
  }
  default: NOT_YET;
  }
}

std::vector<Val> Func::Execute(LocalStack * /*stack*/, std::vector<Val>) const {
  auto ctx = ExecContext(this);
  // TODO args
  while (true) {
    auto block_index = ctx.ExecuteBlock();
    if (block_index.is_none()) {
      return {}; // std::move(ctx.outputs);
    } else if (block_index.value >= 0) {
      ctx.current_block = block_index;
    } else {
      UNREACHABLE;
    }
  }
}
} // namespace IR
