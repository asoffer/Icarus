#include "ir.h"

#include <cmath>
#include <cstring>
#include <memory>

#include "../architecture.h"
#include "../ast/ast.h"
#include "../base/util.h"
#include "../scope.h"
#include "../type/type.h"

std::vector<IR::Val> global_vals;
extern std::vector<Error> errors;

void ReplEval(AST::Expression *expr) {
  auto fn = base::make_owned<IR::Func>(Func(Void, Void));
  CURRENT_FUNC(fn.get()) {
    IR::Block::Current = fn->entry();
    auto expr_val      = expr->EmitIR();
    if (!errors.empty()) {
      std::cerr << "There were " << errors.size() << " errors.";
      return;
    }

    if (expr->type != Void) { expr->type->EmitRepr(expr_val); }
    IR::Jump::Return();
  }

  IR::ExecContext ctx;
  fn->Execute({}, &ctx);
}

IR::Val Evaluate(AST::Expression *expr) {
  IR::Func *fn = nullptr;

  auto fn_ptr = base::make_owned<AST::FunctionLiteral>();
  base::owned_ptr<AST::Node> *to_release = nullptr;
  { // Wrap expression into function
    // TODO should these be at global scope? or a separate REPL scope?
    // Is the scope cleaned up?
    fn_ptr->type               = Func(Void, expr->type);
    fn_ptr->fn_scope           = Scope::Global->add_child<FnScope>();
    fn_ptr->fn_scope->fn_type  = (Function *)fn_ptr->type;
    fn_ptr->scope_             = expr->scope_;
    fn_ptr->statements         = base::make_owned<AST::Statements>();
    fn_ptr->statements->scope_ = fn_ptr->fn_scope.get();
    fn_ptr->return_type_expr   = base::make_owned<AST::Terminal>(
        expr->loc, Type_, IR::Val::Type(expr->type));
    if (expr->type != Void) {
      auto ret     = base::make_owned<AST::Unop>();
      ret->scope_  = fn_ptr->fn_scope.get();
      ret->operand = base::own(expr);
      to_release =
          reinterpret_cast<base::owned_ptr<AST::Node> *>(&ret->operand);
      ret->op         = Language::Operator::Return;
      ret->precedence = Language::precedence(Language::Operator::Return);
      fn_ptr->statements->statements.push_back(std::move(ret));
    } else {
      fn_ptr->statements->statements.push_back(base::own(expr));
      // This vector cannot change in size: there is no way code gen can add
      // statements here. Thus, it is safe to save a pointer to this last
      // element.
      to_release = &fn_ptr->statements->statements.back();
    }
  }

  CURRENT_FUNC(nullptr) { fn = fn_ptr->EmitIR().as_func; }

  std::vector<IR::Val> results;
  IR::ExecContext context;
  results = fn->Execute({}, &context);

  to_release->release();
  // TODO multiple outputs?
  return results.empty() ? IR::Val::None() : results[0];
}

namespace IR {
ExecContext::ExecContext() : stack_(50) {}

BlockIndex ExecContext::ExecuteBlock() {
  const auto &curr_block =
      call_stack.top().fn_->blocks_[call_stack.top().current_.value];
  for (const auto &cmd : curr_block.cmds_) {
    auto result = ExecuteCmd(cmd);

    if (cmd.result.kind == Val::Kind::Reg && cmd.result.type != Void) {
      ASSERT_NE(result.type, static_cast<Type *>(nullptr));
      ASSERT_EQ(result.type, cmd.result.type);

      this->reg(cmd.result.as_reg) = result;
    }
  }

  switch (curr_block.jmp_.type) {
  case Jump::Type::Uncond: return curr_block.jmp_.block_index;
  case Jump::Type::Cond: {
    Val cond_val = curr_block.jmp_.cond_data.cond;
    ASSERT_EQ(cond_val.type, Bool);
    Resolve(&cond_val);
    return cond_val.as_bool ? curr_block.jmp_.cond_data.true_block
                            : curr_block.jmp_.cond_data.false_block;
  } break;
  case Jump::Type::Ret: return BlockIndex{-1};
  case Jump::Type::None: UNREACHABLE;
  }
  UNREACHABLE;
}

IR::Val Stack::Push(Pointer *ptr) {
  size_ = Architecture::InterprettingMachine().MoveForwardToAlignment(
      ptr->pointee, size_);
  auto addr = size_;
  size_ += ptr->pointee->is<Pointer>()
               ? sizeof(Addr)
               : Architecture::InterprettingMachine().bytes(ptr->pointee);
  if (size_ > capacity_) {
    auto old_capacity = capacity_;
    capacity_         = 2 * size_;
    void *new_stack   = malloc(capacity_);
    memcpy(new_stack, stack_, old_capacity);
    free(stack_);
    stack_ = new_stack;
  }
  ASSERT(size_ <= capacity_, "");
  return IR::Val::StackAddr(addr, ptr->pointee);
}

void ExecContext::Resolve(Val *v) const {
  switch (v->kind) {
  case Val::Kind::Arg:
    ASSERT(call_stack.top().args_.size() > v->as_arg, "");
    *v = arg(v->as_arg);
    return;
  case Val::Kind::Reg: *v = reg(v->as_reg); return;
  case Val::Kind::Const: return;
  case Val::Kind::None: return;
  }
}

Val ExecContext::ExecuteCmd(const Cmd &cmd) {
  std::vector<Val> resolved = cmd.args;
  for (auto &r : resolved) { Resolve(&r); }

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
    } else if (resolved[0].type->is<Enum>()) {
      return Val::Enum(ptr_cast<Enum>(resolved[0].type),
                       resolved[0].as_enum + resolved[1].as_enum);
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
    } else if (resolved[0].type->is<Enum>()) {
      return Val::Bool(resolved[0].as_enum <= resolved[1].as_enum);
    } else {
      UNREACHABLE;
    }
  case Op::Eq:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool == resolved[1].as_bool);
    } else if (resolved[0].type == Char) {
      return Val::Bool(resolved[0].as_char == resolved[1].as_char);
    } else if (resolved[0].type == Int) {
      return Val::Bool(resolved[0].as_int == resolved[1].as_int);
    } else if (resolved[0].type == Uint) {
      return Val::Bool(resolved[0].as_uint == resolved[1].as_uint);
    } else if (resolved[0].type == Real) {
      return Val::Bool(resolved[0].as_real == resolved[1].as_real);
    } else if (resolved[0].type == Type_) {
      return Val::Bool(resolved[0].as_type == resolved[1].as_type);
    } else if (resolved[0].type->is<Pointer>()) {
      return Val::Bool(resolved[0].as_addr == resolved[1].as_addr);
    } else {
      cmd.dump(0);
      UNREACHABLE;
    }
  case Op::Ne:
    if (resolved[0].type == Bool) {
      return Val::Bool(resolved[0].as_bool != resolved[1].as_bool);
    } else if (resolved[0].type == Char) {
      return Val::Bool(resolved[0].as_char != resolved[1].as_char);
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
    call_stack.top().rets_ AT(resolved[0].as_uint) = resolved[1];
    return IR::Val::None();
  }
  case Op::Extend: return Val::Uint(static_cast<u64>(resolved[0].as_char));
  case Op::Trunc: return Val::Char(static_cast<char>(resolved[0].as_uint));
  case Op::Call: {
    auto fn = resolved.back().as_func;
    resolved.pop_back();
    auto results = fn->Execute(std::move(resolved), this);

    // TODO multiple returns?
    return results.empty() ? IR::Val::None() : results[0];
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
    } else if (resolved[0].type == Code) {
      std::cerr << *resolved[0].as_code;
    } else if (resolved[0].type == String) {
      std::cerr << resolved[0].as_cstr;
    } else if (resolved[0].type->is<Pointer>()) {
      std::cerr << resolved[0].as_addr.to_string();
    } else if (resolved[0].type->is<Enum>()) {
      std::cerr
          << ptr_cast<Enum>(resolved[0].type)->members[resolved[0].as_enum];
    } else if (resolved[0].type->is<Function>()) {
      std::cerr << "{" << resolved[0].type->to_string() << "}";
    } else {
      std::cerr << *resolved[0].type << std::endl;
      NOT_YET;
    }
    return IR::Val::None();
  case Op::Ptr: return Val::Type(::Ptr(resolved[0].as_type));
  case Op::Load:
    switch (resolved[0].as_addr.kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE;
    case Addr::Kind::Global: return global_vals[resolved[0].as_addr.as_global];
    case Addr::Kind::Stack: {
      if (cmd.result.type == Bool) {
        return IR::Val::Bool(stack_.Load<bool>(resolved[0].as_addr.as_stack));
      } else if (cmd.result.type == Char) {
        return IR::Val::Char(stack_.Load<char>(resolved[0].as_addr.as_stack));
      } else if (cmd.result.type == Int) {
        return IR::Val::Int(stack_.Load<i64>(resolved[0].as_addr.as_stack));
      } else if (cmd.result.type == Uint) {
        return IR::Val::Uint(stack_.Load<u64>(resolved[0].as_addr.as_stack));
      } else if (cmd.result.type == Real) {
        return IR::Val::Real(stack_.Load<double>(resolved[0].as_addr.as_stack));
      } else if (cmd.result.type == Code) {
        return IR::Val::CodeBlock(
            stack_.Load<AST::CodeBlock *>(resolved[0].as_addr.as_stack));

      } else if (cmd.result.type->is<Pointer>()) {
        return IR::Val::Addr(stack_.Load<Addr>(resolved[0].as_addr.as_stack),
                             ptr_cast<Pointer>(cmd.result.type)->pointee);
      } else if (cmd.result.type->is<Enum>()) {
        return IR::Val::Enum(ptr_cast<Enum>(cmd.result.type),
                             stack_.Load<size_t>(resolved[0].as_addr.as_stack));
      } else {
        std::cerr << "Don't know how to load type: " << *cmd.result.type
                  << std::endl;
        NOT_YET;
      }
    } break;
    }
  case Op::Store:
    switch (resolved[1].as_addr.kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE;
    case Addr::Kind::Global:
      global_vals[resolved[1].as_addr.as_global] = resolved[0];
      return IR::Val::None();
    case Addr::Kind::Stack:
      if (resolved[0].type == Bool) {
        stack_.Store(resolved[0].as_bool, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type == Char) {
        stack_.Store(resolved[0].as_char, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type == Int) {
        stack_.Store(resolved[0].as_int, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type == Uint) {
        stack_.Store(resolved[0].as_uint, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type == Real) {
        stack_.Store(resolved[0].as_real, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type->is<Pointer>()) {
        stack_.Store(resolved[0].as_addr, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type->is<Enum>()) {
        stack_.Store(resolved[0].as_enum, resolved[1].as_addr.as_stack);
      } else if (resolved[0].type == Code) {
        stack_.Store(resolved[0].as_code, resolved[1].as_addr.as_stack);
      } else {
        std::cerr << "Don't know how to store type: " << *cmd.result.type
                  << std::endl;
        NOT_YET;
      }

      return IR::Val::None();
    }
  case Op::Phi:
    for (size_t i = 0; i < resolved.size(); i += 2) {
      if (call_stack.top().prev_ == resolved[i].as_block) {
        return resolved[i + 1];
      }
    }
    std::cerr << "Previous block was "
              << Val::Block(call_stack.top().prev_).to_string() << std::endl;
    cmd.dump(0);
    UNREACHABLE;
  case Op::Alloca: return stack_.Push(ptr_cast<Pointer>(cmd.result.type));
  case Op::Access:
    if (resolved[1].as_addr.kind == Addr::Kind::Stack) {
      auto data_type = ptr_cast<Pointer>(cmd.result.type)->pointee;
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolved[0].as_uint, data_type);
      return Val::StackAddr(resolved[1].as_addr.as_stack + bytes_fwd,
                            data_type);
    } else {
      NOT_YET;
    }
  case Op::PtrIncr:
    if (resolved[0].as_addr.kind == Addr::Kind::Stack) {
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolved[1].as_uint, ptr_cast<Pointer>(cmd.result.type)->pointee);
      return Val::StackAddr(resolved[0].as_addr.as_stack + bytes_fwd,
                            ptr_cast<Pointer>(cmd.result.type)->pointee);
    } else {
      NOT_YET;
    }
  case Op::Field: {
    auto struct_type =
        ptr_cast<Struct>(ptr_cast<Pointer>(resolved[0].type)->pointee);
    // This can probably be precomputed.
    u64 offset = 0;
    for (u64 i = 0; i < resolved[1].as_uint; ++i) {
      auto field_type = struct_type->field_type AT(i);

      offset = Architecture::InterprettingMachine().bytes(field_type) +
               Architecture::InterprettingMachine().MoveForwardToAlignment(
                   field_type, offset);
    }

    if (resolved[0].as_addr.kind == Addr::Kind::Stack) {
      return Val::StackAddr(resolved[0].as_addr.as_stack + offset,
                            ptr_cast<Pointer>(cmd.result.type)->pointee);
    } else {
      NOT_YET;
    }
  } break;
  case Op::Contextualize: {
    // TODO this is probably the right way to encode it rather than a vector of
    // alternating entries. Same for PHI nodes.
    std::unordered_map<const AST::Expression *, IR::Val> replacements;

    for (size_t i = 0; i < resolved.size() - 1; i += 2) {
      replacements[resolved[i + 1].as_expr] = resolved[i];
    }

    ASSERT_EQ(resolved.back().type, ::Code);
    auto stmts = resolved.back().as_code->stmts->contextualize(replacements);
    auto code_block =
        base::move<AST::CodeBlock>(resolved.back().as_code->copy_stub());
    code_block->stmts = base::move<AST::Statements>(stmts);

    // TODO LEAK!
    return IR::Val::CodeBlock(std::move(code_block).release());
  }
  case Op::Nop: return Val::None();
  case Op::Malloc: NOT_YET;
  case Op::Free: NOT_YET;
  case Op::ArrayLength: NOT_YET;
  case Op::ArrayData: NOT_YET;
  }
  UNREACHABLE;
}

std::vector<Val> Func::Execute(std::vector<Val> arguments, ExecContext *ctx) {

  ctx->call_stack.emplace(this, std::move(arguments));
  while (true) {
    auto block_index = ctx->ExecuteBlock();
    if (block_index.is_none()) {
      auto rets = std::move(ctx->call_stack.top().rets_);
      ctx->call_stack.pop();
      return rets;
    } else if (block_index.value >= 0) {
      ctx->call_stack.top().prev_    = ctx->call_stack.top().current_;
      ctx->call_stack.top().current_ = block_index;
    } else {
      UNREACHABLE;
    }
  }
}
} // namespace IR
