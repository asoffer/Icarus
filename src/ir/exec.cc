#include "ir.h"

#include <cstring>
#include <memory>

#include "../architecture.h"
#include "../ast/ast.h"
#include "../base/util.h"
#include "../error_log.h"
#include "../scope.h"
#include "../type/type.h"

std::vector<IR::Val> global_vals;

namespace Language {
extern size_t precedence(Operator op);
} // namespace Language

std::unique_ptr<IR::Func> ExprFn(AST::Expression *expr, Type *input_type,
                                 IR::Cmd::Kind kind) {
  std::unique_ptr<IR::Func> fn = nullptr;
  AST::FunctionLiteral fn_lit;
  base::owned_ptr<AST::Node> *to_release = nullptr;
  { // Wrap expression into function
    // TODO should these be at global scope? or a separate REPL scope?
    // Is the scope cleaned up?
    fn_lit.type = Func(input_type != nullptr ? input_type : Void, expr->type);
    fn_lit.fn_scope           = Scope::Global->add_child<FnScope>();
    fn_lit.fn_scope->fn_type  = ptr_cast<Function>(fn_lit.type);
    fn_lit.scope_             = expr->scope_;
    fn_lit.statements         = base::make_owned<AST::Statements>();
    fn_lit.statements->scope_ = fn_lit.fn_scope.get();
    fn_lit.return_type_expr =
        base::make_owned<AST::Terminal>(expr->span, IR::Val::Type(expr->type));
    if (expr->type != Void) {
      auto ret     = base::make_owned<AST::Unop>();
      ret->scope_  = fn_lit.fn_scope.get();
      ret->operand = base::own(expr);
      to_release =
          reinterpret_cast<base::owned_ptr<AST::Node> *>(&ret->operand);
      ret->op         = Language::Operator::Return;
      ret->precedence = Language::precedence(Language::Operator::Return);
      fn_lit.statements->statements.push_back(std::move(ret));
    } else {
      fn_lit.statements->statements.push_back(base::own(expr));
      // This vector cannot change in size: there is no way code gen can add
      // statements here. Thus, it is safe to save a pointer to this last
      // element.
      to_release = &fn_lit.statements->statements.back();
    }
  }

  CURRENT_FUNC(nullptr) {
    fn = base::wrap_unique(fn_lit.EmitTemporaryIR(kind).value.as<IR::Func *>());
  }

  to_release->release();
  return fn;
}

void ReplEval(AST::Expression *expr) {
  auto fn = base::make_owned<IR::Func>(
      Func(Void, Void),
      std::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    IR::Block::Current = fn->entry();
    auto expr_val      = expr->EmitIR(IR::Cmd::Kind::Exec);
    if (ErrorLog::NumErrors() != 0) {
      ErrorLog::Dump();
      return;
    }

    if (expr->type != Void) { expr->type->EmitRepr(expr_val); }
    IR::ReturnJump();
  }

  IR::ExecContext ctx;
  bool were_errors;
  fn->Execute({}, &ctx, &were_errors);
}

IR::Val Evaluate(AST::Expression *expr) {
  std::vector<IR::Val> results;
  IR::ExecContext context;
  bool were_errors;
  // TODO need to count errors locally.
  if (ErrorLog::NumErrors() != 0) {
    ErrorLog::Dump();
    return IR::Val::None();
  }
  auto fn = ExprFn(expr, nullptr, IR::Cmd::Kind::Exec);
  results = fn->Execute({}, &context, &were_errors);
  // TODO wire through errors. Currently we just return IR::Val::None() if there
  // were errors

  // TODO multiple outputs?
  return results.empty() ? IR::Val::None() : results[0];
}

namespace IR {
ExecContext::ExecContext() : stack_(50) {}

BlockIndex ExecContext::ExecuteBlock() {
  Val result;
  for (const auto &cmd : current_block().cmds_) {
    result = ExecuteCmd(cmd);
    if (cmd.type != nullptr && cmd.type != Void) {
      ASSERT_NE(result.type, static_cast<Type *>(nullptr));
      ASSERT_EQ(result.type, cmd.type);

      this->reg(cmd.result) = result;
    }
  }

  ASSERT(result.value.is<BlockIndex>(), "");
  return result.value.as<BlockIndex>();
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
  ASSERT_LE(size_, capacity_);
  return IR::Val::StackAddr(addr, ptr->pointee);
}

void ExecContext::Resolve(Val *v) const {
  if (v->value.is<Register>()) { *v = reg(v->value.as<Register>()); }
}

Val ExecContext::ExecuteCmd(const Cmd &cmd) {
  std::vector<Val> resolved = cmd.args;
  for (auto &r : resolved) { Resolve(&r); }

  switch (cmd.op_code_) {
  case Op::Neg: return Neg(resolved[0]);
  case Op::Add: return Add(resolved[0], resolved[1]);
  case Op::Sub: return Sub(resolved[0], resolved[1]);
  case Op::Mul: return Mul(resolved[0], resolved[1]);
  case Op::Div: return Div(resolved[0], resolved[1]);
  case Op::Mod: return Mod(resolved[0], resolved[1]);
  case Op::Arrow: return Arrow(resolved[0], resolved[1]);
  case Op::Variant: {
    std::vector<Type *> types;
    types.reserve(resolved.size());
    for (const auto &val : resolved) {
      types.push_back(val.value.as<Type *>());
    }
    return IR::Val::Type(Var(std::move(types)));
  }
  case Op::Array: return Array(resolved[0], resolved[1]);
  case Op::Cast:
    if (resolved[1].type == Int) {
      if (resolved[0].value.as<Type *>() == Int) {
        return resolved[1];
      } else if (resolved[0].value.as<Type *>() == Uint) {
        return IR::Val::Uint(static_cast<u64>(resolved[1].value.as<i32>()));
      } else if (resolved[0].value.as<Type *>() == Real) {
        return IR::Val::Real(static_cast<double>(resolved[1].value.as<i32>()));
      } else {
        NOT_YET();
      }
    } else if (resolved[1].type == Uint) {
      if (resolved[0].value.as<Type *>() == Uint) {
        return resolved[1];
      } else if (resolved[0].value.as<Type *>() == Int) {
        return IR::Val::Uint(static_cast<i32>(resolved[1].value.as<u64>()));
      } else if (resolved[0].value.as<Type *>() == Real) {
        return IR::Val::Real(static_cast<double>(resolved[1].value.as<u64>()));
      } else {
        NOT_YET();
      }
    } else {
      NOT_YET();
    }
  case Op::Xor: return Xor(resolved[0], resolved[1]);
  case Op::Lt: return Lt(resolved[0], resolved[1]);
  case Op::Le: return Le(resolved[0], resolved[1]);
  case Op::Eq: return Eq(resolved[0], resolved[1]);
  case Op::Ne: return Ne(resolved[0], resolved[1]);
  case Op::Ge: return Ge(resolved[0], resolved[1]);
  case Op::Gt: return Gt(resolved[0], resolved[1]);
  case Op::SetReturn: {
    call_stack.top().rets_ AT(resolved[0].value.as<IR::ReturnValue>().value) =
        resolved[1];
    return IR::Val::None();
  }
  case Op::Extend: return Extend(resolved[0]);
  case Op::Trunc: return Trunc(resolved[0]);
  case Op::Call: {
    auto fn = resolved.back().value.as<IR::Func *>();
    resolved.pop_back();
    // There's no need to do validation here, because by virtue of executing
    // this function, we know we've already validated all functions that could
    // be called.
    auto results = fn->Execute(std::move(resolved), this, nullptr);

    // TODO multiple returns?
    return results.empty() ? IR::Val::None() : results[0];
  } break;
  case Op::Print:
    if (resolved[0].type == Int) {
      std::cerr << resolved[0].value.as<i32>();
    } else if (resolved[0].type == Uint) {
      std::cerr << resolved[0].value.as<u64>();
    } else if (resolved[0].type == Bool) {
      std::cerr << (resolved[0].value.as<bool>() ? "true" : "false");
    } else if (resolved[0].type == Char) {
      std::cerr << resolved[0].value.as<char>();
    } else if (resolved[0].type == Real) {
      std::cerr << resolved[0].value.as<double>();
    } else if (resolved[0].type == Type_) {
      std::cerr << resolved[0].value.as<Type *>()->to_string();
    } else if (resolved[0].type == Code) {
      std::cerr << *resolved[0].value.as<AST::CodeBlock *>();
    } else if (resolved[0].type == String) {
      std::cerr << resolved[0].value.as<std::string>();
    } else if (resolved[0].type->is<Pointer>()) {
      std::cerr << resolved[0].value.as<Addr>().to_string();
    } else if (resolved[0].type->is<Enum>()) {
      std::cerr << ptr_cast<Enum>(resolved[0].type)
                       ->members[resolved[0].value.as<EnumVal>().value];
    } else if (resolved[0].type->is<Function>()) {
      std::cerr << "{" << resolved[0].type->to_string() << "}";
    } else {
      NOT_YET(*resolved[0].type);
    }
    return IR::Val::None();
  case Op::Ptr: return Ptr(resolved[0]);
  case Op::Load:
    switch (resolved[0].value.as<Addr>().kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE();
    case Addr::Kind::Global:
      return global_vals[resolved[0].value.as<Addr>().as_global];
    case Addr::Kind::Heap: {
      if (cmd.type == Bool) {
        return IR::Val::Bool(
            *static_cast<bool *>(resolved[0].value.as<Addr>().as_heap));
      } else if (cmd.type == Char) {
        return IR::Val::Char(
            *static_cast<char *>(resolved[0].value.as<Addr>().as_heap));
      } else if (cmd.type == Int) {
        return IR::Val::Int(
            *static_cast<i32 *>(resolved[0].value.as<Addr>().as_heap));
      } else if (cmd.type == Uint) {
        return IR::Val::Uint(
            *static_cast<u64 *>(resolved[0].value.as<Addr>().as_heap));
      } else if (cmd.type == Real) {
        return IR::Val::Real(
            *static_cast<double *>(resolved[0].value.as<Addr>().as_heap));
      } else if (cmd.type == Code) {
        return IR::Val::CodeBlock(static_cast<AST::CodeBlock *>(
            resolved[0].value.as<Addr>().as_heap));

      } else if (cmd.type->is<Pointer>()) {
        return IR::Val::Addr(
            *static_cast<Addr *>(resolved[0].value.as<Addr>().as_heap),
            ptr_cast<Pointer>(cmd.type)->pointee);
      } else if (cmd.type->is<Enum>()) {
        return IR::Val::Enum(
            ptr_cast<Enum>(cmd.type),
            *static_cast<size_t *>(resolved[0].value.as<Addr>().as_heap));
      } else {
        call_stack.top().fn_->dump();
        cmd.dump(0);
        NOT_YET("Don't know how to load type: ", *cmd.type);
      }
    } break;
    case Addr::Kind::Stack: {
      if (cmd.type == Bool) {
        return IR::Val::Bool(
            stack_.Load<bool>(resolved[0].value.as<Addr>().as_stack));
      } else if (cmd.type == Char) {
        return IR::Val::Char(
            stack_.Load<char>(resolved[0].value.as<Addr>().as_stack));
      } else if (cmd.type == Int) {
        return IR::Val::Int(
            stack_.Load<i32>(resolved[0].value.as<Addr>().as_stack));
      } else if (cmd.type == Uint) {
        return IR::Val::Uint(
            stack_.Load<u64>(resolved[0].value.as<Addr>().as_stack));
      } else if (cmd.type == Real) {
        return IR::Val::Real(
            stack_.Load<double>(resolved[0].value.as<Addr>().as_stack));
      } else if (cmd.type == Code) {
        return IR::Val::CodeBlock(stack_.Load<AST::CodeBlock *>(
            resolved[0].value.as<Addr>().as_stack));

      } else if (cmd.type->is<Pointer>()) {
        switch (resolved[0].value.as<Addr>().kind) {
        case Addr::Kind::Stack:
          return IR::Val::Addr(
              stack_.Load<Addr>(resolved[0].value.as<Addr>().as_stack),
              ptr_cast<Pointer>(cmd.type)->pointee);
        case Addr::Kind::Heap:
          return IR::Val::Addr(
              *static_cast<Addr *>(resolved[0].value.as<Addr>().as_heap),
              ptr_cast<Pointer>(cmd.type)->pointee);
        case Addr::Kind::Global: NOT_YET();
        case Addr::Kind::Null: NOT_YET();
        }
      } else if (cmd.type->is<Enum>()) {
        return IR::Val::Enum(
            ptr_cast<Enum>(cmd.type),
            stack_.Load<size_t>(resolved[0].value.as<Addr>().as_stack));
      } else {
        NOT_YET("Don't know how to load type: ", *cmd.type);
      }
    } break;
    }
    break;
  case Op::Store:
    switch (resolved[1].value.as<Addr>().kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      cmd.dump(0);
      UNREACHABLE();
    case Addr::Kind::Global:
      global_vals[resolved[1].value.as<Addr>().as_global] = resolved[0];
      return IR::Val::None();
    case Addr::Kind::Stack:
      if (resolved[0].type == Bool) {
        stack_.Store(resolved[0].value.as<bool>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type == Char) {
        stack_.Store(resolved[0].value.as<char>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type == Int) {
        stack_.Store(resolved[0].value.as<i32>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type == Uint) {
        stack_.Store(resolved[0].value.as<u64>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type == Real) {
        stack_.Store(resolved[0].value.as<double>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type->is<Pointer>()) {
        stack_.Store(resolved[0].value.as<Addr>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type->is<Enum>()) {
        stack_.Store(resolved[0].value.as<EnumVal>().value,
                     resolved[1].value.as<Addr>().as_stack);
      } else if (resolved[0].type == Code) {
        stack_.Store(resolved[0].value.as<AST::CodeBlock *>(),
                     resolved[1].value.as<Addr>().as_stack);
      } else {
        NOT_YET("Don't know how to store type: ", *cmd.type);
      }

      return IR::Val::None();
    case Addr::Kind::Heap:
      if (resolved[0].type == Bool) {
        *static_cast<bool *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<bool>();
      } else if (resolved[0].type == Char) {
        *static_cast<char *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<char>();
      } else if (resolved[0].type == Int) {
        *static_cast<i32 *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<i32>();
      } else if (resolved[0].type == Uint) {
        *static_cast<u64 *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<u64>();
      } else if (resolved[0].type == Real) {
        *static_cast<double *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<double>();
      } else if (resolved[0].type->is<Pointer>()) {
        *static_cast<Addr *>(resolved[1].value.as<Addr>().as_heap) =
            resolved[0].value.as<Addr>();
      } else if (resolved[0].type->is<Enum>()) {
        NOT_YET();
      } else if (resolved[0].type == Code) {
        NOT_YET();
      } else {
        NOT_YET("Don't know how to store type: ", *cmd.type);
      }
      return IR::Val::None();
    }
  case Op::Phi:
    for (size_t i = 0; i < resolved.size(); i += 2) {
      if (call_stack.top().prev_ == resolved[i].value.as<IR::BlockIndex>()) {
        return resolved[i + 1];
      }
    }
    call_stack.top().fn_->dump();
    UNREACHABLE("Previous block was ",
                Val::Block(call_stack.top().prev_).to_string());
  case Op::Alloca: return stack_.Push(ptr_cast<Pointer>(cmd.type));
  case Op::PtrIncr:
    switch (resolved[0].value.as<Addr>().kind) {
    case Addr::Kind::Stack: {
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolved[1].value.as<u64>(), ptr_cast<Pointer>(cmd.type)->pointee);
      return Val::StackAddr(resolved[0].value.as<Addr>().as_stack + bytes_fwd,
                            ptr_cast<Pointer>(cmd.type)->pointee);
    }
    case Addr::Kind::Heap: {
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolved[1].value.as<u64>(), ptr_cast<Pointer>(cmd.type)->pointee);
      return Val::HeapAddr(
          static_cast<void *>(
              static_cast<char *>(resolved[0].value.as<Addr>().as_heap) +
              bytes_fwd),
          ptr_cast<Pointer>(cmd.type)->pointee);
    }
    case Addr::Kind::Global: NOT_YET();
    case Addr::Kind::Null: NOT_YET();
    }
    UNREACHABLE("Invalid address kind: ",
                static_cast<int>(resolved[0].value.as<Addr>().kind));
  case Op::Field: {
    auto struct_type =
        ptr_cast<Struct>(ptr_cast<Pointer>(resolved[0].type)->pointee);
    // This can probably be precomputed.
    u64 offset = 0;
    for (u64 i = 0; i < resolved[1].value.as<u64>(); ++i) {
      auto field_type = struct_type->field_type AT(i);

      offset = Architecture::InterprettingMachine().bytes(field_type) +
               Architecture::InterprettingMachine().MoveForwardToAlignment(
                   field_type, offset);
    }

    if (resolved[0].value.as<Addr>().kind == Addr::Kind::Stack) {
      return Val::StackAddr(resolved[0].value.as<Addr>().as_stack + offset,
                            ptr_cast<Pointer>(cmd.type)->pointee);
    } else {
      NOT_YET();
    }
  } break;
  case Op::Contextualize: {
    // TODO this is probably the right way to encode it rather than a vector of
    // alternating entries. Same for PHI nodes.
    std::unordered_map<const AST::Expression *, IR::Val> replacements;

    for (size_t i = 0; i < resolved.size() - 1; i += 2) {
      replacements[resolved[i + 1].value.as<AST::Expression *>()] = resolved[i];
    }

    ASSERT_EQ(resolved.back().type, ::Code);
    auto stmts =
        resolved.back().value.as<AST::CodeBlock *>()->stmts->contextualize(
            replacements);
    auto code_block = base::move<AST::CodeBlock>(
        resolved.back().value.as<AST::CodeBlock *>()->copy_stub());
    code_block->stmts = base::move<AST::Statements>(stmts);

    // TODO LEAK!
    return IR::Val::CodeBlock(std::move(code_block).release());
  }
  case Op::Nop: return Val::None();
  case Op::Malloc:
    ASSERT_TYPE(Pointer, cmd.type);
    return IR::Val::HeapAddr(malloc(resolved[0].value.as<u64>()),
                             ptr_cast<Pointer>(cmd.type)->pointee);
  case Op::Free: free(resolved[0].value.as<Addr>().as_heap); return Val::None();
  case Op::ArrayLength:
    return IR::Val::Addr(resolved[0].value.as<Addr>(), Uint);
  case Op::ArrayData:
    switch (resolved[0].value.as<Addr>().kind) {
    case Addr::Kind::Null: UNREACHABLE();
    case Addr::Kind::Global: NOT_YET();
    case Addr::Kind::Stack:
      return IR::Val::StackAddr(
          resolved[0].value.as<Addr>().as_stack +
              Architecture::InterprettingMachine().bytes(Uint),
          ptr_cast<Pointer>(cmd.type)->pointee);

    case Addr::Kind::Heap:
      return IR::Val::HeapAddr(
          static_cast<void *>(
              static_cast<u8 *>(resolved[0].value.as<Addr>().as_heap) +
              Architecture::InterprettingMachine().bytes(Uint)),
          ptr_cast<Pointer>(cmd.type)->pointee);
    }
  case Op::CondJump: return resolved[resolved[0].value.as<bool>() ? 1 : 2];
  case Op::UncondJump: return resolved[0];
  case Op::ReturnJump: return Val::Block(BlockIndex{-1});
  }
  UNREACHABLE();
}

std::vector<Val> Func::Execute(std::vector<Val> arguments, ExecContext *ctx,
                               bool *were_errors) {
  if (were_errors != nullptr) {
    int num_errors = 0;
    std::queue<Func *> validation_queue;
    validation_queue.push(this);
    while (!validation_queue.empty()) {
      auto fn = std::move(validation_queue.front());
      validation_queue.pop();
      num_errors += fn->ValidateCalls(&validation_queue);
    }
    if (num_errors > 0) {
      *were_errors = true;
      return {};
    }
  }

  ctx->call_stack.emplace(this, std::move(arguments));

  while (true) {
    auto block_index = ctx->ExecuteBlock();
    if (block_index.is_default()) {
      auto rets = std::move(ctx->call_stack.top().rets_);
      ctx->call_stack.pop();
      return rets;
    } else {
      ctx->call_stack.top().MoveTo(block_index);
    }
  }
}
} // namespace IR
