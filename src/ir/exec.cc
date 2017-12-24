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
    fn_lit.fn_scope->fn_type  = &fn_lit.type->as<Function>();
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
    fn = base::wrap_unique(
        std::get<IR::Func *>(fn_lit.EmitTemporaryIR(kind).value));
  }

  to_release->release();
  return fn;
}

// TODO This is ugly and possibly wasteful.
static std::unique_ptr<IR::Func> AssignmentFunction(Type *from, Type *to) {
  auto assign_func = std::make_unique<IR::Func>(
      Func({Ptr(from), Ptr(to)}, Void),
      std::vector<std::pair<std::string, AST::Expression *>>{{"from", nullptr},
                                                             {"to", nullptr}});
  CURRENT_FUNC(assign_func.get()) {
    IR::Block::Current = assign_func->entry();
    to->EmitAssign(from, assign_func->Argument(0), assign_func->Argument(1));
    IR::ReturnJump();
  }
  return assign_func;
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
      ASSERT_EQ(result.type, cmd.type);
      this->reg(cmd.result) = result;
    }
  }
  return std::get<BlockIndex>(result.value);
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
  if (auto *r = std::get_if<Register>(&v->value); r != nullptr) {
    *v = reg(*r);
  }
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
      types.push_back(std::get<Type *>(val.value));
    }
    return IR::Val::Type(Var(std::move(types)));
  }
  case Op::Array: return Array(resolved[0], resolved[1]);
  case Op::Cast:
    if (resolved[1].type == Int) {
      if (std::get<Type*>(resolved[0].value) == Int) {
        return resolved[1];
      } else if (std::get<Type*>(resolved[0].value) == Uint) {
        return IR::Val::Uint(
            static_cast<u64>(std::get<i32>(resolved[1].value)));
      } else if (std::get<Type *>(resolved[0].value) == Real) {
        return IR::Val::Real(
            static_cast<double>(std::get<i32>(resolved[1].value)));
      } else {
        call_stack.top().fn_->dump();
        NOT_YET("(", resolved[0], ", ", resolved[1], ")");
      }
    } else if (resolved[1].type == Uint) {
      if (std::get<Type *>(resolved[0].value) == Uint) {
        return resolved[1];
      } else if (std::get<Type*>(resolved[0].value) == Int) {
        return IR::Val::Uint(static_cast<i32>(std::get<u64>(resolved[1].value)));
      } else if (std::get<Type*>(resolved[0].value) == Real) {
        return IR::Val::Real(
            static_cast<double>(std::get<u64>(resolved[1].value)));
      } else {
        NOT_YET();
      }
    } else if (std::get<Type*>(resolved[0].value)->is<Pointer>() &&
               resolved[1].type->is<Pointer>()) {
      return Val::Addr(std::get<Addr>(resolved[1].value),
                       std::get<Type *>(resolved[0].value));
    } else {
      call_stack.top().fn_->dump();
      cmd.dump(10);
      NOT_YET("(", resolved[0], ", ", resolved[1], ")");
    }
    break;
  case Op::Xor: return Xor(resolved[0], resolved[1]);
  case Op::Lt: return Lt(resolved[0], resolved[1]);
  case Op::Le: return Le(resolved[0], resolved[1]);
  case Op::Eq: return Eq(resolved[0], resolved[1]);
  case Op::Ne: return Ne(resolved[0], resolved[1]);
  case Op::Ge: return Ge(resolved[0], resolved[1]);
  case Op::Gt: return Gt(resolved[0], resolved[1]);
  case Op::SetReturn: {
    const auto &rets = call_stack.top().rets_;
    if (call_stack.top().fn_->type->num_outputs() == 1 &&
        !call_stack.top().fn_->type->output->is_big()) {
      call_stack.top().rets_ AT(0) = resolved[1];
    } else {
      auto type_to_be_assigned =
          rets[std::get<ReturnValue>(resolved[0].value).value]
              .type->as<Pointer>()
              .pointee;
      // TODO Use the right type for the assignment. The item being returned may
      // not actually be the same type. For instance, returning [] out of a
      // function whose output is [--; int].
      auto assignment_fn =
          AssignmentFunction(type_to_be_assigned, type_to_be_assigned);
      assignment_fn->Execute(
          {resolved[1], rets[std::get<ReturnValue>(resolved[0].value).value]},
          this, nullptr);
    }
    return IR::Val::None();
  }
  case Op::Extend: return Extend(resolved[0]);
  case Op::Trunc: return Trunc(resolved[0]);
  case Op::Call: {
    auto fn = std::get<IR::Func *>(resolved.back().value);
    resolved.pop_back();
    // There's no need to do validation here, because by virtue of executing
    // this function, we know we've already validated all functions that could
    // be called.
    auto results = fn->Execute(resolved, this, nullptr);

    // TODO multiple returns?
    return results.empty() ? IR::Val::None() : results[0];
  } break;
  case Op::Print:
    std::visit(
        base::overloaded{
            [](i32 n) { std::cerr << n; }, //
            [](u64 n) { std::cerr << n; },
            [](bool b) { std::cerr << (b ? "true" : "false"); },
            [](char c) { std::cerr << c; }, //
            [](double d) { std::cerr << d; },
            [](Type *t) { std::cerr << t->to_string(); },
            [](AST::CodeBlock *cb) { std::cerr << cb->to_string(); },
            [](const std::string &s) { std::cerr << s; },
            [](const Addr &a) { std::cerr << a.to_string(); },
            [&resolved](EnumVal e) {
              std::cerr << resolved[0].type->as<Enum>().members[e.value];
            },
            [](IR::Func *f) {
              std::cerr << "{" << f->type->to_string() << "}";
            },
            [&resolved](auto) { NOT_YET(resolved[0].type); },
        },
        resolved[0].value);
    return IR::Val::None();
  case Op::Ptr: return Ptr(resolved[0]);
  case Op::Load: {
    auto &addr = std::get<Addr>(resolved[0].value);
    switch (addr.kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE();
    case Addr::Kind::Global: return global_vals[addr.as_global];
    case Addr::Kind::Heap: {
      if (cmd.type == Bool) {
        return IR::Val::Bool(*static_cast<bool *>(addr.as_heap));
      } else if (cmd.type == Char) {
        return IR::Val::Char(*static_cast<char *>(addr.as_heap));
      } else if (cmd.type == Int) {
        return IR::Val::Int(*static_cast<i32 *>(addr.as_heap));
      } else if (cmd.type == Uint) {
        return IR::Val::Uint(*static_cast<u64 *>(addr.as_heap));
      } else if (cmd.type == Real) {
        return IR::Val::Real(*static_cast<double *>(addr.as_heap));
      } else if (cmd.type == Code) {
        return IR::Val::CodeBlock(static_cast<AST::CodeBlock *>(addr.as_heap));
      } else if (cmd.type == Type_) {
        return IR::Val::Type(*static_cast<::Type **>(addr.as_heap));
      } else if (cmd.type->is<Pointer>()) {
        return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                             cmd.type->as<Pointer>().pointee);
      } else if (cmd.type->is<Enum>()) {
        return IR::Val::Enum(&cmd.type->as<Enum>(),
                             *static_cast<size_t *>(addr.as_heap));
      } else {
        NOT_YET("Don't know how to load type: ", cmd.type);
      }
    } break;
    case Addr::Kind::Stack: {
      if (cmd.type == Bool) {
        return IR::Val::Bool(stack_.Load<bool>(addr.as_stack));
      } else if (cmd.type == Char) {
        return IR::Val::Char(stack_.Load<char>(addr.as_stack));
      } else if (cmd.type == Int) {
        return IR::Val::Int(stack_.Load<i32>(addr.as_stack));
      } else if (cmd.type == Uint) {
        return IR::Val::Uint(stack_.Load<u64>(addr.as_stack));
      } else if (cmd.type == Real) {
        return IR::Val::Real(stack_.Load<double>(addr.as_stack));
      } else if (cmd.type == Code) {
        return IR::Val::CodeBlock(stack_.Load<AST::CodeBlock *>(addr.as_stack));
      } else if (cmd.type == Type_) {
        return IR::Val::Type(stack_.Load<::Type *>(addr.as_stack));
      } else if (cmd.type->is<Pointer>()) {
        switch (addr.kind) {
        case Addr::Kind::Stack:
          return IR::Val::Addr(stack_.Load<Addr>(addr.as_stack),
                               cmd.type->as<Pointer>().pointee);
        case Addr::Kind::Heap:
          return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                               cmd.type->as<Pointer>().pointee);
        case Addr::Kind::Global: NOT_YET();
        case Addr::Kind::Null: NOT_YET();
        }
      } else if (cmd.type->is<Enum>()) {
        return IR::Val::Enum(&cmd.type->as<Enum>(),
                             stack_.Load<size_t>(addr.as_stack));
      } else {
        NOT_YET("Don't know how to load type: ", cmd.type);
      }
    } break;
    }
  } break;
  case Op::Store: {
    auto &addr = std::get<Addr>(resolved[1].value);
    switch (addr.kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      cmd.dump(0);
      UNREACHABLE();
    case Addr::Kind::Global:
      global_vals[addr.as_global] = resolved[0];
      return IR::Val::None();
    case Addr::Kind::Stack:
      if (resolved[0].type == Bool) {
        stack_.Store(std::get<bool>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type == Char) {
        stack_.Store(std::get<char>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type == Int) {
        stack_.Store(std::get<i32>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type == Uint) {
        stack_.Store(std::get<u64>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type == Real) {
        stack_.Store(std::get<double>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type->is<Pointer>()) {
        stack_.Store(std::get<Addr>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type->is<Enum>()) {
        stack_.Store(std::get<EnumVal>(resolved[0].value).value, addr.as_stack);
      } else if (resolved[0].type == Type_) {
        stack_.Store(std::get<::Type *>(resolved[0].value), addr.as_stack);
      } else if (resolved[0].type == Code) {
        stack_.Store(std::get<AST::CodeBlock *>(resolved[0].value),
                     addr.as_stack);
      } else {
        NOT_YET("Don't know how to store type: ", cmd.type);
      }

      return IR::Val::None();
    case Addr::Kind::Heap:
      if (resolved[0].type == Bool) {
        *static_cast<bool *>(addr.as_heap) = std::get<bool>(resolved[0].value);
      } else if (resolved[0].type == Char) {
        *static_cast<char *>(addr.as_heap) = std::get<char>(resolved[0].value);
      } else if (resolved[0].type == Int) {
        *static_cast<i32 *>(addr.as_heap) = std::get<i32>(resolved[0].value);
      } else if (resolved[0].type == Uint) {
        *static_cast<u64 *>(addr.as_heap) = std::get<u64>(resolved[0].value);
      } else if (resolved[0].type == Real) {
        *static_cast<double *>(addr.as_heap) =
            std::get<double>(resolved[0].value);
      } else if (resolved[0].type->is<Pointer>()) {
        *static_cast<Addr *>(addr.as_heap) = std::get<Addr>(resolved[0].value);
      } else if (resolved[0].type == Type_) {
        *static_cast<::Type **>(addr.as_heap) =
            std::get<::Type *>(resolved[0].value);
      } else if (resolved[0].type->is<Enum>()) {
        NOT_YET();
      } else if (resolved[0].type == Code) {
        NOT_YET();
      } else {
        NOT_YET("Don't know how to store type: ", cmd.type);
      }
      return IR::Val::None();
    }
  } break;
  case Op::Phi:
    for (size_t i = 0; i < resolved.size(); i += 2) {
      if (call_stack.top().prev_ == std::get<IR::BlockIndex>(resolved[i].value)) {
        return resolved[i + 1];
      }
    }
    call_stack.top().fn_->dump();
    UNREACHABLE("Previous block was ",
                Val::Block(call_stack.top().prev_).to_string());
  case Op::Alloca: return stack_.Push(&cmd.type->as<Pointer>());
  case Op::PtrIncr:
    switch (std::get<Addr>(resolved[0].value).kind) {
    case Addr::Kind::Stack: {
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          std::get<u64>(resolved[1].value), cmd.type->as<Pointer>().pointee);
      return Val::StackAddr(std::get<Addr>(resolved[0].value).as_stack +
                                bytes_fwd,
                            cmd.type->as<Pointer>().pointee);
    }
    case Addr::Kind::Heap: {
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          std::get<u64>(resolved[1].value), cmd.type->as<Pointer>().pointee);
      return Val::HeapAddr(
          static_cast<void *>(
              static_cast<char *>(std::get<Addr>(resolved[0].value).as_heap) +
              bytes_fwd),
          cmd.type->as<Pointer>().pointee);
    }
    case Addr::Kind::Global: NOT_YET();
    case Addr::Kind::Null: NOT_YET();
    }
    UNREACHABLE("Invalid address kind: ",
                static_cast<int>(std::get<Addr>(resolved[0].value).kind));
  case Op::Field: {
    auto *struct_type = &resolved[0].type->as<Pointer>().pointee->as<Struct>();
    // This can probably be precomputed.
    u64 offset = 0;
    for (u64 i = 0; i < std::get<u64>(resolved[1].value); ++i) {
      auto field_type = struct_type->field_type AT(i);

      offset = Architecture::InterprettingMachine().bytes(field_type) +
               Architecture::InterprettingMachine().MoveForwardToAlignment(
                   field_type, offset);
    }

    if (std::get<Addr>(resolved[0].value).kind == Addr::Kind::Stack) {
      return Val::StackAddr(std::get<Addr>(resolved[0].value).as_stack + offset,
                            cmd.type->as<Pointer>().pointee);
    } else {
      NOT_YET();
    }
  } break;
  case Op::Contextualize: {
    // TODO this is probably the right way to encode it rather than a vector of
    // alternating entries. Same for PHI nodes.
    std::unordered_map<const AST::Expression *, IR::Val> replacements;

    for (size_t i = 0; i < resolved.size() - 1; i += 2) {
      replacements[std::get<AST::Expression *>(resolved[i + 1].value)] =
          resolved[i];
    }

    ASSERT_EQ(resolved.back().type, ::Code);
    auto stmts = std::get<AST::CodeBlock *>(resolved.back().value)
                     ->stmts->contextualize(replacements);
    auto code_block = base::move<AST::CodeBlock>(
        std::get<AST::CodeBlock *>(resolved.back().value)->copy_stub());
    code_block->stmts = base::move<AST::Statements>(stmts);

    // TODO LEAK!
    return IR::Val::CodeBlock(code_block.release());
  } break;
  case Op::VariantType:
    return Val::Addr(std::get<Addr>(resolved[0].value), Ptr(Type_));
  case Op::VariantValue: {
    auto bytes = Architecture::InterprettingMachine().bytes(Ptr(Type_));
    auto bytes_fwd =
        Architecture::InterprettingMachine().MoveForwardToAlignment(Ptr(Type_),
                                                                    bytes);
    switch (std::get<Addr>(resolved[0].value).kind) {
    case Addr::Kind::Stack: {
      return Val::StackAddr(std::get<Addr>(resolved[0].value).as_stack +
                                bytes_fwd,
                            cmd.type->as<Pointer>().pointee);
    }
    case Addr::Kind::Heap: {
      return Val::HeapAddr(
          static_cast<void *>(
              static_cast<char *>(std::get<Addr>(resolved[0].value).as_heap) +
              bytes_fwd),
          cmd.type->as<Pointer>().pointee);
    }
    case Addr::Kind::Global: NOT_YET();
    case Addr::Kind::Null: NOT_YET();
    }
    UNREACHABLE("Invalid address kind: ",
                static_cast<int>(std::get<Addr>(resolved[0].value).kind));
  } break;
  case Op::Nop: return Val::None();
  case Op::Malloc:
    ASSERT_TYPE(Pointer, cmd.type);
    return IR::Val::HeapAddr(malloc(std::get<u64>(resolved[0].value)),
                             cmd.type->as<Pointer>().pointee);
  case Op::Free:
    free(std::get<Addr>(resolved[0].value).as_heap);
    return Val::None();
  case Op::ArrayLength:
    return IR::Val::Addr(std::get<Addr>(resolved[0].value), Uint);
  case Op::ArrayData:
    switch (std::get<Addr>(resolved[0].value).kind) {
    case Addr::Kind::Null: UNREACHABLE();
    case Addr::Kind::Global: NOT_YET();
    case Addr::Kind::Stack:
      return IR::Val::StackAddr(
          std::get<Addr>(resolved[0].value).as_stack +
              Architecture::InterprettingMachine().bytes(Uint),
          cmd.type->as<Pointer>().pointee);

    case Addr::Kind::Heap:
      return IR::Val::HeapAddr(
          static_cast<void *>(
              static_cast<u8 *>(std::get<Addr>(resolved[0].value).as_heap) +
              Architecture::InterprettingMachine().bytes(Uint)),
          cmd.type->as<Pointer>().pointee);
    }
    break;
  case Op::CondJump: return resolved[std::get<bool>(resolved[0].value) ? 1 : 2];
  case Op::UncondJump: return resolved[0];
  case Op::ReturnJump: return Val::Block(BlockIndex{-1});
  }
  UNREACHABLE();
}

std::vector<Val> Func::Execute(const std::vector<Val> &arguments,
                               ExecContext *ctx, bool *were_errors) {
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

  ctx->call_stack.emplace(this, arguments);

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
