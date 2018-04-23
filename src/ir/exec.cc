#include "exec.h"

#include <cstring>
#include <iostream>
#include <memory>
#include <future>

#include "../architecture.h"
#include "../ast/ast.h"
#include "../base/guarded.h"
#include "../base/util.h"
#include "../context.h"
#include "../error/log.h"
#include "../type/all.h"
#include "func.h"

using base::check::Is;

std::vector<IR::Val> global_vals;

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

static std::unique_ptr<IR::Func> ExprFn(AST::Expression *expr, Context *ctx) {
  auto fn = std::make_unique<IR::Func>(
      ctx->mod_, type::Func({}, {expr->type}),
      std::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitIR.
    // Factor these out together.
    IR::Block::Current = fn->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).

    auto start_block = IR::Block::Current = IR::Func::Current->AddBlock();

    ASSERT(ctx != nullptr);
    ForEachExpr(expr, [ctx](size_t i, AST::Expression *e) {
      IR::SetReturn(IR::ReturnValue{static_cast<i32>(i)}, e->EmitIR(ctx));
    });
    IR::ReturnJump();

    IR::Block::Current = fn->entry();
    IR::UncondJump(start_block);
  }
  return fn;
}

// TODO This is ugly and possibly wasteful.
static std::unique_ptr<IR::Func> AssignmentFunction(const type::Type *from,
                                                    const type::Type *to) {
  // TODO is nullptr for module okay here?
  auto assign_func = std::make_unique<IR::Func>(
      nullptr, type::Func({Ptr(from), Ptr(to)}, {}),
      std::vector<std::pair<std::string, AST::Expression *>>{{"from", nullptr},
                                                             {"to", nullptr}});
  // TODO maybe we want to wire contexts through here? probably.
  // TODO get the right module
  Context ctx(nullptr); 
  CURRENT_FUNC(assign_func.get()) {
    IR::Block::Current = assign_func->entry();
    to->EmitAssign(from, assign_func->Argument(0), assign_func->Argument(1),
                   &ctx);
    IR::ReturnJump();
  }
  return assign_func;
}

namespace IR {
static std::vector<Val> Execute(Func *fn, const std::vector<Val> &arguments,
                                ExecContext *ctx) {
  if (fn->fn_lit_) { fn->fn_lit_->CompleteBody(fn->mod_); }
  ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an error.

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

ExecContext::ExecContext() : stack_(50u) {}

Block &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(Func *fn, const std::vector<Val> &arguments)
    : fn_(fn), current_(fn_->entry()), prev_(fn_->entry()),
      regs_(fn->num_regs_, Val::None()) {
  size_t num_inputs = fn->ir_type->input.size();
  ASSERT(num_inputs <= arguments.size());
  ASSERT(num_inputs <= regs_.size());
  size_t i = 0;
  for (; i < num_inputs; ++i) { regs_[i] = arguments[i]; }
  for (; i < arguments.size(); ++i) { rets_.push_back(arguments[i]); }

  if (rets_.empty() && fn->type_->output.size() == 1) {
    // This is the case of a simple return type (i.e., type can be held in
    // register).
    rets_.push_back(IR::Val::None());
  }
}

BlockIndex ExecContext::ExecuteBlock() {
  Val result;
  ASSERT(current_block().cmds_.size() > 0u);
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter);
    if (cmd_iter->type != nullptr && cmd_iter->type != type::Void) {
      ASSERT(result.type == cmd_iter->type);
      this->reg(cmd_iter->result) = result;
    }
    ++cmd_iter;
  } while (!std::holds_alternative<BlockIndex>(result.value));

  return std::get<BlockIndex>(result.value);
}

IR::Val Stack::Push(const type::Pointer *ptr) {
  size_ = Architecture::InterprettingMachine().MoveForwardToAlignment(
      ptr->pointee, size_);
  auto addr = size_;
  size_ += ptr->pointee->is<type::Pointer>()
               ? sizeof(Addr)
               : Architecture::InterprettingMachine().bytes(ptr->pointee);
  if (size_ > capacity_) {
    auto old_capacity = capacity_;
    capacity_         = 2 * size_;
    void *new_stack   = calloc(1, capacity_);
    memcpy(new_stack, stack_, old_capacity);
    free(stack_);
    stack_ = new_stack;
  }
  ASSERT(size_ <= capacity_);
  return IR::Val::StackAddr(addr, ptr->pointee);
}

void ExecContext::Resolve(Val *v) const {
  if (auto *r = std::get_if<Register>(&v->value); r != nullptr) {
    *v = reg(*r);
  }
}

Val ExecContext::ExecuteCmd(const Cmd &cmd) {
  std::vector<Val> resolved;
  resolved.reserve(cmd.args.size());
  for (const auto& arg : cmd.args) {
    if (auto *r = std::get_if<Register>(&arg.value)) {
      resolved.push_back(reg(*r));
    } else {
      resolved.push_back(arg);
    }
  }

  switch (cmd.op_code_) {
    case Op::Cast: return Cast(cmd.type, resolved[0]);
    case Op::Neg: return Neg(resolved[0]);
    case Op::Add: return Add(resolved[0], resolved[1]);
    case Op::Sub: return Sub(resolved[0], resolved[1]);
    case Op::Mul: return Mul(resolved[0], resolved[1]);
    case Op::Div: return Div(resolved[0], resolved[1]);
    case Op::Mod: return Mod(resolved[0], resolved[1]);
    case Op::Arrow: return Arrow(resolved[0], resolved[1]);
    case Op::Variant: {
      std::vector<const type::Type *> types;
      types.reserve(resolved.size());
      for (const auto &val : resolved) {
        types.push_back(std::get<const type::Type *>(val.value));
      }
      return IR::Val::Type(type::Var(std::move(types)));
    }
    case Op::Array: return Array(resolved[0], resolved[1]);
    case Op::Xor: return Xor(resolved[0], resolved[1]);
    case Op::Or: return Or(resolved[0], resolved[1]);
    case Op::And: return And(resolved[0], resolved[1]);
    case Op::Lt: return Lt(resolved[0], resolved[1]);
    case Op::Le: return Le(resolved[0], resolved[1]);
    case Op::Eq: return Eq(resolved[0], resolved[1]);
    case Op::Ne: return Ne(resolved[0], resolved[1]);
    case Op::Ge: return Ge(resolved[0], resolved[1]);
    case Op::Gt: return Gt(resolved[0], resolved[1]);
    case Op::SetReturn: {
      const auto &rets = call_stack.top().rets_;
      if (call_stack.top().fn_->type_->output.size() == 1 &&
          !call_stack.top().fn_->type_->output[0]->is_big()) {
        call_stack.top().rets_ AT(0) = resolved[1];
      } else {
        auto fn = AssignmentFunction(
            resolved[0].type->is<type::Pointer>()
                ? resolved[0].type->as<type::Pointer>().pointee
                : resolved[0].type,
            rets[std::get<ReturnValue>(resolved[0].value).value]
                .type->as<type::Pointer>()
                .pointee);
        Execute(
            fn.get(),
            {resolved[1], rets[std::get<ReturnValue>(resolved[0].value).value]},
            this);
      }
      return IR::Val::None();
    }
    case Op::Extend: return Extend(resolved[0]);
    case Op::Trunc: return Trunc(resolved[0]);
    case Op::Err:
      return IR::Val::CodeBlock(
          AST::CodeBlock(std::get<std::string>(resolved[0].value)));
    case Op::Call: {
      auto fn = std::get<IR::Func *>(resolved.back().value);
      resolved.pop_back();
      // There's no need to do validation here, because by virtue of executing
      // this function, we know we've already validated all functions that could
      // be called.

      // If there were multiple return values, they would be passed as
      // out-params in the IR.
      auto results = Execute(fn, resolved, this);
      return results.empty() ? IR::Val::None() : results[0];
    } break;
    case Op::Print:
      std::visit(
          base::overloaded{
              [](i32 n) { std::cerr << n; },
              [](bool b) { std::cerr << (b ? "true" : "false"); },
              [](char c) { std::cerr << c; },
              [](double d) { std::cerr << d; },
              [](const type::Type *t) { std::cerr << t->to_string(); },
              [](const AST::CodeBlock &cb) { std::cerr << cb.to_string(); },
              [](const std::string &s) { std::cerr << s; },
              [](const Addr &a) { std::cerr << a.to_string(); },
              [&resolved](EnumVal e) {
                if (resolved[0].type->as<type::Enum>().is_enum_) {
                  std::cerr
                      << resolved[0].type->as<type::Enum>().members_[e.value];
                } else {
                  size_t val = e.value;
                  std::vector<std::string> vals;
                  const auto &members =
                      resolved[0].type->as<type::Enum>().members_;
                  size_t i   = 0;
                  size_t pow = 1;
                  while (pow <= val) {
                    if (val & pow) { vals.push_back(members[i]); }
                    ++i;
                    pow <<= 1;
                  }
                  if (vals.empty()) {
                    std::cerr << "(empty)";
                  } else {
                    auto iter = vals.begin();
                    std::cerr << *iter++;
                    while (iter != vals.end()) {
                      std::cerr << " | " << *iter++;
                    }
                  }
                }
              },
              [](IR::Func *f) {
                std::cerr << "{" << f->type_->to_string() << "}";
              },
              [](AST::GenericFunctionLiteral *f) {
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
#define LOAD_FROM_HEAP(lang_type, ctor, cpp_type)                              \
  if (cmd.type == lang_type) {                                                 \
    return IR::Val::ctor(*static_cast<cpp_type *>(addr.as_heap));              \
  }
          LOAD_FROM_HEAP(type::Bool, Bool, bool);
          LOAD_FROM_HEAP(type::Char, Char, char);
          LOAD_FROM_HEAP(type::Int, Int, i32);
          LOAD_FROM_HEAP(type::Real, Real, double);
          LOAD_FROM_HEAP(type::Code, CodeBlock, AST::CodeBlock);
          LOAD_FROM_HEAP(type::Type_, Type, const type::Type *);
          if (cmd.type->is<type::Pointer>()) {
            return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                 cmd.type->as<type::Pointer>().pointee);
          } else if (cmd.type->is<type::Enum>()) {
            return IR::Val::Enum(&cmd.type->as<type::Enum>(),
                                 *static_cast<size_t *>(addr.as_heap));
          } else {
            NOT_YET("Don't know how to load type: ", cmd.type);
          }
#undef LOAD_FROM_HEAP
        } break;
        case Addr::Kind::Stack: {
#define LOAD_FROM_STACK(lang_type, ctor, cpp_type)                             \
  if (cmd.type == lang_type) {                                                 \
    return IR::Val::ctor(stack_.Load<cpp_type>(addr.as_stack));                \
  }
          LOAD_FROM_STACK(type::Bool, Bool, bool);
          LOAD_FROM_STACK(type::Char, Char, char);
          LOAD_FROM_STACK(type::Int, Int, i32);
          LOAD_FROM_STACK(type::Real, Real, double);
          LOAD_FROM_STACK(type::Code, CodeBlock, AST::CodeBlock);
          LOAD_FROM_STACK(type::String, StrLit, std::string);
          LOAD_FROM_STACK(type::Type_, Type, const type::Type *);
          if (cmd.type->is<type::Pointer>()) {
            switch (addr.kind) {
              case Addr::Kind::Stack:
                return IR::Val::Addr(stack_.Load<Addr>(addr.as_stack),
                                     cmd.type->as<type::Pointer>().pointee);
              case Addr::Kind::Heap:
                return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                     cmd.type->as<type::Pointer>().pointee);
              case Addr::Kind::Global: NOT_YET();
              case Addr::Kind::Null: NOT_YET();
            }
          } else if (cmd.type->is<type::Enum>()) {
            return IR::Val::Enum(&cmd.type->as<type::Enum>(),
                                 stack_.Load<size_t>(addr.as_stack));
          } else {
            call_stack.top().fn_->dump();
            NOT_YET("Don't know how to load type: ", cmd.type);
          }
#undef LOAD_FROM_STACK
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
          if (resolved[0].type == type::Bool) {
            stack_.Store(std::get<bool>(resolved[0].value), addr.as_stack);
          } else if (resolved[0].type == type::Char) {
            stack_.Store(std::get<char>(resolved[0].value), addr.as_stack);
          } else if (resolved[0].type == type::Int) {
            stack_.Store(std::get<i32>(resolved[0].value), addr.as_stack);
          } else if (resolved[0].type == type::Real) {
            stack_.Store(std::get<double>(resolved[0].value), addr.as_stack);
          } else if (resolved[0].type->is<type::Pointer>()) {
            stack_.Store(std::get<Addr>(resolved[0].value), addr.as_stack);
          } else if (resolved[0].type->is<type::Enum>()) {
            stack_.Store(std::get<EnumVal>(resolved[0].value).value,
                         addr.as_stack);
          } else if (resolved[0].type == type::Type_) {
            stack_.Store(std::get<const type::Type *>(resolved[0].value),
                         addr.as_stack);
          } else if (resolved[0].type == type::Code) {
            stack_.Store(std::get<AST::CodeBlock>(resolved[0].value),
                         addr.as_stack);
          } else if (resolved[0].type == type::String) {
            stack_.Store(std::get<std::string>(resolved[0].value),
                         addr.as_stack);
          } else {
            NOT_YET("Don't know how to store that: args = ", resolved);
          }

          return IR::Val::None();
        case Addr::Kind::Heap:
          if (resolved[0].type == type::Bool) {
            *static_cast<bool *>(addr.as_heap) =
                std::get<bool>(resolved[0].value);
          } else if (resolved[0].type == type::Char) {
            *static_cast<char *>(addr.as_heap) =
                std::get<char>(resolved[0].value);
          } else if (resolved[0].type == type::Int) {
            *static_cast<i32 *>(addr.as_heap) =
                std::get<i32>(resolved[0].value);
          } else if (resolved[0].type == type::Real) {
            *static_cast<double *>(addr.as_heap) =
                std::get<double>(resolved[0].value);
          } else if (resolved[0].type->is<type::Pointer>()) {
            *static_cast<Addr *>(addr.as_heap) =
                std::get<Addr>(resolved[0].value);
          } else if (resolved[0].type == type::Type_) {
            *static_cast<const type::Type **>(addr.as_heap) =
                std::get<const type::Type *>(resolved[0].value);
          } else if (resolved[0].type->is<type::Enum>()) {
            NOT_YET();
          } else if (resolved[0].type == type::Code) {
            NOT_YET();
          } else {
            NOT_YET("Don't know how to store that: args = ", resolved);
          }
          return IR::Val::None();
      }
    } break;
    case Op::Phi:
      for (size_t i = 0; i < resolved.size(); i += 2) {
        if (call_stack.top().prev_ ==
            std::get<IR::BlockIndex>(resolved[i].value)) {
          return resolved[i + 1];
        }
      }
      call_stack.top().fn_->dump();
      UNREACHABLE("Previous block was ", Val::Block(call_stack.top().prev_),
                  "\nCurrent block is ", Val::Block(call_stack.top().current_));
    case Op::Alloca: return stack_.Push(&cmd.type->as<type::Pointer>());
    case Op::PtrIncr:
      switch (std::get<Addr>(resolved[0].value).kind) {
        case Addr::Kind::Stack: {
          auto bytes_fwd =
              Architecture::InterprettingMachine().ComputeArrayLength(
                  std::get<i32>(resolved[1].value),
                  cmd.type->as<type::Pointer>().pointee);
          return Val::StackAddr(
              std::get<Addr>(resolved[0].value).as_stack + bytes_fwd,
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Heap: {
          auto bytes_fwd =
              Architecture::InterprettingMachine().ComputeArrayLength(
                  std::get<i32>(resolved[1].value),
                  cmd.type->as<type::Pointer>().pointee);
          return Val::HeapAddr(
              static_cast<void *>(
                  static_cast<char *>(
                      std::get<Addr>(resolved[0].value).as_heap) +
                  bytes_fwd),
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Global: NOT_YET();
        case Addr::Kind::Null: NOT_YET();
      }
      UNREACHABLE("Invalid address kind: ",
                  static_cast<int>(std::get<Addr>(resolved[0].value).kind));
    case Op::CreateStruct: {
      return IR::Val::Struct();
    } break;
    case Op::InsertField: {
      auto *struct_to_mod = std::get<type::Struct *>(resolved[0].value);
      struct_to_mod->fields_.push_back(type::Struct::Field{
          std::get<std::string>(resolved[1].value),
          std::get<const type::Type *>(resolved[2].value), resolved[3]});

      auto[iter, success] = struct_to_mod->field_indices_.emplace(
          std::get<std::string>(resolved[1].value),
          struct_to_mod->fields_.size() - 1);
      ASSERT(success);

      return IR::Val::None();
    } break;
    case Op::FinalizeStruct: {
      return IR::Val::Type(
          std::get<type::Struct *>(resolved[0].value)->finalize());
    } break;
    case Op::Field: {
      auto *struct_type =
          &resolved[0].type->as<type::Pointer>().pointee->as<type::Struct>();
      // This can probably be precomputed.
      size_t offset = 0;
      for (i32 i = 0; i < std::get<i32>(resolved[1].value); ++i) {
        auto field_type = struct_type->fields_ AT(i).type;
        offset += Architecture::InterprettingMachine().bytes(field_type);
        offset = Architecture::InterprettingMachine().MoveForwardToAlignment(
            struct_type->fields_ AT(i + 1).type, offset);
      }

      if (std::get<Addr>(resolved[0].value).kind == Addr::Kind::Stack) {
        return Val::StackAddr(
            std::get<Addr>(resolved[0].value).as_stack + offset,
            cmd.type->as<type::Pointer>().pointee);
      } else {
        NOT_YET();
      }
    } break;
    case Op::Contextualize: {
      // TODO this is probably the right way to encode it rather than a vector
      // of alternating entries. Same for PHI nodes.
      std::unordered_map<const AST::Expression *, IR::Val> replacements;
      for (size_t i = 0; i < resolved.size() - 1; i += 2) {
        replacements[std::get<AST::Expression *>(resolved[i + 1].value)] =
            resolved[i];
      }

      ASSERT(cmd.args.back().type == type::Code);
      const auto &code_block = std::get<AST::CodeBlock>(cmd.args.back().value);
      auto copied_block      = code_block;
      std::get<AST::Statements>(copied_block.content_)
          .contextualize(&std::get<AST::Statements>(code_block.content_),
                         replacements);
      return IR::Val::CodeBlock(std::move(copied_block));
    } break;
    case Op::VariantType:
      return Val::Addr(std::get<Addr>(resolved[0].value), type::Type_);
    case Op::VariantValue: {
      auto bytes = Architecture::InterprettingMachine().bytes(Ptr(type::Type_));
      auto bytes_fwd =
          Architecture::InterprettingMachine().MoveForwardToAlignment(
              Ptr(type::Type_), bytes);
      ASSERT(std::get_if<Addr>(&resolved[0].value) != nullptr);
      switch (std::get<Addr>(resolved[0].value).kind) {
        case Addr::Kind::Stack: {
          return Val::StackAddr(
              std::get<Addr>(resolved[0].value).as_stack + bytes_fwd,
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Heap: {
          return Val::HeapAddr(
              static_cast<void *>(
                  static_cast<char *>(
                      std::get<Addr>(resolved[0].value).as_heap) +
                  bytes_fwd),
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Global: NOT_YET();
        case Addr::Kind::Null: NOT_YET();
      }
      UNREACHABLE("Invalid address kind: ",
                  static_cast<int>(std::get<Addr>(resolved[0].value).kind));
    } break;
    case Op::Malloc:
      ASSERT(cmd.type, Is<type::Pointer>());
      return IR::Val::HeapAddr(malloc(std::get<i32>(resolved[0].value)),
                               cmd.type->as<type::Pointer>().pointee);
    case Op::Free:
      free(std::get<Addr>(resolved[0].value).as_heap);
      return Val::None();
    case Op::ArrayLength:
      return IR::Val::Addr(std::get<Addr>(resolved[0].value), type::Int);
    case Op::ArrayData:
      switch (std::get<Addr>(resolved[0].value).kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Global: NOT_YET();
        case Addr::Kind::Stack:
          return IR::Val::StackAddr(
              std::get<Addr>(resolved[0].value).as_stack +
                  Architecture::InterprettingMachine().bytes(type::Int),
              cmd.type->as<type::Pointer>().pointee);

        case Addr::Kind::Heap:
          return IR::Val::HeapAddr(
              static_cast<void *>(
                  static_cast<u8 *>(std::get<Addr>(resolved[0].value).as_heap) +
                  Architecture::InterprettingMachine().bytes(type::Int)),
              cmd.type->as<type::Pointer>().pointee);
      }
      break;
    case Op::CondJump:
      return resolved[std::get<bool>(resolved[0].value) ? 1 : 2];
    case Op::UncondJump: return resolved[0];
    case Op::ReturnJump: return Val::Block(BlockIndex{-1});
  }
  UNREACHABLE();
}
} // namespace IR

void ReplEval(AST::Expression *expr) {
  // TODO is nullptr for module okay here?
  auto fn = std::make_unique<IR::Func>(
      nullptr, type::Func({}, {}),
      std::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    IR::Block::Current = fn->entry();
    // TODO use the right module
    Context ctx(nullptr);
    auto expr_val      = expr->EmitIR(&ctx);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return;
    }

    if (expr->type != type::Void) { expr->type->EmitRepr(expr_val, &ctx); }
    IR::ReturnJump();
  }

  IR::ExecContext ctx;
  Execute(fn.get(), {}, &ctx);
}

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx) {
  IR::ExecContext exec_context;
  // TODO wire through errors.
  auto fn = ExprFn(expr, ctx);
  if (ctx->num_errors() == 0) {
    return Execute(fn.get(), {}, &exec_context);
  } else {
    return {};
  }
}
