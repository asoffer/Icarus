#include "backend/exec.h"

#include <cstring>
#include <future>
#include <iostream>
#include <memory>

#include "architecture.h"
#include "ast/codeblock.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "base/util.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"

using base::check::Is;

std::vector<IR::Val> global_vals;

namespace IR {
std::vector<Val> Execute(Func *fn, const std::vector<Val> &arguments,
                         ExecContext *ctx) {
  if (fn->gened_fn_) { fn->gened_fn_->CompleteBody(fn->mod_); }
  ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an error.

  while (true) {
    auto block_index = ctx->ExecuteBlock();
    if (block_index.is_default()) {
      std::vector<IR::Val> rets;
      auto *fn_type = ctx->call_stack.top().fn_->type_;
      for (size_t i = fn_type->input.size();
           i < fn_type->input.size() + fn_type->output.size(); ++i) {
        rets.push_back(std::move(ctx->reg(Register(i))));
      }
      ctx->call_stack.pop();
      return rets;
    } else {
      ctx->call_stack.top().MoveTo(block_index);
    }
  }
}

ExecContext::ExecContext() : stack_(50u) {}

BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(Func *fn, const std::vector<Val> &arguments)
    : fn_(fn), current_(fn_->entry()), prev_(fn_->entry()),
      regs_(fn->num_regs_, Val::None()) {
  size_t num_inputs = fn->type_->input.size();
  ASSERT(num_inputs <= arguments.size());
  ASSERT(num_inputs <= regs_.size());
  for (size_t i = 0; i < arguments.size(); ++i) { regs_[i] = arguments[i]; }
}

BlockIndex ExecContext::ExecuteBlock() {
  Val result;
  ASSERT(current_block().cmds_.size() > 0u);
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter);
    if (cmd_iter->type != nullptr && cmd_iter->type != type::Void()) {
      // TODO the below assertion fails for structs (actual struct vs ptr to the
      // struct). Figure out how to fix it
      // ASSERT(result.type == cmd_iter->type);
      this->reg(cmd_iter->result) = result;
    }
    ++cmd_iter;
  } while (!std::holds_alternative<BlockIndex>(result.value));

  return std::get<BlockIndex>(result.value);
}

IR::Val Stack::Push(const type::Pointer *ptr) {
  size_t addr = buffer_.size();
  buffer_.append_bytes(
      Architecture::InterprettingMachine().bytes(ptr->pointee));
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
    case Op::Cast:
      // TODO nullptr okay here?
      return Cast(cmd.type, resolved[0], nullptr);
    case Op::Neg: return Neg(resolved[0]);
    case Op::Add: return Add(resolved[0], resolved[1]);
    case Op::Sub: return Sub(resolved[0], resolved[1]);
    case Op::Mul: return Mul(resolved[0], resolved[1]);
    case Op::Div: return Div(resolved[0], resolved[1]);
    case Op::Mod: return Mod(resolved[0], resolved[1]);
    case Op::Arrow: return Arrow(resolved[0], resolved[1]);
    case Op::BlockSeq: return BlockSeq(std::move(resolved));
    case Op::BlockSeqContains: {
      // TODO constant propogation?
      const auto &seq = std::get<BlockSequence>(resolved[0].value).seq_;
      auto* block_lit = std::get<BlockSequence>(resolved[1].value).seq_[0];
      return IR::Val::Bool(std::any_of(
          seq.begin(), seq.end(),
          [block_lit](AST::BlockLiteral *lit) { return lit == block_lit; }));
    } break;
    case Op::Tup: {
      std::vector<const type::Type *> types;
      types.reserve(resolved.size());
      for (const auto &val : resolved) {
        types.push_back(std::get<const type::Type *>(val.value));
      }
      return IR::Val::Type(type::Tup(std::move(types)));
    }
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
    case Op::Extend: return Extend(resolved[0]);
    case Op::Trunc: return Trunc(resolved[0]);
    case Op::Err:
      return IR::Val::CodeBlock(
          AST::CodeBlock(std::get<std::string_view>(resolved[0].value)));
    case Op::Call: {
      // TODO this feels like a gross hack
      IR::Func *fn = std::visit(
          [](auto f) -> IR::Func * {
            if constexpr (std::is_same_v<std::decay_t<decltype(f)>,
                                         IR::Func *>) {
              return f;
            } else {
              return nullptr;
            }
          },
          resolved.back().value);
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
              // TODO is this actually how you want ot print a codeblock? should
              // you be allowed to print a codeblock?
              [](const AST::CodeBlock &cb) { std::cerr << cb.to_string(0); },
              [](std::string_view s) { std::cerr << s; },
              [](const Addr &a) { std::cerr << a.to_string(); },
              [&resolved](EnumVal e) {
                std::cerr
                    << resolved[0].type->as<type::Enum>().members_[e.value];
              },
              [&resolved](FlagsVal f) {
                size_t val = f.value;
                std::vector<std::string> vals;
                const auto &members =
                    resolved[0].type->as<type::Flags>().members_;
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
                  while (iter != vals.end()) { std::cerr << " | " << *iter++; }
                }
              },
              [](IR::Func *f) {
                std::cerr << "{" << f->type_->to_string() << "}";
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
    return ctor(*static_cast<cpp_type *>(addr.as_heap));                       \
  }
          LOAD_FROM_HEAP(type::Bool, IR::Val::Bool, bool);
          LOAD_FROM_HEAP(type::Char, IR::Val::Char, char);
          LOAD_FROM_HEAP(type::Int, IR::Val::Int, i32);
          LOAD_FROM_HEAP(type::Real, IR::Val::Real, double);
          // LOAD_FROM_HEAP(type::Code, IR::Val::CodeBlock, AST::CodeBlock);
          LOAD_FROM_HEAP(type::Type_, IR::Val::Type, const type::Type *);
          if (cmd.type->is<type::Pointer>()) {
            return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                 cmd.type->as<type::Pointer>().pointee);
          } else if (cmd.type->is<type::Enum>()) {
            return IR::Val::Enum(&cmd.type->as<type::Enum>(),
                                 *static_cast<size_t *>(addr.as_heap));
          } else if (cmd.type->is<type::Flags>()) {
            return IR::Val::Flags(&cmd.type->as<type::Flags>(),
                                  *static_cast<size_t *>(addr.as_heap));
          } else if (cmd.type->is<type::CharBuffer>()) {
            // TODO Add a string_view overload for Val::CharBuf.
            return IR::Val::CharBuf(
                std::string(*static_cast<std::string_view *>(addr.as_heap)));
          } else {
            NOT_YET("Don't know how to load type: ", cmd.type);
          }
          NOT_YET("Don't know how to load type: ", cmd.type);
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
          // TODO LOAD_FROM_STACK(type::Code, CodeBlock, AST::CodeBlock);
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
          } else if (cmd.type->is<type::CharBuffer>()) {
            // TODO Add a string_view overload for Val::CharBuf.
            return IR::Val::CharBuf(
                std::string(stack_.Load<std::string_view>(addr.as_stack)));
          } else if (cmd.type->is<type::Flags>()) {
            return IR::Val::Flags(&cmd.type->as<type::Flags>(),
                                 stack_.Load<size_t>(addr.as_stack));
          } else if (cmd.type->is<type::Function>()) {
            return IR::Val::Func(stack_.Load<IR::Func *>(addr.as_stack));
          } else {
            call_stack.top().fn_->dump();
            NOT_YET("Don't know how to load type: ", cmd.type);
          }
#undef LOAD_FROM_STACK_IF
        } break;
      }
    } break;
    case Op::SetReturn: {
      reg(std::get<Register>(cmd.args[1].value)) = std::move(resolved[0]);
      return IR::Val::None();
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
          std::visit(
              base::overloaded{[this, &addr](EnumVal e) {
                                 stack_.Store(e.value, addr.as_stack);
                               },
                               [this, &addr](FlagsVal f) {
                                 stack_.Store(f.value, addr.as_stack);
                               },
                               [this, &addr](AST::Function *fn) {
                                 LOG << fn;
                                 UNREACHABLE();
                                 stack_.Store(fn, addr.as_stack);
                               },
                               [this, &addr](const auto &v) {
                                 if constexpr (std::is_trivial_v<
                                                   std::decay_t<decltype(v)>>) {
                                   stack_.Store(v, addr.as_stack);
                                 } else {
                                   NOT_YET();
                                 }
                               }},
              resolved[0].value);
          return IR::Val::None();
        case Addr::Kind::Heap:
          std::visit(
              [&addr](const auto &v) {
                *static_cast<std::decay_t<decltype(v)> *>(addr.as_heap) = v;
              },
              resolved[0].value);
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
      UNREACHABLE(
          "Previous block was ", Val::BasicBlock(call_stack.top().prev_),
          "\nCurrent block is ", Val::BasicBlock(call_stack.top().current_));
    case Op::Alloca: return stack_.Push(&cmd.type->as<type::Pointer>());
    case Op::PtrIncr:
      if (auto addr = std::get_if<Addr>(&resolved[0].value)) {
        switch (addr->kind) {
          case Addr::Kind::Stack: {
            auto bytes_fwd =
                Architecture::InterprettingMachine().ComputeArrayLength(
                    std::get<i32>(resolved[1].value),
                    cmd.type->as<type::Pointer>().pointee);
            return Val::StackAddr(addr->as_stack + bytes_fwd,
                                  cmd.type->as<type::Pointer>().pointee);
          }
          case Addr::Kind::Heap: {
            auto bytes_fwd =
                Architecture::InterprettingMachine().ComputeArrayLength(
                    std::get<i32>(resolved[1].value),
                    cmd.type->as<type::Pointer>().pointee);
            return Val::HeapAddr(
                static_cast<void *>(static_cast<char *>(addr->as_heap) +
                                    bytes_fwd),
                cmd.type->as<type::Pointer>().pointee);
          }
          case Addr::Kind::Global: NOT_YET();
          case Addr::Kind::Null: NOT_YET();
        }
        UNREACHABLE("Invalid address kind: ", static_cast<int>(addr->kind));
       } else {
        NOT_YET();
      }
    case Op::CreateStruct: {
      return IR::Val::Struct();
    } break;
    case Op::InsertField: {
      auto *struct_to_mod = std::get<type::Struct *>(resolved[0].value);
      struct_to_mod->fields_.push_back(type::Struct::Field{
          std::string(std::get<std::string_view>(resolved[1].value)),
          std::get<const type::Type *>(resolved[2].value), resolved[3]});

      auto[iter, success] = struct_to_mod->field_indices_.emplace(
          std::string(std::get<std::string_view>(resolved[1].value)),
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
    case Op::ReturnJump: return Val::BasicBlock(BlockIndex{-1});
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
    IR::BasicBlock::Current = fn->entry();
    // TODO use the right module
    Context ctx(nullptr);
    auto expr_val      = expr->EmitIR(&ctx);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return;
    }

    if (expr->type != type::Void()) { expr->type->EmitRepr(expr_val, &ctx); }
    IR::ReturnJump();
  }

  IR::ExecContext ctx;
  Execute(fn.get(), {}, &ctx);
}
