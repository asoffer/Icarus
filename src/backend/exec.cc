#include "backend/exec.h"

#include <cmath>
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

// TODO compile-time failure. dump the stack trace and abort for Null address
// kinds

namespace IR {
base::vector<Val> Execute(Func *fn, const base::vector<Val> &arguments,
                         ExecContext *ctx) {
  if (fn->gened_fn_) { fn->gened_fn_->CompleteBody(fn->mod_); }
  ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an error.

  while (true) {
    auto block_index = ctx->ExecuteBlock();
    if (block_index.is_default()) {
      base::vector<IR::Val> rets;
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

ExecContext::Frame::Frame(Func *fn, const base::vector<Val> &arguments)
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
  base::vector<Val> resolved;
  resolved.reserve(cmd.args.size());
  for (const auto& arg : cmd.args) {
    if (auto *r = std::get_if<Register>(&arg.value)) {
      resolved.push_back(reg(*r));
    } else {
      resolved.push_back(arg);
    }
  }

  switch (cmd.op_code_) {
    case Op::Trunc: return Trunc(reg(cmd.trunc_.reg_));
    case Op::Extend: return Extend(reg(cmd.extend_.reg_));
    case Op::Bytes:
      return IR::Val::Int(Architecture::InterprettingMachine().bytes(
          cmd.bytes_.arg_.is_reg_
              ? std::get<const type::Type *>(reg(cmd.bytes_.arg_.reg_).value)
              : cmd.bytes_.arg_.val_));
    case Op::Align:
      return IR::Val::Int(Architecture::InterprettingMachine().alignment(
          cmd.align_.arg_.is_reg_
              ? std::get<const type::Type *>(reg(cmd.align_.arg_.reg_).value)
              : cmd.align_.arg_.val_));
    case Op::Not: return Not(reg(cmd.not_.reg_));
    case Op::NegInt: return NegInt(reg(cmd.neg_int_.reg_));
    case Op::NegReal: return NegReal(reg(cmd.neg_real_.reg_));
    case Op::ArrayLength:
      return IR::Val::Addr(
          cmd.array_data_.arg_.is_reg_
              ? std::get<IR::Addr>(reg(cmd.array_data_.arg_.reg_).value)
              : cmd.array_data_.arg_.val_,
          type::Int);
    case Op::ArrayData: {
      auto addr = cmd.array_data_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.array_data_.arg_.reg_).value)
                      : cmd.array_data_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Stack:
          return IR::Val::StackAddr(
              addr.as_stack +
                  Architecture::InterprettingMachine().bytes(type::Int),
              cmd.type->as<type::Pointer>().pointee);

        case Addr::Kind::Heap:
          return IR::Val::HeapAddr(
              static_cast<void *>(
                  static_cast<u8 *>(addr.as_heap) +
                  Architecture::InterprettingMachine().bytes(type::Int)),
              cmd.type->as<type::Pointer>().pointee);
      }
    } break;
    case Op::LoadBool: {
      auto addr = cmd.load_bool_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.load_bool_.arg_.reg_).value)
                      : cmd.load_bool_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Heap:
          return IR::Val::Bool(*static_cast<bool *>(addr.as_heap));
        case Addr::Kind::Stack:
          return IR::Val::Bool(stack_.Load<bool>(addr.as_stack));
      }
    } break;
    case Op::LoadChar: {
      auto addr = cmd.load_char_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.load_char_.arg_.reg_).value)
                      : cmd.load_char_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Heap:
          return IR::Val::Char(*static_cast<char *>(addr.as_heap));
        case Addr::Kind::Stack:
          return IR::Val::Char(stack_.Load<char>(addr.as_stack));
      }
    } break;
    case Op::LoadInt: {
      auto addr = cmd.load_int_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.load_int_.arg_.reg_).value)
                      : cmd.load_int_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Heap:
          return IR::Val::Int(*static_cast<int *>(addr.as_heap));
        case Addr::Kind::Stack:
          return IR::Val::Int(stack_.Load<int>(addr.as_stack));
      }
    } break;
    case Op::LoadReal: {
      auto addr = cmd.load_real_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.load_real_.arg_.reg_).value)
                      : cmd.load_real_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Heap:
          return IR::Val::Real(*static_cast<double*>(addr.as_heap));
        case Addr::Kind::Stack:
          return IR::Val::Real(stack_.Load<double>(addr.as_stack));
      }
    } break;
    case Op::LoadType: {
      auto addr = cmd.load_type_.arg_.is_reg_
                      ? std::get<IR::Addr>(reg(cmd.load_type_.arg_.reg_).value)
                      : cmd.load_type_.arg_.val_;
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Heap:
          return IR::Val::Type(*static_cast<const type::Type **>(addr.as_heap));
        case Addr::Kind::Stack:
          return IR::Val::Type(stack_.Load<const type::Type *>(addr.as_stack));
      }
    } break;
     case Op::LoadEnum: {
       auto addr = cmd.load_enum_.arg_.is_reg_
                       ? std::get<IR::Addr>(reg(cmd.load_enum_.arg_.reg_).value)
                       : cmd.load_enum_.arg_.val_;
       switch (addr.kind) {
         case Addr::Kind::Null: UNREACHABLE();
         case Addr::Kind::Heap:
           return IR::Val::Enum(&cmd.type->as<type::Enum>(),
                                *static_cast<size_t *>(addr.as_heap));
         case Addr::Kind::Stack:
           return IR::Val::Enum(&cmd.type->as<type::Enum>(),
                                stack_.Load<size_t>(addr.as_stack));
       }
     } break;
     case Op::LoadFlags: {
       auto addr =
           cmd.load_flags_.arg_.is_reg_
               ? std::get<IR::Addr>(reg(cmd.load_flags_.arg_.reg_).value)
               : cmd.load_flags_.arg_.val_;
       switch (addr.kind) {
         case Addr::Kind::Null: UNREACHABLE();
         case Addr::Kind::Heap:
           return IR::Val::Flags(&cmd.type->as<type::Flags>(),
                                 *static_cast<size_t *>(addr.as_heap));
         case Addr::Kind::Stack:
           return IR::Val::Flags(&cmd.type->as<type::Flags>(),
                                 stack_.Load<size_t>(addr.as_stack));
       }
     } break;
     case Op::LoadAddr: {
       auto addr = cmd.load_addr_.arg_.is_reg_
                       ? std::get<IR::Addr>(reg(cmd.load_addr_.arg_.reg_).value)
                       : cmd.load_addr_.arg_.val_;
       switch (addr.kind) {
         case Addr::Kind::Null: UNREACHABLE();
         case Addr::Kind::Heap:
           return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                cmd.type->as<type::Pointer>().pointee);
         case Addr::Kind::Stack:
           return IR::Val::Addr(stack_.Load<Addr>(addr.as_stack),
                                cmd.type->as<type::Pointer>().pointee);
       }

       /*
       switch (addr.kind) {
         case Addr::Kind::Null: UNREACHABLE();
         case Addr::Kind::Heap: {
           // LOAD_FROM_HEAP(type::Code, IR::Val::CodeBlock, AST::CodeBlock);
           if (cmd.type->is<type::Pointer>()) {
             return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                  cmd.type->as<type::Pointer>().pointee);
           } else if (cmd.type->is<type::CharBuffer>()) {
             // TODO Add a string_view overload for Val::CharBuf.
             return IR::Val::CharBuf(
                 std::string(*static_cast<std::string_view *>(addr.as_heap)));
           } else {
             NOT_YET("Don't know how to load type: ", cmd.type);
           }
           NOT_YET("Don't know how to load type: ", cmd.type);
         } break;
         case Addr::Kind::Stack: {
           // TODO LOAD_FROM_STACK(type::Code, CodeBlock, AST::CodeBlock);
           if (cmd.type->is<type::Pointer>()) {
             switch (addr.kind) {
               case Addr::Kind::Stack:
                 return IR::Val::Addr(stack_.Load<Addr>(addr.as_stack),
                                      cmd.type->as<type::Pointer>().pointee);
               case Addr::Kind::Heap:
                 return IR::Val::Addr(*static_cast<Addr *>(addr.as_heap),
                                      cmd.type->as<type::Pointer>().pointee);
               case Addr::Kind::Null: NOT_YET();
             }
           } else if (cmd.type->is<type::CharBuffer>()) {
             // TODO Add a string_view overload for Val::CharBuf.
             return IR::Val::CharBuf(
                 std::string(stack_.Load<std::string_view>(addr.as_stack)));
           } else if (cmd.type->is<type::Function>()) {
             return IR::Val::Func(stack_.Load<IR::Func *>(addr.as_stack));
           } else {
             call_stack.top().fn_->dump();
             NOT_YET("Don't know how to load type: ", cmd.type);
           }
         } break;
       }*/
     } break;

     case Op::AddInt:
       return IR::Val::Int(
           (cmd.add_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.add_int_.args_[0].reg_).value)
                : cmd.add_int_.args_[0].val_) +
           (cmd.add_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.add_int_.args_[1].reg_).value)
                : cmd.add_int_.args_[1].val_));
     case Op::AddReal:
       return IR::Val::Real(
           (cmd.add_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.add_real_.args_[0].reg_).value)
                : cmd.add_real_.args_[0].val_) +
           (cmd.add_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.add_real_.args_[1].reg_).value)
                : cmd.add_real_.args_[1].val_));
     case Op::AddCharBuf:
       return IR::Val::CharBuf(
           std::string(cmd.add_char_buf_.args_[0].is_reg_
                           ? std::get<std::string_view>(
                                 reg(cmd.add_char_buf_.args_[0].reg_).value)
                           : cmd.add_char_buf_.args_[0].val_) +
           std::string(cmd.add_char_buf_.args_[1].is_reg_
                           ? std::get<std::string_view>(
                                 reg(cmd.add_char_buf_.args_[1].reg_).value)
                           : cmd.add_char_buf_.args_[1].val_));
     case Op::SubInt:
       return IR::Val::Int(
           (cmd.sub_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.sub_int_.args_[0].reg_).value)
                : cmd.sub_int_.args_[0].val_) -
           (cmd.sub_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.sub_int_.args_[1].reg_).value)
                : cmd.sub_int_.args_[1].val_));
     case Op::SubReal:
       return IR::Val::Real(
           (cmd.sub_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.sub_real_.args_[0].reg_).value)
                : cmd.sub_real_.args_[0].val_) -
           (cmd.sub_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.sub_real_.args_[1].reg_).value)
                : cmd.sub_real_.args_[1].val_));
     case Op::MulInt:
       return IR::Val::Int(
           (cmd.mul_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.mul_int_.args_[0].reg_).value)
                : cmd.mul_int_.args_[0].val_) *
           (cmd.mul_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.mul_int_.args_[1].reg_).value)
                : cmd.mul_int_.args_[1].val_));
     case Op::MulReal:
       return IR::Val::Real(
           (cmd.mul_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.mul_real_.args_[0].reg_).value)
                : cmd.mul_real_.args_[0].val_) *
           (cmd.mul_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.mul_real_.args_[1].reg_).value)
                : cmd.mul_real_.args_[1].val_));
     case Op::DivInt:
       return IR::Val::Int(
           (cmd.div_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.div_int_.args_[0].reg_).value)
                : cmd.div_int_.args_[0].val_) *
           (cmd.div_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.div_int_.args_[1].reg_).value)
                : cmd.div_int_.args_[1].val_));
     case Op::DivReal:
       return IR::Val::Real(
           (cmd.div_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.div_real_.args_[0].reg_).value)
                : cmd.div_real_.args_[0].val_) *
           (cmd.div_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.div_real_.args_[1].reg_).value)
                : cmd.div_real_.args_[1].val_));
     case Op::ModInt:
       return IR::Val::Int(
           (cmd.mod_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.mod_int_.args_[0].reg_).value)
                : cmd.mod_int_.args_[0].val_) %
           (cmd.mod_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.mod_int_.args_[1].reg_).value)
                : cmd.mod_int_.args_[1].val_));
     case Op::ModReal:
       return IR::Val::Real(std::fmod(
           (cmd.mod_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.mod_real_.args_[0].reg_).value)
                : cmd.mod_real_.args_[0].val_),
           (cmd.mod_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.mod_real_.args_[1].reg_).value)
                : cmd.mod_real_.args_[1].val_)));
     case Op::LtInt:
       return IR::Val::Bool(
           (cmd.lt_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.lt_int_.args_[0].reg_).value)
                : cmd.lt_int_.args_[0].val_) <
           (cmd.lt_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.lt_int_.args_[1].reg_).value)
                : cmd.lt_int_.args_[1].val_));
     case Op::LtReal:
       return IR::Val::Bool(
           (cmd.lt_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.lt_real_.args_[0].reg_).value)
                : cmd.lt_real_.args_[0].val_) <
           (cmd.lt_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.lt_real_.args_[1].reg_).value)
                : cmd.lt_real_.args_[1].val_));
     case Op::LtFlags: {
       auto lhs =
           cmd.lt_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[0].reg_).value)
               : cmd.lt_flags_.args_[0].val_;
       auto rhs =
           cmd.lt_flags_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[1].reg_).value)
               : cmd.lt_flags_.args_[1].val_;
       return IR::Val::Bool(lhs.value != rhs.value &&
                            ((lhs.value | rhs.value) == rhs.value));
     } break;
     case Op::LeInt:
       return IR::Val::Bool(
           (cmd.le_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.le_int_.args_[0].reg_).value)
                : cmd.le_int_.args_[0].val_) <=
           (cmd.le_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.le_int_.args_[1].reg_).value)
                : cmd.le_int_.args_[1].val_));
     case Op::LeReal:
       return IR::Val::Bool(
           (cmd.le_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.le_real_.args_[0].reg_).value)
                : cmd.le_real_.args_[0].val_) <=
           (cmd.le_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.le_real_.args_[1].reg_).value)
                : cmd.le_real_.args_[1].val_));
     case Op::LeFlags: {
       auto lhs =
           cmd.lt_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[0].reg_).value)
               : cmd.lt_flags_.args_[0].val_;
       auto rhs =
           cmd.lt_flags_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[1].reg_).value)
               : cmd.lt_flags_.args_[1].val_;
       return IR::Val::Bool((lhs.value | rhs.value) == rhs.value);
     } break;
     case Op::GtInt:
       return IR::Val::Bool(
           (cmd.lt_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.lt_int_.args_[0].reg_).value)
                : cmd.lt_int_.args_[0].val_) >
           (cmd.lt_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.lt_int_.args_[1].reg_).value)
                : cmd.lt_int_.args_[1].val_));
     case Op::GtReal:
       return IR::Val::Bool(
           (cmd.lt_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.lt_real_.args_[0].reg_).value)
                : cmd.lt_real_.args_[0].val_) >
           (cmd.lt_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.lt_real_.args_[1].reg_).value)
                : cmd.lt_real_.args_[1].val_));
     case Op::GtFlags: {
       auto lhs =
           cmd.lt_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[0].reg_).value)
               : cmd.lt_flags_.args_[0].val_;
       auto rhs =
           cmd.lt_flags_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[1].reg_).value)
               : cmd.lt_flags_.args_[1].val_;
       return IR::Val::Bool(lhs.value != rhs.value &&
                            ((lhs.value | rhs.value) == lhs.value));
     } break;
     case Op::GeInt:
       return IR::Val::Bool(
           (cmd.le_int_.args_[0].is_reg_
                ? std::get<i32>(reg(cmd.le_int_.args_[0].reg_).value)
                : cmd.le_int_.args_[0].val_) >=
           (cmd.le_int_.args_[1].is_reg_
                ? std::get<i32>(reg(cmd.le_int_.args_[1].reg_).value)
                : cmd.le_int_.args_[1].val_));
     case Op::GeReal:
       return IR::Val::Bool(
           (cmd.le_real_.args_[0].is_reg_
                ? std::get<double>(reg(cmd.le_real_.args_[0].reg_).value)
                : cmd.le_real_.args_[0].val_) >=
           (cmd.le_real_.args_[1].is_reg_
                ? std::get<double>(reg(cmd.le_real_.args_[1].reg_).value)
                : cmd.le_real_.args_[1].val_));
     case Op::GeFlags: {
       auto lhs =
           cmd.lt_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[0].reg_).value)
               : cmd.lt_flags_.args_[0].val_;
       auto rhs =
           cmd.lt_flags_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.lt_flags_.args_[1].reg_).value)
               : cmd.lt_flags_.args_[1].val_;
       return IR::Val::Bool((lhs.value | rhs.value) == lhs.value);
     } break;
     case Op::EqBool: {
       auto lhs = cmd.eq_bool_.args_[0].is_reg_
                      ? std::get<bool>(reg(cmd.eq_bool_.args_[0].reg_).value)
                      : cmd.eq_bool_.args_[0].val_;
       auto rhs = cmd.eq_bool_.args_[1].is_reg_
                      ? std::get<bool>(reg(cmd.eq_bool_.args_[1].reg_).value)
                      : cmd.eq_bool_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqChar: {
       auto lhs = cmd.eq_char_.args_[0].is_reg_
                      ? std::get<char>(reg(cmd.eq_char_.args_[0].reg_).value)
                      : cmd.eq_char_.args_[0].val_;
       auto rhs = cmd.eq_char_.args_[1].is_reg_
                      ? std::get<char>(reg(cmd.eq_char_.args_[1].reg_).value)
                      : cmd.eq_char_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqInt: {
       auto lhs = cmd.eq_int_.args_[0].is_reg_
                      ? std::get<int>(reg(cmd.eq_int_.args_[0].reg_).value)
                      : cmd.eq_int_.args_[0].val_;
       auto rhs = cmd.eq_int_.args_[1].is_reg_
                      ? std::get<int>(reg(cmd.eq_int_.args_[1].reg_).value)
                      : cmd.eq_int_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqReal: {
       auto lhs = cmd.eq_real_.args_[0].is_reg_
                      ? std::get<double>(reg(cmd.eq_real_.args_[0].reg_).value)
                      : cmd.eq_real_.args_[0].val_;
       auto rhs = cmd.eq_real_.args_[1].is_reg_
                      ? std::get<double>(reg(cmd.eq_real_.args_[1].reg_).value)
                      : cmd.eq_real_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqFlags: {
       auto lhs =
           cmd.eq_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.eq_flags_.args_[0].reg_).value)
               : cmd.eq_flags_.args_[0].val_;
       auto rhs =
           cmd.eq_type_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.eq_flags_.args_[1].reg_).value)
               : cmd.eq_flags_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqType: {
       auto lhs = cmd.eq_type_.args_[0].is_reg_
                      ? std::get<const type::Type *>(
                            reg(cmd.eq_type_.args_[0].reg_).value)
                      : cmd.eq_type_.args_[0].val_;
       auto rhs = cmd.eq_type_.args_[1].is_reg_
                      ? std::get<const type::Type *>(
                            reg(cmd.eq_type_.args_[1].reg_).value)
                      : cmd.eq_type_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::EqAddr: {
       auto lhs =
           cmd.eq_addr_.args_[0].is_reg_
               ? std::get<IR::Addr>(reg(cmd.eq_addr_.args_[0].reg_).value)
               : cmd.eq_addr_.args_[0].val_;
       auto rhs =
           cmd.eq_addr_.args_[1].is_reg_
               ? std::get<IR::Addr>(reg(cmd.eq_addr_.args_[1].reg_).value)
               : cmd.eq_addr_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeBool: {
       auto lhs = cmd.ne_bool_.args_[0].is_reg_
                      ? std::get<bool>(reg(cmd.ne_bool_.args_[0].reg_).value)
                      : cmd.ne_bool_.args_[0].val_;
       auto rhs = cmd.ne_bool_.args_[1].is_reg_
                      ? std::get<bool>(reg(cmd.ne_bool_.args_[1].reg_).value)
                      : cmd.ne_bool_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeChar: {
       auto lhs = cmd.ne_char_.args_[0].is_reg_
                      ? std::get<char>(reg(cmd.ne_char_.args_[0].reg_).value)
                      : cmd.ne_char_.args_[0].val_;
       auto rhs = cmd.ne_char_.args_[1].is_reg_
                      ? std::get<char>(reg(cmd.ne_char_.args_[1].reg_).value)
                      : cmd.ne_char_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeInt: {
       auto lhs = cmd.ne_int_.args_[0].is_reg_
                      ? std::get<int>(reg(cmd.ne_int_.args_[0].reg_).value)
                      : cmd.ne_int_.args_[0].val_;
       auto rhs = cmd.ne_int_.args_[1].is_reg_
                      ? std::get<int>(reg(cmd.ne_int_.args_[1].reg_).value)
                      : cmd.ne_int_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeReal: {
       auto lhs = cmd.ne_real_.args_[0].is_reg_
                      ? std::get<double>(reg(cmd.ne_real_.args_[0].reg_).value)
                      : cmd.ne_real_.args_[0].val_;
       auto rhs = cmd.ne_real_.args_[1].is_reg_
                      ? std::get<double>(reg(cmd.ne_real_.args_[1].reg_).value)
                      : cmd.ne_real_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeFlags: {
       auto lhs =
           cmd.ne_flags_.args_[0].is_reg_
               ? std::get<FlagsVal>(reg(cmd.ne_flags_.args_[0].reg_).value)
               : cmd.ne_flags_.args_[0].val_;
       auto rhs =
           cmd.ne_type_.args_[1].is_reg_
               ? std::get<FlagsVal>(reg(cmd.ne_flags_.args_[1].reg_).value)
               : cmd.ne_flags_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeType: {
       auto lhs = cmd.ne_type_.args_[0].is_reg_
                      ? std::get<const type::Type *>(
                            reg(cmd.ne_type_.args_[0].reg_).value)
                      : cmd.ne_type_.args_[0].val_;
       auto rhs = cmd.ne_type_.args_[1].is_reg_
                      ? std::get<const type::Type *>(
                            reg(cmd.ne_type_.args_[1].reg_).value)
                      : cmd.ne_type_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::NeAddr: {
       auto lhs =
           cmd.ne_addr_.args_[0].is_reg_
               ? std::get<IR::Addr>(reg(cmd.ne_addr_.args_[0].reg_).value)
               : cmd.ne_addr_.args_[0].val_;
       auto rhs =
           cmd.ne_addr_.args_[1].is_reg_
               ? std::get<IR::Addr>(reg(cmd.ne_addr_.args_[1].reg_).value)
               : cmd.ne_addr_.args_[1].val_;
       return IR::Val::Bool(lhs == rhs);
     } break;
     case Op::XorBool: {
       auto lhs = cmd.xor_bool_.args_[0].is_reg_
                      ? std::get<bool>(reg(cmd.xor_bool_.args_[0].reg_).value)
                      : cmd.xor_bool_.args_[0].val_;
       auto rhs = cmd.xor_bool_.args_[1].is_reg_
                      ? std::get<bool>(reg(cmd.xor_bool_.args_[1].reg_).value)
                      : cmd.xor_bool_.args_[1].val_;
       return IR::Val::Bool(lhs ^ rhs);
     } break;
     case Op::XorFlags: NOT_YET();
     case Op::OrBool: {
       auto lhs = cmd.or_bool_.args_[0].is_reg_
                      ? std::get<bool>(reg(cmd.or_bool_.args_[0].reg_).value)
                      : cmd.or_bool_.args_[0].val_;
       auto rhs = cmd.or_bool_.args_[1].is_reg_
                      ? std::get<bool>(reg(cmd.or_bool_.args_[1].reg_).value)
                      : cmd.or_bool_.args_[1].val_;
       return IR::Val::Bool(lhs | rhs);
     } break;
     case Op::OrFlags: NOT_YET();
     case Op::AndBool: {
       auto lhs = cmd.and_bool_.args_[0].is_reg_
                      ? std::get<bool>(reg(cmd.and_bool_.args_[0].reg_).value)
                      : cmd.and_bool_.args_[0].val_;
       auto rhs = cmd.and_bool_.args_[1].is_reg_
                      ? std::get<bool>(reg(cmd.and_bool_.args_[1].reg_).value)
                      : cmd.and_bool_.args_[1].val_;
       return IR::Val::Bool(lhs & rhs);
     } break;
     case Op::AndFlags: NOT_YET();
     case Op::CreateStruct: {
       return IR::Val::Struct();
     } break;
     case Op::FinalizeStruct: {
       return IR::Val::Type(
           std::get<type::Struct *>(reg(cmd.finalize_struct_.reg_).value)
               ->finalize());
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
    case Op::Malloc:
      return IR::Val::HeapAddr(
          malloc(cmd.malloc_.arg_.is_reg_
                     ? std::get<i32>(reg(cmd.malloc_.arg_.reg_).value)
                     : cmd.malloc_.arg_.val_),
          cmd.type->as<type::Pointer>().pointee);
    case Op::Free:
      free(std::get<Addr>(reg(cmd.free_.reg_).value).as_heap);
      return Val::None();
    case Op::Alloca: return stack_.Push(&cmd.type->as<type::Pointer>());
    case Op::Ptr: return IR::Ptr(reg(cmd.ptr_.reg_));
    case Op::Arrow: {
      auto lhs = cmd.arrow_.args_[0].is_reg_
                     ? std::get<type::Type const *>(
                           reg(cmd.arrow_.args_[0].reg_).value)
                     : cmd.arrow_.args_[0].val_;
      auto rhs = cmd.arrow_.args_[1].is_reg_
                     ? std::get<type::Type const *>(
                           reg(cmd.arrow_.args_[1].reg_).value)
                     : cmd.arrow_.args_[1].val_;
      return IR::Val::Type(type::Func({lhs}, {rhs}));
    } break;
    case Op::Array: {
      auto len = cmd.array_.len_.is_reg_
                     ? std::get<i32>(reg(cmd.array_.len_.reg_).value)
                     : cmd.array_.len_.val_;
      auto t =
          cmd.array_.type_.is_reg_
              ? std::get<type::Type const *>(reg(cmd.array_.type_.reg_).value)
              : cmd.array_.type_.val_;
      return IR::Val::Type(len == -1 ? type::Arr(t) : type::Arr(t, len));
    } break;
    case Op::VariantType:
      return Val::Addr(std::get<Addr>(reg(cmd.variant_type_.reg_).value), type::Type_);
    case Op::VariantValue: {
      auto bytes = Architecture::InterprettingMachine().bytes(Ptr(type::Type_));
      auto bytes_fwd =
          Architecture::InterprettingMachine().MoveForwardToAlignment(
              Ptr(type::Type_), bytes);
      switch (std::get<Addr>(reg(cmd.variant_value_.reg_).value).kind) {
        case Addr::Kind::Stack: {
          return Val::StackAddr(
              std::get<Addr>(reg(cmd.variant_value_.reg_).value).as_stack +
                  bytes_fwd,
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Heap: {
          return Val::HeapAddr(
              static_cast<void *>(
                  static_cast<char *>(
                      std::get<Addr>(reg(cmd.variant_value_.reg_).value)
                          .as_heap) +
                  bytes_fwd),
              cmd.type->as<type::Pointer>().pointee);
        }
        case Addr::Kind::Null: NOT_YET();
      }
      UNREACHABLE("Invalid address kind: ");
    } break;
    case Op::PtrIncr: {
      auto addr = std::get<Addr>(reg(cmd.ptr_incr_.ptr_).value);
      auto incr = cmd.ptr_incr_.incr_.is_reg_
                      ? std::get<i32>(reg(cmd.ptr_incr_.incr_.reg_).value)
                      : cmd.ptr_incr_.incr_.val_;
      // Sadly must convert to value and back even though it's guaranteed to be
      // constant folded
      auto bytes_fwd = std::get<i32>(
          Architecture::InterprettingMachine()
              .ComputeArrayLength(IR::Val::Int(incr),
                                  cmd.type->as<type::Pointer>().pointee)
              .value);
      switch (addr.kind) {
        case Addr::Kind::Stack:
          return Val::StackAddr(addr.as_stack + bytes_fwd,
                                cmd.type->as<type::Pointer>().pointee);

        case Addr::Kind::Heap:
          return Val::HeapAddr(
              static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                  bytes_fwd),
              cmd.type->as<type::Pointer>().pointee);

        case Addr::Kind::Null: NOT_YET();
      }
    } break;
    case Op::Field: {
      auto addr_val = reg(cmd.field_.ptr_);
      auto addr     = std::get<Addr>(addr_val.value);

      auto *struct_type =
          &addr_val.type->as<type::Pointer>().pointee->as<type::Struct>();
      // This can probably be precomputed.
      size_t offset = 0;
      for (size_t i = 0; i < cmd.field_.num_; ++i) {
        auto field_type = struct_type->fields_.at(i).type;
        offset += Architecture::InterprettingMachine().bytes(field_type);
        offset = Architecture::InterprettingMachine().MoveForwardToAlignment(
            struct_type->fields_.at(i + 1).type, offset);
      }

      if (addr.kind == Addr::Kind::Stack) {
        return Val::StackAddr(addr.as_stack + offset,
                              cmd.type->as<type::Pointer>().pointee);
      } else {
        return Val::HeapAddr(static_cast<char *>(addr.as_heap) + offset,
                             cmd.type->as<type::Pointer>().pointee);
      }
    } break;
    case Op::PrintBool:
      std::cerr << ((cmd.print_bool_.arg_.is_reg_
                         ? std::get<bool>(reg(cmd.print_bool_.arg_.reg_).value)
                         : cmd.print_bool_.arg_.val_)
                        ? "true"
                        : "false");
      return IR::Val::None();
    case Op::PrintChar:
      std::cerr << (cmd.print_char_.arg_.is_reg_
                        ? std::get<char>(reg(cmd.print_char_.arg_.reg_).value)
                        : cmd.print_char_.arg_.val_);
      return IR::Val::None();
    case Op::PrintInt:
      std::cerr << (cmd.print_int_.arg_.is_reg_
                        ? std::get<i32>(reg(cmd.print_int_.arg_.reg_).value)
                        : cmd.print_int_.arg_.val_);
      return IR::Val::None();
    case Op::PrintReal:
      std::cerr << (cmd.print_real_.arg_.is_reg_
                        ? std::get<double>(
                              reg(cmd.print_real_.arg_.reg_).value)
                        : cmd.print_real_.arg_.val_);
      return IR::Val::None();
    case Op::PrintType:
      std::cerr << (cmd.print_type_.arg_.is_reg_
                        ? std::get<type::Type const *>(
                              reg(cmd.print_type_.arg_.reg_).value)
                        : cmd.print_type_.arg_.val_)
                       ->to_string();
      return IR::Val::None();
    case Op::PrintEnum: NOT_YET(); return IR::Val::None();
      /*
      std::cerr << resolved[0].type->as<type::Enum>().members_[e.value];
      */
    case Op::PrintFlags:
      NOT_YET();
      /*
      size_t val = f.value;
      base::vector<std::string> vals;
      const auto &members = resolved[0].type->as<type::Flags>().members_;
      size_t i            = 0;
      size_t pow          = 1;
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
      */
      return IR::Val::None();
    case Op::PrintAddr:
      std::cerr << (cmd.print_addr_.arg_.is_reg_
                        ? std::get<IR::Addr>(
                              reg(cmd.print_addr_.arg_.reg_).value)
                        : cmd.print_addr_.arg_.val_)
                       .to_string();
      return IR::Val::None();
    case Op::PrintCharBuffer:
      std::cerr << (cmd.print_char_buffer_.arg_.is_reg_
                        ? std::get<std::string_view>(
                              reg(cmd.print_char_buffer_.arg_.reg_).value)
                        : cmd.print_char_buffer_.arg_.val_);
      return IR::Val::None();
    case Op::Call: {
      std::vector<Val> resolved_args;
      resolved_args.reserve(ASSERT_NOT_NULL(cmd.call_.args_)->size());
      for (auto const &arg : *cmd.call_.args_) {
        if (auto *r = std::get_if<Register>(&arg.value)) {
          resolved_args.push_back(reg(*r));
        } else {
          resolved_args.push_back(arg);
        }
      }

      // TODO you need to be able to determine how many args there are
      switch (cmd.call_.which_active_) {
        case 0x00: {
          // TODO what if the register is a foerign fn?
          auto results = Execute(std::get<Func *>(reg(cmd.call_.reg_).value),
                                 resolved_args, this);
          return results.empty() ? IR::Val::None() : results[0];
        } break;
        case 0x01: {
          auto results = Execute(cmd.call_.fn_, resolved_args, this);
          return results.empty() ? IR::Val::None() : results[0];
        } break;
        case 0x02:
          if (cmd.call_.foreign_fn_.name_ == "malloc") {
            return IR::Val::HeapAddr(
                malloc(std::get<i32>(resolved_args[0].value)),
                cmd.type->as<type::Pointer>().pointee);
          } else {
            NOT_YET();
          }
      }
    } break;
    case Op::Tup: {
      base::vector<const type::Type *> types;
      types.reserve(cmd.tup_.args_->size());
      for (const auto &val : *cmd.tup_.args_) {
        types.push_back(std::get<const type::Type *>(val.value));
      }
      return IR::Val::Type(type::Tup(std::move(types)));
    }
    case Op::Variant: {
      base::vector<const type::Type *> types;
      types.reserve(cmd.variant_.args_->size());
      for (const auto &val : *cmd.variant_.args_) {
        types.push_back(std::get<const type::Type *>(val.value));
      }
      return IR::Val::Type(type::Var(std::move(types)));
    }

    case Op::Cast:
      // TODO nullptr okay here?
      return Cast(cmd.type, resolved[0], nullptr);
    case Op::AddCodeBlock: NOT_YET();
    case Op::BlockSeq: return BlockSeq(std::move(resolved));
    case Op::BlockSeqContains: {
      // TODO constant propogation?
      const auto &seq = *std::get<BlockSequence>(resolved[0].value).seq_;
      auto *block_lit =
          std::get<BlockSequence>(resolved[1].value).seq_->front();
      return IR::Val::Bool(std::any_of(
          seq.begin(), seq.end(),
          [block_lit](AST::BlockLiteral *lit) { return lit == block_lit; }));
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
    case Op::Contextualize: NOT_YET();
    case Op::CondJump:
      return Val::BasicBlock(
          cmd.cond_jump_
              .blocks_[std::get<bool>(reg(cmd.cond_jump_.cond_).value)]);
    case Op::UncondJump: return Val::BasicBlock(cmd.uncond_jump_.block_);
    case Op::ReturnJump: return Val::BasicBlock(BlockIndex{-1});
  }
  UNREACHABLE();
}
} // namespace IR

void ReplEval(AST::Expression *expr) {
  // TODO is nullptr for module okay here?
  auto fn = std::make_unique<IR::Func>(
      nullptr, type::Func({}, {}),
      base::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    IR::BasicBlock::Current = fn->entry();
    // TODO use the right module
    Context ctx(nullptr);
    // TODO support multiple values computed simultaneously?
    auto expr_val = expr->EmitIR(&ctx)[0];
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
