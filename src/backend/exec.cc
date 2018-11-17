#include "backend/exec.h"

#include <chrono>
#include <cmath>
#include <cstring>
#include <future>
#include <iostream>
#include <memory>
#include <thread>

#include "architecture.h"
#include "ast/block_literal.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/scope_node.h"
#include "base/util.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"

using base::check::Is;

// TODO compile-time failure. dump the stack trace and abort for Null address
// kinds

namespace ir {
std::string_view SaveStringGlobally(std::string const &s);
ir::BlockSequence MakeBlockSeq(const base::vector<ir::BlockSequence> &blocks);
}  // namespace ir

namespace backend {

void Execute(ir::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<ir::Addr> &ret_slots,
             backend::ExecContext *exec_ctx) {
  // TODO what about bound constants?
  exec_ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an
  // error.

  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  for (auto *t : fn->type_->output) {
    offset = arch.MoveForwardToAlignment(t, offset) + arch.bytes(t);
  }
  base::untyped_buffer ret_buffer(offset);

  while (true) {
    auto block_index = exec_ctx->ExecuteBlock(ret_slots);
    if (block_index.is_default()) {
      exec_ctx->call_stack.pop();
      return;
    } else {
      exec_ctx->call_stack.top().MoveTo(block_index);
    }
  }
}

template <typename T>
T ExecContext::resolve(ir::Register val) const {
  return call_stack.top().regs_.get<T>(val.value);
}

ExecContext::ExecContext() : stack_(50u) {}

ir::BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(ir::Func *fn, const base::untyped_buffer &arguments)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_)) {
  regs_.write(0, arguments);
}

ir::BlockIndex ExecContext::ExecuteBlock(
    const base::vector<ir::Addr> &ret_slots) {
  ir::BlockIndex result;
  ASSERT(current_block().cmds_.size() > 0u);
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter++, ret_slots);
  } while (result == ir::BlockIndex{-2});
  return result;
}

template <typename T>
static T LoadValue(ir::Addr addr, const base::untyped_buffer &stack) {
  switch (addr.kind) {
    case ir::Addr::Kind::Null: UNREACHABLE();
    case ir::Addr::Kind::Heap: return *static_cast<T *>(addr.as_heap); break;
    case ir::Addr::Kind::Stack: return stack.get<T>(addr.as_stack); break;
  }
  UNREACHABLE();
}

template <typename T>
static void StoreValue(T val, ir::Addr addr, base::untyped_buffer *stack) {
  switch (addr.kind) {
    case ir::Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE();
    case ir::Addr::Kind::Stack: stack->set(addr.as_stack, val); return;
    case ir::Addr::Kind::Heap: *static_cast<T *>(addr.as_heap) = val;
  }
}

ir::BlockIndex ExecContext::ExecuteCmd(
    const ir::Cmd &cmd, const base::vector<ir::Addr> &ret_slots) {
  auto save = [&](auto val) {
    call_stack.top().regs_.set(cmd.result.value, val);
  };

  switch (cmd.op_code_) {
    case ir::Op::Death: UNREACHABLE();
    case ir::Op::Trunc: save(static_cast<char>(resolve<i32>(cmd.reg_))); break;
    case ir::Op::Extend: save(static_cast<i32>(resolve<char>(cmd.reg_))); break;
    case ir::Op::Bytes:
      save(Architecture::InterprettingMachine().bytes(resolve(cmd.type_arg_)));
      break;
    case ir::Op::Align:
      save(Architecture::InterprettingMachine().alignment(
          resolve(cmd.type_arg_)));
      break;
    case ir::Op::Not: save(!resolve<bool>(cmd.reg_)); break;
    case ir::Op::NegInt: save(-resolve<i32>(cmd.reg_)); break;
    case ir::Op::NegFloat32: save(-resolve<double>(cmd.reg_)); break;
    case ir::Op::NegFloat64: save(-resolve<double>(cmd.reg_)); break;
    case ir::Op::ArrayLength: save(resolve<ir::Addr>(cmd.reg_)); break;
    case ir::Op::ArrayData: {
      auto addr = resolve<ir::Addr>(cmd.reg_);
      switch (addr.kind) {
        case ir::Addr::Kind::Null: UNREACHABLE();
        case ir::Addr::Kind::Stack:
          addr.as_stack +=
              Architecture::InterprettingMachine().bytes(type::Int);
          break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(
              static_cast<u8 *>(addr.as_heap) +
              Architecture::InterprettingMachine().bytes(type::Int));
          break;
      }
      save(addr);

    } break;
    case ir::Op::LoadBool:
      save(LoadValue<bool>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadChar:
      save(LoadValue<char>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadInt:
      save(LoadValue<i32>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadFloat32:
      save(LoadValue<double>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadFloat64:
      save(LoadValue<double>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadType:
      save(LoadValue<type::Type const *>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadEnum:
      save(LoadValue<size_t>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadFlags:
      save(LoadValue<size_t>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadAddr:
      save(LoadValue<ir::Addr>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadFunc:
      save(LoadValue<ir::Func *>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::AddInt:
      save(resolve(cmd.i32_args_.args_[0]) + resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::AddFloat32:
      save(resolve(cmd.float32_args_.args_[0]) +
           resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::AddFloat64:
      save(resolve(cmd.float64_args_.args_[0]) +
           resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::SubInt:
      save(resolve(cmd.i32_args_.args_[0]) - resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::SubFloat32:
      save(resolve(cmd.float32_args_.args_[0]) - resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::SubFloat64:
      save(resolve(cmd.float64_args_.args_[0]) - resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::MulInt:
      save(resolve(cmd.i32_args_.args_[0]) * resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::MulFloat32:
      save(resolve(cmd.float32_args_.args_[0]) * resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::MulFloat64:
      save(resolve(cmd.float64_args_.args_[0]) * resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::DivInt:
      save(resolve(cmd.i32_args_.args_[0]) / resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::DivFloat32:
      save(resolve(cmd.float32_args_.args_[0]) / resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::DivFloat64:
      save(resolve(cmd.float64_args_.args_[0]) / resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::ModInt:
      save(resolve(cmd.i32_args_.args_[0]) % resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::LtInt:
      save(resolve(cmd.i32_args_.args_[0]) < resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::LtFloat32:
      save(resolve(cmd.float32_args_.args_[0]) < resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::LtFloat64:
      save(resolve(cmd.float64_args_.args_[0]) < resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::LtFlags: {
      auto lhs = resolve(cmd.flags_args_.args_[0]);
      auto rhs = resolve(cmd.flags_args_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value));
    } break;
    case ir::Op::LeInt:
      save(resolve(cmd.i32_args_.args_[0]) <= resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::LeFloat32:
      save(resolve(cmd.float32_args_.args_[0]) <= resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::LeFloat64:
      save(resolve(cmd.float64_args_.args_[0]) <= resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::LeFlags: {
      auto lhs = resolve(cmd.flags_args_.args_[0]);
      auto rhs = resolve(cmd.flags_args_.args_[1]);
      save((lhs.value | rhs.value) == rhs.value);
    } break;
    case ir::Op::GtInt:
      save(resolve(cmd.i32_args_.args_[0]) > resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::GtFloat32:
      save(resolve(cmd.float32_args_.args_[0]) > resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::GtFloat64:
      save(resolve(cmd.float64_args_.args_[0]) > resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::GtFlags: {
      auto lhs = resolve(cmd.flags_args_.args_[0]);
      auto rhs = resolve(cmd.flags_args_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value));
    } break;
    case ir::Op::GeInt:
      save(resolve(cmd.i32_args_.args_[0]) >= resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::GeFloat32:
      save(resolve(cmd.float32_args_.args_[0]) >= resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::GeFloat64:
      save(resolve(cmd.float64_args_.args_[0]) >= resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::GeFlags: {
      auto lhs = resolve(cmd.flags_args_.args_[0]);
      auto rhs = resolve(cmd.flags_args_.args_[1]);
      save((lhs.value | rhs.value) == lhs.value);
    } break;
    case ir::Op::EqBool:
      save(resolve(cmd.bool_args_.args_[0]) == resolve(cmd.bool_args_.args_[1]));
      break;
    case ir::Op::EqChar:
      save(resolve(cmd.char_args_.args_[0]) == resolve(cmd.char_args_.args_[1]));
      break;
    case ir::Op::EqInt:
      save(resolve(cmd.i32_args_.args_[0]) == resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::EqFloat32:
      save(resolve(cmd.float32_args_.args_[0]) == resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::EqFloat64:
      save(resolve(cmd.float64_args_.args_[0]) == resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::EqEnum:
      save(resolve(cmd.enum_args_.args_[0]) == resolve(cmd.enum_args_.args_[1]));
      break;
    case ir::Op::EqFlags:
      save(resolve(cmd.flags_args_.args_[0]) == resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::EqType:
      save(resolve(cmd.type_args_.args_[0]) == resolve(cmd.type_args_.args_[1]));
      break;
    case ir::Op::EqAddr:
      save(resolve(cmd.addr_args_.args_[0]) == resolve(cmd.addr_args_.args_[1]));
      break;
    case ir::Op::NeChar:
      save(resolve(cmd.char_args_.args_[0]) != resolve(cmd.char_args_.args_[1]));
      break;
    case ir::Op::NeInt:
      save(resolve(cmd.i32_args_.args_[0]) != resolve(cmd.i32_args_.args_[1]));
      break;
    case ir::Op::NeFloat32:
      save(resolve(cmd.float32_args_.args_[0]) != resolve(cmd.float32_args_.args_[1]));
      break;
    case ir::Op::NeFloat64:
      save(resolve(cmd.float64_args_.args_[0]) != resolve(cmd.float64_args_.args_[1]));
      break;
    case ir::Op::NeEnum:
      save(resolve(cmd.enum_args_.args_[0]) != resolve(cmd.enum_args_.args_[1]));
      break;
    case ir::Op::NeFlags:
      save(resolve(cmd.flags_args_.args_[0]) != resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::NeType:
      save(resolve(cmd.type_args_.args_[0]) != resolve(cmd.type_args_.args_[1]));
      break;
    case ir::Op::NeAddr:
      save(resolve(cmd.addr_args_.args_[0]) != resolve(cmd.addr_args_.args_[1]));
      break;
    case ir::Op::XorBool:
      save(resolve(cmd.bool_args_.args_[0]) ^ resolve(cmd.bool_args_.args_[1]));
      break;
    case ir::Op::XorFlags:
      save(resolve(cmd.flags_args_.args_[0]) ^ resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::OrFlags:
      save(resolve(cmd.flags_args_.args_[0]) | resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::AndFlags:
      save(resolve(cmd.flags_args_.args_[0]) & resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::CreateStruct:
      save(type::Struct::Make(cmd.create_struct_.lit_));
      break;
    case ir::Op::CreateStructField: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.create_struct_field_.struct_));
      struct_to_modify->add_field(resolve(cmd.create_struct_field_.type_));
    } break;
    case ir::Op::SetStructFieldName: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.set_struct_field_name_.struct_));
      struct_to_modify->set_last_name(cmd.set_struct_field_name_.name_);
    } break;
    case ir::Op::FinalizeStruct:
      // TODO remove me.
      break;
    case ir::Op::DebugIr: LOG << call_stack.top().fn_; break;
    case ir::Op::Malloc: save(malloc(resolve(cmd.i32_arg_))); break;
    case ir::Op::Free: free(resolve<ir::Addr>(cmd.reg_).as_heap); break;
    case ir::Op::Alloca: {
      ir::Addr addr;
      addr.as_stack = stack_.size();
      addr.kind     = ir::Addr::Kind::Stack;
      save(addr);

      auto arch = Architecture::InterprettingMachine();
      stack_.append_bytes(arch.bytes(cmd.type_),
                          arch.alignment(cmd.type_));

    } break;
    case ir::Op::Ptr:
      save(type::Ptr(resolve<type::Type const *>(cmd.reg_)));
      break;
    case ir::Op::Arrow:
      save(type::Func({resolve(cmd.type_args_.args_[0])},
                      {resolve(cmd.type_args_.args_[1])}));
      break;
    case ir::Op::Array: {
      auto len = resolve(cmd.array_.len_);
      auto t   = resolve(cmd.array_.type_);
      save(len == -1 ? type::Arr(t) : type::Arr(t, len));
    } break;
    case ir::Op::VariantType: save(resolve<ir::Addr>(cmd.reg_)); break;
    case ir::Op::VariantValue: {
      auto bytes = Architecture::InterprettingMachine().bytes(Ptr(type::Type_));
      auto bytes_fwd =
          Architecture::InterprettingMachine().MoveForwardToAlignment(
              Ptr(type::Type_), bytes);
      auto addr = resolve<ir::Addr>(cmd.reg_);
      switch (addr.kind) {
        case ir::Addr::Kind::Stack:
          addr.as_stack += bytes_fwd;
          save(addr);
          break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                             bytes_fwd);
          save(addr);
          break;
        case ir::Addr::Kind::Null: NOT_YET();
      }
    } break;
    case ir::Op::PtrIncr: {
      auto addr      = resolve<ir::Addr>(cmd.ptr_incr_.ptr_);
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolve(cmd.ptr_incr_.incr_), cmd.ptr_incr_.pointee_type_);
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: save(addr.as_stack + bytes_fwd); break;
        case ir::Addr::Kind::Heap:
          save(static_cast<char *>(addr.as_heap) + bytes_fwd);
          break;
        case ir::Addr::Kind::Null: NOT_YET();
      }
    } break;
    case ir::Op::Field: {
      auto addr = resolve<ir::Addr>(cmd.field_.ptr_);
      auto *struct_type =
          resolve<type::Struct const *>(cmd.field_.struct_type_);
      size_t offset = struct_type->offset(cmd.field_.num_,
                                          Architecture::InterprettingMachine());

      if (addr.kind == ir::Addr::Kind::Stack) {
        addr.as_stack += offset;
      } else {
        addr.as_heap =
            static_cast<void *>(static_cast<char *>(addr.as_heap) + offset);
      }
      save(addr);
    } break;
    case ir::Op::PrintBool:
      std::cerr << (resolve(cmd.bool_arg_) ? "true" : "false");
      break;
    case ir::Op::PrintChar: std::cerr << resolve(cmd.char_arg_); break;
    case ir::Op::PrintInt: std::cerr << resolve(cmd.i32_arg_); break;
    case ir::Op::PrintFloat32: std::cerr << resolve(cmd.float32_arg_); break;
    case ir::Op::PrintFloat64: std::cerr << resolve(cmd.float64_arg_); break;
    case ir::Op::PrintType:
      std::cerr << resolve(cmd.type_arg_)->to_string();
      break;
    case ir::Op::PrintEnum:
      NOT_YET();
      /*
      std::cerr << resolved[0].type->as<type::Enum>().members_[e.value];
      */
    case ir::Op::PrintFlags:
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
      break;
    case ir::Op::PrintAddr:
      std::cerr << resolve(cmd.addr_arg_).to_string();
      break;
    case ir::Op::PrintCharBuffer:
      std::cerr << resolve(cmd.char_buf_arg_);
      break;
    case ir::Op::Call: {
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the
      // interpretter.
      base::vector<ir::Addr> ret_slots;
      if (cmd.call_.outs_ != nullptr) {
        ret_slots.reserve(cmd.call_.outs_->outs_.size());
        for (const auto &out_param : cmd.call_.outs_->outs_) {
          if (out_param.is_loc_) {
            ret_slots.push_back(resolve<ir::Addr>(out_param.reg_));
          } else {
            ir::Addr addr;
            addr.kind    = ir::Addr::Kind::Heap;
            addr.as_heap = call_stack.top().regs_.raw(out_param.reg_.value);
            ret_slots.push_back(addr);
          }
        }
      }

      // TODO we can compute the exact required size.
      base::untyped_buffer call_buf(32);

      size_t offset   = 0;
      auto arch       = Architecture::InterprettingMachine();
      auto &long_args = cmd.call_.long_args_->args_;
      for (size_t i = 0; i < cmd.call_.long_args_->is_reg_.size(); ++i) {
        bool is_reg = cmd.call_.long_args_->is_reg_[i];
        auto *t     = (i < cmd.call_.long_args_->type_->input.size())
                      ? cmd.call_.long_args_->type_->input.at(i)
                      : cmd.call_.long_args_->type_->output.at(
                            i - cmd.call_.long_args_->type_->input.size());

        offset = arch.MoveForwardToAlignment(t, offset);
        call_buf.pad_to(offset);

        if (t == type::Bool) {
          call_buf.append(
              is_reg ? resolve<bool>(long_args.get<ir::Register>(offset))
                     : long_args.get<bool>(offset));
        } else if (t == type::Char) {
          call_buf.append(
              is_reg ? resolve<char>(long_args.get<ir::Register>(offset))
                     : long_args.get<char>(offset));
        } else if (t == type::Int) {
          call_buf.append(
              is_reg ? resolve<i32>(long_args.get<ir::Register>(offset))
                     : long_args.get<i32>(offset));
        } else if (t == type::Float32) {
          call_buf.append(
              is_reg ? resolve<float>(long_args.get<ir::Register>(offset))
                     : long_args.get<float>(offset));
        } else if (t == type::Float64) {
          call_buf.append(
              is_reg ? resolve<double>(long_args.get<ir::Register>(offset))
                     : long_args.get<double>(offset));
        } else if (t == type::Scope) {
          call_buf.append(is_reg ? resolve<ast::ScopeLiteral *>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<ast::ScopeLiteral *>(offset));
        } else if (t == type::Type_) {
          call_buf.append(is_reg ? resolve<type::Type const *>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<type::Type const *>(offset));
        } else if (t->is<type::CharBuffer>()) {
          call_buf.append(is_reg ? resolve<std::string_view>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<std::string_view>(offset));
        } else if (t->is<type::Function>()) {
          call_buf.append(
              is_reg ? resolve<ir::Func *>(long_args.get<ir::Register>(offset))
                     : long_args.get<ir::Func *>(offset));
        } else if (t == type::Module) {
          call_buf.append(is_reg ? resolve<Module const *>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<Module const *>(offset));
        } else if (t == type::Generic) {
          // TODO mostly wrong.
          call_buf.append(is_reg ? resolve<ast::Function *>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<ast::Function *>(offset));
        } else if (t == type::Block || t == type::OptBlock) {
          call_buf.append(is_reg ? resolve<ir::BlockSequence>(
                                       long_args.get<ir::Register>(offset))
                                 : long_args.get<ir::BlockSequence>(offset));
        } else if (t->is<type::Variant>()) {
          call_buf.append(
              is_reg ? resolve<ir::Addr>(long_args.get<ir::Register>(offset))
                     : long_args.get<ir::Addr>(offset));
        } else if (t->is<type::Pointer>()) {
          ASSERT(is_reg);
          call_buf.append(
              resolve<ir::Addr>(long_args.get<ir::Register>(offset)));
        } else {
          NOT_YET(t->to_string());
        }

        offset += is_reg ? sizeof(ir::Register) : arch.bytes(t);
      }
      // TODO you need to be able to determine how many args there are
      if (cmd.call_.fn_.is_reg_) {
        // TODO what if the register is a foerign fn?
        backend::Execute(resolve<ir::Func *>(cmd.call_.fn_.reg_), call_buf,
                         ret_slots, this);
      } else if (cmd.call_.fn_.val_.is_fn_) {
        backend::Execute(cmd.call_.fn_.val_.fn_, call_buf, ret_slots, this);
      } else {
        if (cmd.call_.fn_.val_.foreign_.name_ == "malloc") {
          ir::Addr addr;
          addr.kind    = ir::Addr::Kind::Heap;
          addr.as_heap = malloc(call_buf.get<i32>(0));
          StoreValue(addr, ret_slots.at(0), &stack_);
        } else if (cmd.call_.fn_.val_.foreign_.name_ == "abs") {
          StoreValue(std::abs(call_buf.get<i32>(0)), ret_slots.at(0), &stack_);
        } else if (cmd.call_.fn_.val_.foreign_.name_ == "sleep") {
          std::this_thread::sleep_for(
              std::chrono::seconds(call_buf.get<i32>(0)));
        } else {
          NOT_YET();
        }
      }
    } break;
    case ir::Op::CreateTuple: {
      save(new type::Tuple(base::vector<type::Type const *>{}));
    } break;
    case ir::Op::AppendToTuple: {
      auto *tuple_to_modify =
          ASSERT_NOT_NULL(resolve<type::Tuple *>(cmd.store_type_.addr_));
      tuple_to_modify->entries_.push_back(resolve(cmd.store_type_.val_));
    } break;
    case ir::Op::FinalizeTuple:
      save(resolve<type::Tuple *>(cmd.reg_)->finalize());
      break;
    case ir::Op::CreateVariant: {
      save(new type::Variant(base::vector<type::Type const *>{}));
    } break;
    case ir::Op::AppendToVariant: {
      auto *variant_to_modify =
          ASSERT_NOT_NULL(resolve<type::Variant *>(cmd.store_type_.addr_));
      variant_to_modify->variants_.push_back(resolve(cmd.store_type_.val_));
    } break;
    case ir::Op::FinalizeVariant:
      save(resolve<type::Variant *>(cmd.reg_)->finalize());
      break;
    case ir::Op::CastIntToFloat32:
      save(static_cast<float>(resolve<i32>(cmd.reg_)));
      break;
    case ir::Op::CastIntToFloat64:
      save(static_cast<double>(resolve<i32>(cmd.reg_)));
      break;
    case ir::Op::CastPtr: save(resolve<ir::Addr>(cmd.typed_reg_.get())); break;
    case ir::Op::CreateBlockSeq:
      save(new base::vector<ir::BlockSequence>{});
      break;
    case ir::Op::AppendToBlockSeq: {
      auto *block_seq_to_modify = ASSERT_NOT_NULL(
          resolve<base::vector<ir::BlockSequence> *>(cmd.store_block_.addr_));
      block_seq_to_modify->push_back(resolve(cmd.store_block_.val_));
    } break;
    case ir::Op::FinalizeBlockSeq: {
      auto *block_seq = resolve<base::vector<ir::BlockSequence> *>(cmd.reg_);
      auto seq = ir::MakeBlockSeq(*block_seq);
      delete block_seq;
      save(seq);
    } break;
    case ir::Op::BlockSeqContains: {
      auto *seq = resolve<ir::BlockSequence>(cmd.block_seq_contains_.reg_).seq_;
      save(std::any_of(seq->begin(), seq->end(), [&](ast::BlockLiteral *lit) {
        return lit == cmd.block_seq_contains_.lit_;
      }));
    } break;
    case ir::Op::SetRetBool:
      StoreValue(resolve(cmd.set_ret_bool_.val_),
                 ret_slots.at(cmd.set_ret_bool_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetChar:
      StoreValue(resolve(cmd.set_ret_char_.val_),
                 ret_slots.at(cmd.set_ret_char_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetI32:
      StoreValue(resolve(cmd.set_ret_i32_.val_),
                 ret_slots.at(cmd.set_ret_i32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFloat32:
      StoreValue(resolve(cmd.set_ret_float32_.val_),
                 ret_slots.at(cmd.set_ret_float32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFloat64:
      StoreValue(resolve(cmd.set_ret_float64_.val_),
                 ret_slots.at(cmd.set_ret_float64_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetType:
      StoreValue(resolve(cmd.set_ret_type_.val_),
                 ret_slots.at(cmd.set_ret_type_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetCharBuf:
      StoreValue(resolve(cmd.set_ret_char_buf_.val_),
                 ret_slots.at(cmd.set_ret_char_buf_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetAddr:
      StoreValue(resolve(cmd.set_ret_addr_.val_),
                 ret_slots.at(cmd.set_ret_addr_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetEnum:
      StoreValue(resolve(cmd.set_ret_enum_.val_),
                 ret_slots.at(cmd.set_ret_enum_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFlags:
      StoreValue(resolve(cmd.set_ret_flags_.val_),
                 ret_slots.at(cmd.set_ret_flags_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFunc:
      StoreValue(resolve(cmd.set_ret_func_.val_),
                 ret_slots.at(cmd.set_ret_func_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetScope:
      StoreValue(resolve(cmd.set_ret_scope_.val_),
                 ret_slots.at(cmd.set_ret_scope_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetGeneric:
      StoreValue(resolve(cmd.set_ret_generic_.val_),
                 ret_slots.at(cmd.set_ret_generic_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetModule:
      StoreValue(resolve(cmd.set_ret_module_.val_),
                 ret_slots.at(cmd.set_ret_module_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetBlock:
      StoreValue(resolve(cmd.set_ret_block_.val_),
                 ret_slots.at(cmd.set_ret_block_.ret_num_), &stack_);
      break;
    case ir::Op::StoreBool:
      StoreValue(resolve(cmd.store_bool_.val_),
                 resolve<ir::Addr>(cmd.store_bool_.addr_), &stack_);
      break;
    case ir::Op::StoreChar:
      StoreValue(resolve(cmd.store_char_.val_),
                 resolve<ir::Addr>(cmd.store_char_.addr_), &stack_);
      break;
    case ir::Op::StoreI32:
      StoreValue(resolve(cmd.store_i32_.val_),
                 resolve<ir::Addr>(cmd.store_i32_.addr_), &stack_);
      break;
    case ir::Op::StoreFloat32:
      StoreValue(resolve(cmd.store_float32_.val_),
                 resolve<ir::Addr>(cmd.store_float32_.addr_), &stack_);
      break;
    case ir::Op::StoreFloat64:
      StoreValue(resolve(cmd.store_float64_.val_),
                 resolve<ir::Addr>(cmd.store_float64_.addr_), &stack_);
      break;
    case ir::Op::StoreType:
      StoreValue(resolve(cmd.store_type_.val_),
                 resolve<ir::Addr>(cmd.store_type_.addr_), &stack_);
      break;
    case ir::Op::StoreEnum:
      StoreValue(resolve(cmd.store_enum_.val_),
                 resolve<ir::Addr>(cmd.store_enum_.addr_), &stack_);
      break;
    case ir::Op::StoreFunc:
      StoreValue(resolve(cmd.store_func_.val_),
                 resolve<ir::Addr>(cmd.store_func_.addr_), &stack_);
      break;
    case ir::Op::StoreFlags:
      StoreValue(resolve(cmd.store_flags_.val_),
                 resolve<ir::Addr>(cmd.store_flags_.addr_), &stack_);
      break;
    case ir::Op::StoreAddr:
      StoreValue(resolve(cmd.store_addr_.val_),
                 resolve<ir::Addr>(cmd.store_addr_.addr_), &stack_);
      break;
    case ir::Op::PhiBool:
      save(resolve(cmd.phi_bool_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiChar:
      save(resolve(cmd.phi_char_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiI32:
      save(resolve(cmd.phi_i32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFloat32:
      save(resolve(cmd.phi_float32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFloat64:
      save(resolve(cmd.phi_float64_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiType:
      save(resolve(cmd.phi_type_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiAddr:
      save(resolve(cmd.phi_addr_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiBlock:
      save(resolve(cmd.phi_block_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::CondJump:
      return cmd.cond_jump_.blocks_[resolve<bool>(cmd.cond_jump_.cond_)];
    case ir::Op::UncondJump: return cmd.uncond_jump_.block_;
    case ir::Op::ReturnJump: return ir::BlockIndex{-1};
    case ir::Op::BlockSeqJump: {
      auto bseq = resolve(cmd.block_seq_jump_.bseq_);

      for (auto *bl : *bseq.seq_) {
        auto iter = cmd.block_seq_jump_.jump_table_->find(bl);
        if (iter != cmd.block_seq_jump_.jump_table_->end()) {
          return iter->second;
        }
      }
      NOT_YET(bseq.seq_);
    } break;
  }
  return ir::BlockIndex{-2};
}
}  // namespace backend
