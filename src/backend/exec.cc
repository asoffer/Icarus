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
#include "ast/codeblock.h"
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

namespace IR {
std::string_view SaveStringGlobally(std::string const &s);
IR::BlockSequence MakeBlockSeq(const base::vector<IR::BlockSequence> &blocks);
}  // namespace IR

namespace backend {

void Execute(IR::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<IR::Addr> &ret_slots,
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
T ExecContext::resolve(IR::Register val) const {
  return call_stack.top().regs_.get<T>(val.value);
}

ExecContext::ExecContext() : stack_(50u) {}

IR::BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(IR::Func *fn, const base::untyped_buffer &arguments)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_)) {
  regs_.write(0, arguments);
}

IR::BlockIndex ExecContext::ExecuteBlock(
    const base::vector<IR::Addr> &ret_slots) {
  IR::BlockIndex result;
  ASSERT(current_block().cmds_.size() > 0u);
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter++, ret_slots);
  } while (result == IR::BlockIndex{-2});
  return result;
}

template <typename T>
static T LoadValue(IR::Addr addr, const base::untyped_buffer &stack) {
  switch (addr.kind) {
    case IR::Addr::Kind::Null: UNREACHABLE();
    case IR::Addr::Kind::Heap: return *static_cast<T *>(addr.as_heap); break;
    case IR::Addr::Kind::Stack: return stack.get<T>(addr.as_stack); break;
  }
  UNREACHABLE();
}

template <typename T>
static void StoreValue(T val, IR::Addr addr, base::untyped_buffer *stack) {
  switch (addr.kind) {
    case IR::Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE();
    case IR::Addr::Kind::Stack: stack->set(addr.as_stack, val); return;
    case IR::Addr::Kind::Heap: *static_cast<T *>(addr.as_heap) = val;
  }
}

IR::BlockIndex ExecContext::ExecuteCmd(
    const IR::Cmd &cmd, const base::vector<IR::Addr> &ret_slots) {
  auto save = [&](auto val) {
    call_stack.top().regs_.set(cmd.result.value, val);
  };

  switch (cmd.op_code_) {
    case IR::Op::Death: UNREACHABLE();
    case IR::Op::Trunc:
      save(static_cast<char>(resolve<i32>(cmd.trunc_.reg_)));
      break;
    case IR::Op::Extend:
      save(static_cast<i32>(resolve<char>(cmd.extend_.reg_)));
      break;
    case IR::Op::Bytes:
      save(
          Architecture::InterprettingMachine().bytes(resolve(cmd.bytes_.arg_)));
      break;
    case IR::Op::Align:
      save(Architecture::InterprettingMachine().alignment(
          resolve(cmd.align_.arg_)));
      break;
    case IR::Op::Not: save(!resolve<bool>(cmd.not_.reg_)); break;
    case IR::Op::NegInt: save(-resolve<i32>(cmd.neg_int_.reg_)); break;
    case IR::Op::NegReal: save(-resolve<double>(cmd.neg_real_.reg_)); break;
    case IR::Op::ArrayLength:
      save(resolve<IR::Addr>(cmd.array_data_.arg_));
      break;
    case IR::Op::ArrayData: {
      auto addr = resolve<IR::Addr>(cmd.array_data_.arg_);
      switch (addr.kind) {
        case IR::Addr::Kind::Null: UNREACHABLE();
        case IR::Addr::Kind::Stack:
          addr.as_stack +=
              Architecture::InterprettingMachine().bytes(type::Int);
          break;
        case IR::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(
              static_cast<u8 *>(addr.as_heap) +
              Architecture::InterprettingMachine().bytes(type::Int));
          break;
      }
      save(addr);

    } break;
    case IR::Op::LoadBool:
      save(LoadValue<bool>(resolve<IR::Addr>(cmd.load_bool_.arg_), stack_));
      break;
    case IR::Op::LoadChar:
      save(LoadValue<char>(resolve<IR::Addr>(cmd.load_char_.arg_), stack_));
      break;
    case IR::Op::LoadInt:
      save(LoadValue<i32>(resolve<IR::Addr>(cmd.load_int_.arg_), stack_));
      break;
    case IR::Op::LoadReal:
      save(LoadValue<double>(resolve<IR::Addr>(cmd.load_real_.arg_), stack_));
      break;
    case IR::Op::LoadType:
      save(LoadValue<type::Type const *>(resolve<IR::Addr>(cmd.load_type_.arg_),
                                         stack_));
      break;
    case IR::Op::LoadEnum:
      save(LoadValue<size_t>(resolve<IR::Addr>(cmd.load_enum_.arg_), stack_));
      break;
    case IR::Op::LoadFlags:
      save(LoadValue<size_t>(resolve<IR::Addr>(cmd.load_flags_.arg_), stack_));
      break;
    case IR::Op::LoadAddr:
      save(LoadValue<IR::Addr>(resolve<IR::Addr>(cmd.load_addr_.arg_), stack_));
      break;
    case IR::Op::AddInt:
      save(resolve(cmd.add_int_.args_[0]) + resolve(cmd.add_int_.args_[1]));
      break;
    case IR::Op::AddReal:
      save(resolve(cmd.add_real_.args_[0]) + resolve(cmd.add_real_.args_[1]));
      break;
    case IR::Op::SubInt:
      save(resolve(cmd.sub_int_.args_[0]) - resolve(cmd.sub_int_.args_[1]));
      break;
    case IR::Op::SubReal:
      save(resolve(cmd.sub_real_.args_[0]) - resolve(cmd.sub_real_.args_[1]));
      break;
    case IR::Op::MulInt:
      save(resolve(cmd.mul_int_.args_[0]) * resolve(cmd.mul_int_.args_[1]));
      break;
    case IR::Op::MulReal:
      save(resolve(cmd.mul_real_.args_[0]) * resolve(cmd.mul_real_.args_[1]));
      break;
    case IR::Op::DivInt:
      save(resolve(cmd.div_int_.args_[0]) / resolve(cmd.div_int_.args_[1]));
      break;
    case IR::Op::DivReal:
      save(resolve(cmd.div_real_.args_[0]) / resolve(cmd.div_real_.args_[1]));
      break;
    case IR::Op::ModInt:
      save(resolve(cmd.mod_int_.args_[0]) % resolve(cmd.mod_int_.args_[1]));
      break;
    case IR::Op::ModReal:
      save(std::fmod(resolve(cmd.mod_real_.args_[0]),
                     resolve(cmd.mod_real_.args_[1])));
      break;
    case IR::Op::LtInt:
      save(resolve(cmd.lt_int_.args_[0]) < resolve(cmd.lt_int_.args_[1]));
      break;
    case IR::Op::LtReal:
      save(resolve(cmd.lt_real_.args_[0]) < resolve(cmd.lt_real_.args_[1]));
      break;
    case IR::Op::LtFlags: {
      auto lhs = resolve(cmd.lt_flags_.args_[0]);
      auto rhs = resolve(cmd.lt_flags_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value));
    } break;
    case IR::Op::LeInt:
      save(resolve(cmd.le_int_.args_[0]) <= resolve(cmd.le_int_.args_[1]));
      break;
    case IR::Op::LeReal:
      save(resolve(cmd.le_real_.args_[0]) <= resolve(cmd.le_real_.args_[1]));
      break;
    case IR::Op::LeFlags: {
      auto lhs = resolve(cmd.le_flags_.args_[0]);
      auto rhs = resolve(cmd.le_flags_.args_[1]);
      save((lhs.value | rhs.value) == rhs.value);
    } break;
    case IR::Op::GtInt:
      save(resolve(cmd.gt_int_.args_[0]) > resolve(cmd.gt_int_.args_[1]));
      break;
    case IR::Op::GtReal:
      save(resolve(cmd.gt_real_.args_[0]) > resolve(cmd.gt_real_.args_[1]));
      break;
    case IR::Op::GtFlags: {
      auto lhs = resolve(cmd.gt_flags_.args_[0]);
      auto rhs = resolve(cmd.gt_flags_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value));
    } break;
    case IR::Op::GeInt:
      save(resolve(cmd.ge_int_.args_[0]) >= resolve(cmd.ge_int_.args_[1]));
      break;
    case IR::Op::GeReal:
      save(resolve(cmd.ge_real_.args_[0]) >= resolve(cmd.ge_real_.args_[1]));
      break;
    case IR::Op::GeFlags: {
      auto lhs = resolve(cmd.ge_flags_.args_[0]);
      auto rhs = resolve(cmd.ge_flags_.args_[1]);
      save((lhs.value | rhs.value) == lhs.value);
    } break;
    case IR::Op::EqBool:
      save(resolve<bool>(cmd.eq_bool_.args_[0]) ==
           resolve<bool>(cmd.eq_bool_.args_[1]));
      break;
    case IR::Op::EqChar:
      save(resolve(cmd.eq_char_.args_[0]) == resolve(cmd.eq_char_.args_[1]));
      break;
    case IR::Op::EqInt:
      save(resolve(cmd.eq_int_.args_[0]) == resolve(cmd.eq_int_.args_[1]));
      break;
    case IR::Op::EqReal:
      save(resolve(cmd.eq_real_.args_[0]) == resolve(cmd.eq_real_.args_[1]));
      break;
    case IR::Op::EqEnum:
      save(resolve(cmd.eq_enum_.args_[0]) == resolve(cmd.eq_enum_.args_[1]));
      break;
    case IR::Op::EqFlags:
      save(resolve(cmd.eq_flags_.args_[0]) == resolve(cmd.eq_flags_.args_[1]));
      break;
    case IR::Op::EqType:
      save(resolve(cmd.eq_type_.args_[0]) == resolve(cmd.eq_type_.args_[1]));
      break;
    case IR::Op::EqAddr:
      save(resolve(cmd.eq_addr_.args_[0]) == resolve(cmd.eq_addr_.args_[1]));
      break;
    case IR::Op::NeChar:
      save(resolve(cmd.ne_char_.args_[0]) != resolve(cmd.ne_char_.args_[1]));
      break;
    case IR::Op::NeInt:
      save(resolve(cmd.ne_int_.args_[0]) != resolve(cmd.ne_int_.args_[1]));
      break;
    case IR::Op::NeReal:
      save(resolve(cmd.ne_real_.args_[0]) != resolve(cmd.ne_real_.args_[1]));
      break;
    case IR::Op::NeEnum:
      save(resolve(cmd.ne_enum_.args_[0]) != resolve(cmd.ne_enum_.args_[1]));
      break;
    case IR::Op::NeFlags:
      save(resolve(cmd.ne_flags_.args_[0]) != resolve(cmd.ne_flags_.args_[1]));
      break;
    case IR::Op::NeType:
      save(resolve(cmd.ne_type_.args_[0]) != resolve(cmd.ne_type_.args_[1]));
      break;
    case IR::Op::NeAddr:
      save(resolve(cmd.ne_addr_.args_[0]) != resolve(cmd.ne_addr_.args_[1]));
      break;
    case IR::Op::XorBool:
      save(resolve(cmd.xor_bool_.args_[0]) ^ resolve(cmd.xor_bool_.args_[1]));
      break;
    case IR::Op::XorFlags:
      save(resolve(cmd.xor_flags_.args_[0]) ^ resolve(cmd.xor_flags_.args_[1]));
      break;
    case IR::Op::OrFlags:
      save(resolve(cmd.or_flags_.args_[0]) | resolve(cmd.or_flags_.args_[1]));
      break;
    case IR::Op::AndFlags:
      save(resolve(cmd.and_flags_.args_[0]) & resolve(cmd.and_flags_.args_[1]));
      break;
    case IR::Op::CreateStruct:
      save(type::Struct::Make(cmd.create_struct_.lit_));
      break;
    case IR::Op::CreateStructField: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.create_struct_field_.struct_));
      struct_to_modify->add_field(resolve(cmd.create_struct_field_.type_));
    } break;
    case IR::Op::SetStructFieldName: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.set_struct_field_name_.struct_));
      struct_to_modify->set_last_name(cmd.set_struct_field_name_.name_);
    } break;
    case IR::Op::FinalizeStruct:
      // TODO remove me.
      break;
    case IR::Op::DebugIr: LOG << call_stack.top().fn_; break;
    case IR::Op::Malloc: save(malloc(resolve(cmd.malloc_.arg_))); break;
    case IR::Op::Free: free(resolve<IR::Addr>(cmd.free_.reg_).as_heap); break;
    case IR::Op::Alloca: {
      IR::Addr addr;
      addr.as_stack = stack_.size();
      addr.kind     = IR::Addr::Kind::Stack;
      save(addr);

      auto arch = Architecture::InterprettingMachine();
      stack_.append_bytes(arch.bytes(cmd.alloca_.type_),
                          arch.alignment(cmd.alloca_.type_));

    } break;
    case IR::Op::Ptr:
      save(type::Ptr(resolve<type::Type const *>(cmd.ptr_.reg_)));
      break;
    case IR::Op::Arrow:
      save(type::Func({resolve(cmd.arrow_.args_[0])},
                      {resolve(cmd.arrow_.args_[1])}));
      break;
    case IR::Op::Array: {
      auto len = resolve(cmd.array_.len_);
      auto t   = resolve(cmd.array_.type_);
      save(len == -1 ? type::Arr(t) : type::Arr(t, len));
    } break;
    case IR::Op::VariantType:
      save(resolve<IR::Addr>(cmd.variant_type_.reg_));
      break;
    case IR::Op::VariantValue: {
      auto bytes = Architecture::InterprettingMachine().bytes(Ptr(type::Type_));
      auto bytes_fwd =
          Architecture::InterprettingMachine().MoveForwardToAlignment(
              Ptr(type::Type_), bytes);
      auto addr = resolve<IR::Addr>(cmd.variant_value_.reg_);
      switch (addr.kind) {
        case IR::Addr::Kind::Stack:
          addr.as_stack += bytes_fwd;
          save(addr);
          break;
        case IR::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                             bytes_fwd);
          save(addr);
          break;
        case IR::Addr::Kind::Null: NOT_YET();
      }
    } break;
    case IR::Op::PtrIncr: {
      auto addr      = resolve<IR::Addr>(cmd.ptr_incr_.ptr_);
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          resolve(cmd.ptr_incr_.incr_), cmd.ptr_incr_.pointee_type_);
      switch (addr.kind) {
        case IR::Addr::Kind::Stack: save(addr.as_stack + bytes_fwd); break;
        case IR::Addr::Kind::Heap:
          save(static_cast<char *>(addr.as_heap) + bytes_fwd);
          break;
        case IR::Addr::Kind::Null: NOT_YET();
      }
    } break;
    case IR::Op::Field: {
      auto addr = resolve<IR::Addr>(cmd.field_.ptr_);
      auto *struct_type =
          resolve<type::Struct const *>(cmd.field_.struct_type_);
      size_t offset = struct_type->offset(cmd.field_.num_,
                                          Architecture::InterprettingMachine());

      if (addr.kind == IR::Addr::Kind::Stack) {
        addr.as_stack += offset;
      } else {
        addr.as_heap =
            static_cast<void *>(static_cast<char *>(addr.as_heap) + offset);
      }
      save(addr);
    } break;
    case IR::Op::PrintBool:
      std::cerr << (resolve(cmd.print_bool_.arg_) ? "true" : "false");
      break;
    case IR::Op::PrintChar: std::cerr << resolve(cmd.print_char_.arg_); break;
    case IR::Op::PrintInt: std::cerr << resolve(cmd.print_int_.arg_); break;
    case IR::Op::PrintReal: std::cerr << resolve(cmd.print_real_.arg_); break;
    case IR::Op::PrintType:
      std::cerr << resolve(cmd.print_type_.arg_)->to_string();
      break;
    case IR::Op::PrintEnum:
      NOT_YET();
      /*
      std::cerr << resolved[0].type->as<type::Enum>().members_[e.value];
      */
    case IR::Op::PrintFlags:
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
    case IR::Op::PrintAddr:
      std::cerr << resolve(cmd.print_addr_.arg_).to_string();
      break;
    case IR::Op::PrintCharBuffer:
      std::cerr << resolve(cmd.print_char_buffer_.arg_);
      break;
    case IR::Op::Call: {
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the
      // interpretter.
      base::vector<IR::Addr> ret_slots;
      if (cmd.call_.outs_ != nullptr) {
        ret_slots.reserve(cmd.call_.outs_->outs_.size());
        for (const auto &out_param : cmd.call_.outs_->outs_) {
          if (out_param.is_loc_) {
            ret_slots.push_back(resolve<IR::Addr>(out_param.reg_));
          } else {
            IR::Addr addr;
            addr.kind    = IR::Addr::Kind::Heap;
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
              is_reg ? resolve<bool>(long_args.get<IR::Register>(offset))
                     : long_args.get<bool>(offset));
        } else if (t == type::Char) {
          call_buf.append(
              is_reg ? resolve<char>(long_args.get<IR::Register>(offset))
                     : long_args.get<char>(offset));
        } else if (t == type::Int) {
          call_buf.append(
              is_reg ? resolve<i32>(long_args.get<IR::Register>(offset))
                     : long_args.get<i32>(offset));
        } else if (t == type::Real) {
          call_buf.append(
              is_reg ? resolve<double>(long_args.get<IR::Register>(offset))
                     : long_args.get<double>(offset));
        } else if (t == type::Scope) {
          call_buf.append(is_reg ? resolve<AST::ScopeLiteral *>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<AST::ScopeLiteral *>(offset));
        } else if (t == type::Type_) {
          call_buf.append(is_reg ? resolve<type::Type const *>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<type::Type const *>(offset));
        } else if (t->is<type::CharBuffer>()) {
          call_buf.append(is_reg ? resolve<std::string_view>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<std::string_view>(offset));
        } else if (t->is<type::Function>()) {
          call_buf.append(
              is_reg ? resolve<IR::Func *>(long_args.get<IR::Register>(offset))
                     : long_args.get<IR::Func *>(offset));
        } else if (t == type::Module) {
          call_buf.append(is_reg ? resolve<Module const *>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<Module const *>(offset));
        } else if (t == type::Generic) {
          // TODO mostly wrong.
          call_buf.append(is_reg ? resolve<AST::Function *>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<AST::Function *>(offset));
        } else if (t == type::Block || t == type::OptBlock) {
          call_buf.append(is_reg ? resolve<IR::BlockSequence>(
                                       long_args.get<IR::Register>(offset))
                                 : long_args.get<IR::BlockSequence>(offset));
        } else if (t->is<type::Variant>()) {
          call_buf.append(
              is_reg ? resolve<IR::Addr>(long_args.get<IR::Register>(offset))
                     : long_args.get<IR::Addr>(offset));
        } else if (t->is<type::Pointer>()) {
          ASSERT(is_reg);
          call_buf.append(
              resolve<IR::Addr>(long_args.get<IR::Register>(offset)));
        } else {
          NOT_YET(t->to_string());
        }

        offset += is_reg ? sizeof(IR::Register) : arch.bytes(t);
      }
      // TODO you need to be able to determine how many args there are
      if (cmd.call_.fn_.is_reg_) {
        // TODO what if the register is a foerign fn?
        backend::Execute(resolve<IR::Func *>(cmd.call_.fn_.reg_), call_buf,
                         ret_slots, this);
      } else if (cmd.call_.fn_.val_.is_fn_) {
        backend::Execute(cmd.call_.fn_.val_.fn_, call_buf, ret_slots, this);
      } else {
        if (cmd.call_.fn_.val_.foreign_.name_ == "malloc") {
          IR::Addr addr;
          addr.kind    = IR::Addr::Kind::Heap;
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
    case IR::Op::CreateTuple: {
      save(new type::Tuple(base::vector<type::Type const *>{}));
    } break;
    case IR::Op::AppendToTuple: {
      auto *tuple_to_modify =
          ASSERT_NOT_NULL(resolve<type::Tuple *>(cmd.append_to_tuple_.tup_));
      tuple_to_modify->entries_.push_back(resolve(cmd.append_to_tuple_.arg_));
    } break;
    case IR::Op::FinalizeTuple: {
      save(resolve<type::Tuple *>(cmd.finalize_tuple_.tup_)->finalize());
    } break;
    case IR::Op::CreateVariant: {
      save(new type::Variant(base::vector<type::Type const *>{}));
    } break;
    case IR::Op::AppendToVariant: {
      auto *variant_to_modify = ASSERT_NOT_NULL(
          resolve<type::Variant *>(cmd.append_to_variant_.var_));
      variant_to_modify->variants_.push_back(
          resolve(cmd.append_to_variant_.arg_));
    } break;
    case IR::Op::FinalizeVariant: {
      save(resolve<type::Variant *>(cmd.finalize_variant_.var_)->finalize());
    } break;
    case IR::Op::CastIntToReal:
      save(static_cast<double>(resolve<i32>(cmd.cast_int_to_real_.reg_)));
      break;
    case IR::Op::CastPtr: save(resolve<IR::Addr>(cmd.cast_ptr_.reg_)); break;
    case IR::Op::AddCodeBlock: NOT_YET();
    case IR::Op::CreateBlockSeq: {
      save(new base::vector<IR::BlockSequence>{});
    } break;
    case IR::Op::AppendToBlockSeq: {
      auto *block_seq_to_modify =
          ASSERT_NOT_NULL(resolve<base::vector<IR::BlockSequence> *>(
              cmd.append_to_block_seq_.block_seq_));
      block_seq_to_modify->push_back(resolve(cmd.append_to_block_seq_.arg_));
    } break;
    case IR::Op::FinalizeBlockSeq: {
      auto *block_seq = resolve<base::vector<IR::BlockSequence> *>(
          cmd.finalize_block_seq_.block_seq_);
      auto seq = IR::MakeBlockSeq(*block_seq);
      delete block_seq;
      save(seq);
    } break;
    case IR::Op::BlockSeqContains: {
      auto *seq = resolve<IR::BlockSequence>(cmd.block_seq_contains_.reg_).seq_;
      save(std::any_of(seq->begin(), seq->end(), [&](AST::BlockLiteral *lit) {
        return lit == cmd.block_seq_contains_.lit_;
      }));
    } break;
    case IR::Op::SetReturnBool:
      StoreValue(resolve(cmd.set_return_bool_.val_),
                 ret_slots.at(cmd.set_return_bool_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnChar:
      StoreValue(resolve(cmd.set_return_char_.val_),
                 ret_slots.at(cmd.set_return_char_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnInt:
      StoreValue(resolve(cmd.set_return_int_.val_),
                 ret_slots.at(cmd.set_return_int_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnReal:
      StoreValue(resolve(cmd.set_return_real_.val_),
                 ret_slots.at(cmd.set_return_real_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnType:
      StoreValue(resolve(cmd.set_return_type_.val_),
                 ret_slots.at(cmd.set_return_type_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnCharBuf:
      StoreValue(resolve(cmd.set_return_char_buf_.val_),
                 ret_slots.at(cmd.set_return_char_buf_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnAddr:
      StoreValue(resolve(cmd.set_return_addr_.val_),
                 ret_slots.at(cmd.set_return_addr_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnEnum:
      StoreValue(resolve(cmd.set_return_enum_.val_),
                 ret_slots.at(cmd.set_return_enum_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnFlags:
      StoreValue(resolve(cmd.set_return_flags_.val_),
                 ret_slots.at(cmd.set_return_flags_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnFunc:
      StoreValue(resolve(cmd.set_return_func_.val_),
                 ret_slots.at(cmd.set_return_func_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnScope:
      StoreValue(resolve(cmd.set_return_scope_.val_),
                 ret_slots.at(cmd.set_return_scope_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnModule:
      StoreValue(resolve(cmd.set_return_module_.val_),
                 ret_slots.at(cmd.set_return_module_.ret_num_), &stack_);
      break;
    case IR::Op::SetReturnBlock:
      StoreValue(resolve(cmd.set_return_block_.val_),
                 ret_slots.at(cmd.set_return_block_.ret_num_), &stack_);
      break;
    case IR::Op::StoreBool:
      StoreValue(resolve(cmd.store_bool_.val_),
                 resolve<IR::Addr>(cmd.store_bool_.addr_), &stack_);
      break;
    case IR::Op::StoreChar:
      StoreValue(resolve(cmd.store_char_.val_),
                 resolve<IR::Addr>(cmd.store_char_.addr_), &stack_);
      break;
    case IR::Op::StoreInt:
      StoreValue(resolve(cmd.store_int_.val_),
                 resolve<IR::Addr>(cmd.store_int_.addr_), &stack_);
      break;
    case IR::Op::StoreReal:
      StoreValue(resolve(cmd.store_real_.val_),
                 resolve<IR::Addr>(cmd.store_real_.addr_), &stack_);
      break;
    case IR::Op::StoreType:
      StoreValue(resolve(cmd.store_type_.val_),
                 resolve<IR::Addr>(cmd.store_type_.addr_), &stack_);
      break;
    case IR::Op::StoreEnum:
      StoreValue(resolve(cmd.store_enum_.val_),
                 resolve<IR::Addr>(cmd.store_enum_.addr_), &stack_);
      break;
    case IR::Op::StoreFlags:
      StoreValue(resolve(cmd.store_flags_.val_),
                 resolve<IR::Addr>(cmd.store_flags_.addr_), &stack_);
      break;
    case IR::Op::StoreAddr:
      StoreValue(resolve(cmd.store_addr_.val_),
                 resolve<IR::Addr>(cmd.store_addr_.addr_), &stack_);
      break;
    case IR::Op::PhiBool:
      save(resolve(cmd.phi_bool_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiChar:
      save(resolve(cmd.phi_char_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiInt:
      save(resolve(cmd.phi_int_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiReal:
      save(resolve(cmd.phi_real_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiType:
      save(resolve(cmd.phi_type_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiAddr:
      save(resolve(cmd.phi_addr_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::PhiBlock:
      save(resolve(cmd.phi_block_.args_->map_.at(call_stack.top().prev_)));
      break;
    case IR::Op::Contextualize: NOT_YET();
    case IR::Op::CondJump:
      return cmd.cond_jump_.blocks_[resolve<bool>(cmd.cond_jump_.cond_)];
    case IR::Op::UncondJump: return cmd.uncond_jump_.block_;
    case IR::Op::ReturnJump: return IR::BlockIndex{-1};
    case IR::Op::BlockSeqJump: {
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
  return IR::BlockIndex{-2};
}
}  // namespace backend
