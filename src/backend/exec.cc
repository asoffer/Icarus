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
std::string_view SaveStringGlobally(std::string const &s);
}  // namespace IR

namespace backend {

void Execute(IR::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<IR::Addr> &ret_slots, IR::ExecContext *ctx) {
  if (fn->gened_fn_) { fn->gened_fn_->CompleteBody(fn->mod_); }
  ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an
  // error.

  auto arch     = Architecture::InterprettingMachine();
  size_t offset = 0;
  for (auto *t : fn->type_->output) {
    offset = arch.MoveForwardToAlignment(t, offset) + arch.bytes(t);
  }
  base::untyped_buffer ret_buffer(offset);

  while (true) {
    auto block_index = ctx->ExecuteBlock(ret_slots);
    if (block_index.is_default()) {
      ctx->call_stack.pop();
      return;
    } else {
      ctx->call_stack.top().MoveTo(block_index);
    }
  }
}
}  // namespace backend

// TODO namespace migration.
namespace IR {
template <typename T>
T ExecContext::resolve(Register val) const {
  return call_stack.top().regs_.get<T>(val.value);
}

extern Val MakeBlockSeq(const base::vector<Val> &blocks);

ExecContext::ExecContext() : stack_(50u) {}

BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(Func *fn, const base::untyped_buffer &arguments)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_)) {
  regs_.write(0, arguments);
}

BlockIndex ExecContext::ExecuteBlock(const base::vector<IR::Addr> &ret_slots) {
  BlockIndex result;
  ASSERT(current_block().cmds_.size() > 0u);
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter++, ret_slots);
  } while (result == BlockIndex{-2});
  return result;
}

IR::Addr Stack::Push(const type::Pointer *ptr) {
  Addr addr;
  addr.as_stack = buffer_.size();
  addr.kind     = Addr::Kind::Stack;
  buffer_.append_bytes(
      Architecture::InterprettingMachine().bytes(ptr->pointee));
  return addr;
}

template <typename T>
static T LoadValue(Addr addr, const Stack &stack) {
  switch (addr.kind) {
    case Addr::Kind::Null: UNREACHABLE();
    case Addr::Kind::Heap: return *static_cast<T *>(addr.as_heap); break;
    case Addr::Kind::Stack: return stack.Load<T>(addr.as_stack); break;
  }
  UNREACHABLE();
}

template <typename T>
static void StoreValue(T val, Addr addr, Stack *stack) {
  switch (addr.kind) {
    case Addr::Kind::Null:
      // TODO compile-time failure. dump the stack trace and abort.
      UNREACHABLE();
    case Addr::Kind::Stack: stack->Store(val, addr.as_stack); return;
    case Addr::Kind::Heap: *static_cast<T *>(addr.as_heap) = val;
  }
}

BlockIndex ExecContext::ExecuteCmd(const Cmd &cmd,
                                   const base::vector<IR::Addr> &ret_slots) {
  auto save = [&](auto val) {
    call_stack.top().regs_.set(cmd.result.value, val);
  };

  switch (cmd.op_code_) {
    case Op::Trunc:
      save(static_cast<char>(resolve<i32>(cmd.trunc_.reg_)));
      break;
    case Op::Extend:
      save(static_cast<i32>(resolve<char>(cmd.extend_.reg_)));
      break;
    case Op::Bytes:
      save(
          Architecture::InterprettingMachine().bytes(resolve(cmd.bytes_.arg_)));
      break;
    case Op::Align:
      save(Architecture::InterprettingMachine().alignment(
          resolve(cmd.align_.arg_)));
      break;
    case Op::Not: save(!resolve<bool>(cmd.not_.reg_)); break;
    case Op::NegInt: save(-resolve<i32>(cmd.neg_int_.reg_)); break;
    case Op::NegReal: save(-resolve<double>(cmd.neg_real_.reg_)); break;
    case Op::ArrayLength: save(resolve(cmd.array_data_.arg_)); break;
    case Op::ArrayData: {
      auto addr = resolve(cmd.array_data_.arg_);
      switch (addr.kind) {
        case Addr::Kind::Null: UNREACHABLE();
        case Addr::Kind::Stack:
          save(addr.as_stack +
               Architecture::InterprettingMachine().bytes(type::Int));
          break;
        case Addr::Kind::Heap:
          save(static_cast<void *>(
              static_cast<u8 *>(addr.as_heap) +
              Architecture::InterprettingMachine().bytes(type::Int)));
          break;
      }
    } break;
    case Op::LoadBool:
      save(LoadValue<bool>(resolve(cmd.load_bool_.arg_), stack_));
      break;
    case Op::LoadChar:
      save(LoadValue<char>(resolve(cmd.load_char_.arg_), stack_));
      break;
    case Op::LoadInt:
      save(LoadValue<i32>(resolve(cmd.load_int_.arg_), stack_));
      break;
    case Op::LoadReal:
      save(LoadValue<double>(resolve(cmd.load_real_.arg_), stack_));
      break;
    case Op::LoadType:
      save(LoadValue<type::Type const *>(resolve(cmd.load_type_.arg_), stack_));
      break;
    case Op::LoadEnum:
      save(LoadValue<size_t>(resolve(cmd.load_enum_.arg_), stack_));
      break;
    case Op::LoadFlags:
      save(LoadValue<size_t>(resolve(cmd.load_flags_.arg_), stack_));
      break;
    case Op::LoadAddr:
      save(LoadValue<Addr>(resolve(cmd.load_addr_.arg_), stack_));
      break;
    case Op::AddInt:
      save(resolve(cmd.add_int_.args_[0]) + resolve(cmd.add_int_.args_[1]));
      break;
    case Op::AddReal:
      save(resolve(cmd.add_real_.args_[0]) + resolve(cmd.add_real_.args_[1]));
      break;
    case Op::AddCharBuf:
      save(
          SaveStringGlobally(std::string(resolve(cmd.add_char_buf_.args_[0])) +
                             std::string(resolve(cmd.add_char_buf_.args_[1]))));
      break;
    case Op::SubInt:
      save(resolve(cmd.sub_int_.args_[0]) - resolve(cmd.sub_int_.args_[1]));
      break;
    case Op::SubReal:
      save(resolve(cmd.sub_real_.args_[0]) - resolve(cmd.sub_real_.args_[1]));
      break;
    case Op::MulInt:
      save(resolve(cmd.mul_int_.args_[0]) * resolve(cmd.mul_int_.args_[1]));
      break;
    case Op::MulReal:
      save(resolve(cmd.mul_real_.args_[0]) * resolve(cmd.mul_real_.args_[1]));
      break;
    case Op::DivInt:
      save(resolve(cmd.div_int_.args_[0]) / resolve(cmd.div_int_.args_[1]));
      break;
    case Op::DivReal:
      save(resolve(cmd.div_real_.args_[0]) / resolve(cmd.div_real_.args_[1]));
      break;
    case Op::ModInt:
      save(resolve(cmd.mod_int_.args_[0]) % resolve(cmd.mod_int_.args_[1]));
      break;
    case Op::ModReal:
      save(std::fmod(resolve(cmd.mod_real_.args_[0]),
                     resolve(cmd.mod_real_.args_[1])));
      break;
    case Op::LtInt:
      save(resolve(cmd.lt_int_.args_[0]) < resolve(cmd.lt_int_.args_[1]));
      break;
    case Op::LtReal:
      save(resolve(cmd.lt_real_.args_[0]) < resolve(cmd.lt_real_.args_[1]));
      break;
    case Op::LtFlags: {
      auto lhs = resolve(cmd.lt_flags_.args_[0]);
      auto rhs = resolve(cmd.lt_flags_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value));
    } break;
    case Op::LeInt:
      save(resolve(cmd.le_int_.args_[0]) <= resolve(cmd.le_int_.args_[1]));
      break;
    case Op::LeReal:
      save(resolve(cmd.le_real_.args_[0]) <= resolve(cmd.le_real_.args_[1]));
      break;
    case Op::LeFlags: {
      auto lhs = resolve(cmd.le_flags_.args_[0]);
      auto rhs = resolve(cmd.le_flags_.args_[1]);
      save((lhs.value | rhs.value) == rhs.value);
    } break;
    case Op::GtInt:
      save(resolve(cmd.gt_int_.args_[0]) > resolve(cmd.gt_int_.args_[1]));
      break;
    case Op::GtReal:
      save(resolve(cmd.gt_real_.args_[0]) > resolve(cmd.gt_real_.args_[1]));
      break;
    case Op::GtFlags: {
      auto lhs = resolve(cmd.gt_flags_.args_[0]);
      auto rhs = resolve(cmd.gt_flags_.args_[1]);
      save(lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value));
    } break;
    case Op::GeInt:
      save(resolve(cmd.ge_int_.args_[0]) >= resolve(cmd.ge_int_.args_[1]));
      break;
    case Op::GeReal:
      save(resolve(cmd.ge_real_.args_[0]) >= resolve(cmd.ge_real_.args_[1]));
      break;
    case Op::GeFlags: {
      auto lhs = resolve(cmd.ge_flags_.args_[0]);
      auto rhs = resolve(cmd.ge_flags_.args_[1]);
      save((lhs.value | rhs.value) == lhs.value);
    } break;
    case Op::EqBool:
      save(resolve(cmd.eq_bool_.args_[0]) == resolve(cmd.eq_bool_.args_[1]));
      break;
    case Op::EqChar:
      save(resolve(cmd.eq_char_.args_[0]) == resolve(cmd.eq_char_.args_[1]));
      break;
    case Op::EqInt:
      save(resolve(cmd.eq_int_.args_[0]) == resolve(cmd.eq_int_.args_[1]));
      break;
    case Op::EqReal:
      save(resolve(cmd.eq_real_.args_[0]) == resolve(cmd.eq_real_.args_[1]));
      break;
    case Op::EqFlags:
      save(resolve(cmd.eq_flags_.args_[0]) == resolve(cmd.eq_flags_.args_[1]));
      break;
    case Op::EqType:
      save(resolve(cmd.eq_type_.args_[0]) == resolve(cmd.eq_type_.args_[1]));
      break;
    case Op::EqAddr:
      save(resolve(cmd.eq_addr_.args_[0]) == resolve(cmd.eq_addr_.args_[1]));
      break;
    case Op::NeBool:
      save(resolve(cmd.ne_bool_.args_[0]) == resolve(cmd.ne_bool_.args_[1]));
      break;
    case Op::NeChar:
      save(resolve(cmd.ne_char_.args_[0]) == resolve(cmd.ne_char_.args_[1]));
      break;
    case Op::NeInt:
      save(resolve(cmd.ne_int_.args_[0]) == resolve(cmd.ne_int_.args_[1]));
      break;
    case Op::NeReal:
      save(resolve(cmd.ne_real_.args_[0]) == resolve(cmd.ne_real_.args_[1]));
      break;
    case Op::NeFlags:
      save(resolve(cmd.ne_flags_.args_[0]) == resolve(cmd.ne_flags_.args_[1]));
      break;
    case Op::NeType:
      save(resolve(cmd.ne_type_.args_[0]) == resolve(cmd.ne_type_.args_[1]));
      break;
    case Op::NeAddr:
      save(resolve(cmd.ne_addr_.args_[0]) == resolve(cmd.ne_addr_.args_[1]));
      break;
    case Op::XorBool:
      save(resolve(cmd.xor_bool_.args_[0]) ^ resolve(cmd.xor_bool_.args_[1]));
      break;
    case Op::XorFlags: NOT_YET();
    case Op::OrBool:
      save(resolve(cmd.or_bool_.args_[0]) | resolve(cmd.or_bool_.args_[1]));
      break;
    case Op::OrFlags: NOT_YET();
    case Op::AndBool:
      save(resolve(cmd.and_bool_.args_[0]) & resolve(cmd.and_bool_.args_[1]));
      break;
    case Op::AndFlags: NOT_YET();
    case Op::CreateStruct: save(new type::Struct); break;
    case Op::InsertField: {
      NOT_YET();
      // std::vector<Val> resolved_args;
      // resolved_args.reserve(ASSERT_NOT_NULL(cmd.insert_field_.args_)->size());
      // for (auto const &arg : *cmd.insert_field_.args_) {
      //   if (auto *r = std::get_if<Register>(&arg.value)) {
      //     resolved_args.push_back(reg(*r));
      //   } else {
      //     resolved_args.push_back(arg);
      //   }
      // }

      // auto *struct_to_mod = std::get<type::Struct
      // *>(resolved_args[0].value);
      // struct_to_mod->fields_.push_back(type::Struct::Field{
      //     std::string(std::get<std::string_view>(resolved_args[1].value)),
      //     std::get<const type::Type *>(resolved_args[2].value),
      //     resolved_args[3]});

      // auto[iter, success] = struct_to_mod->field_indices_.emplace(
      //     std::string(std::get<std::string_view>(resolved_args[1].value)),
      //     struct_to_mod->fields_.size() - 1);
      // ASSERT(success);
    } break;
    case Op::FinalizeStruct:
      save(resolve<type::Struct *>(cmd.finalize_struct_.reg_)->finalize());
      break;
    case Op::Malloc: save(malloc(resolve(cmd.malloc_.arg_))); break;
    case Op::Free: free(resolve<Addr>(cmd.free_.reg_).as_heap); break;
    case Op::Alloca: save(stack_.Push(&cmd.type->as<type::Pointer>())); break;
    case Op::Ptr:
      save(type::Ptr(resolve<type::Type const *>(cmd.ptr_.reg_)));
      break;
    case Op::Arrow:
      save(type::Func({resolve(cmd.arrow_.args_[0])},
                      {resolve(cmd.arrow_.args_[1])}));
      break;
    case Op::Array: {
      auto len = resolve(cmd.array_.len_);
      auto t   = resolve(cmd.array_.type_);
      save(len == -1 ? type::Arr(t) : type::Arr(t, len));
    } break;
    case Op::VariantType: save(resolve<Addr>(cmd.variant_type_.reg_)); break;
    case Op::VariantValue: {
      auto bytes = Architecture::InterprettingMachine().bytes(Ptr(type::Type_));
      auto bytes_fwd =
          Architecture::InterprettingMachine().MoveForwardToAlignment(
              Ptr(type::Type_), bytes);
      auto addr = resolve<Addr>(cmd.variant_value_.reg_);
      switch (addr.kind) {
        case Addr::Kind::Stack:
          addr.as_stack += bytes_fwd;
          save(addr);
          break;
        case Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                             bytes_fwd);
          save(addr);
          break;
        case Addr::Kind::Null: NOT_YET();
      }
    } break;
    case Op::PtrIncr: {
      auto addr = resolve<Addr>(cmd.ptr_incr_.ptr_);
      auto incr = resolve(cmd.ptr_incr_.incr_);
      // Sadly must convert to value and back even though it's guaranteed to
      // be constant folded
      auto bytes_fwd = Architecture::InterprettingMachine().ComputeArrayLength(
          incr, cmd.type->as<type::Pointer>().pointee);
      switch (addr.kind) {
        case Addr::Kind::Stack: save(addr.as_stack + bytes_fwd); break;
        case Addr::Kind::Heap:
          save(static_cast<char *>(addr.as_heap) + bytes_fwd);
          break;
        case Addr::Kind::Null: NOT_YET();
      }
    } break;
    case Op::Field: {
      auto addr = resolve<Addr>(cmd.field_.ptr_);
      auto *struct_type =
          resolve<type::Struct const *>(cmd.field_.struct_type_);
      size_t offset = 0;
      for (size_t i = 0; i < cmd.field_.num_; ++i) {
        auto field_type = struct_type->fields_.at(i).type;
        offset += Architecture::InterprettingMachine().bytes(field_type);
        offset = Architecture::InterprettingMachine().MoveForwardToAlignment(
            struct_type->fields_.at(i + 1).type, offset);
      }

      if (addr.kind == Addr::Kind::Stack) {
        save(addr.as_stack + offset);
      } else {
        save(static_cast<char *>(addr.as_heap) + offset);
      }
    } break;
    case Op::PrintBool:
      std::cerr << (resolve(cmd.print_bool_.arg_) ? "true" : "false");
      break;
    case Op::PrintChar: std::cerr << resolve(cmd.print_char_.arg_); break;
    case Op::PrintInt: std::cerr << resolve(cmd.print_int_.arg_); break;
    case Op::PrintReal: std::cerr << resolve(cmd.print_real_.arg_); break;
    case Op::PrintType:
      std::cerr << resolve(cmd.print_type_.arg_)->to_string();
      break;
    case Op::PrintEnum:
      NOT_YET();
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
      break;
    case Op::PrintAddr:
      std::cerr << resolve(cmd.print_addr_.arg_).to_string();
      break;
    case Op::PrintCharBuffer:
      std::cerr << resolve(cmd.print_char_buffer_.arg_);
      break;
    case Op::Call: {
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

        if (t == type::Bool) {
          call_buf.append(is_reg
                              ? resolve<bool>(long_args.get<Register>(offset))
                              : long_args.get<bool>(offset));
        } else if (t == type::Char) {
          call_buf.append(is_reg
                              ? resolve<char>(long_args.get<Register>(offset))
                              : long_args.get<char>(offset));
        } else if (t == type::Int) {
          call_buf.append(is_reg ? resolve<i32>(long_args.get<Register>(offset))
                                 : long_args.get<i32>(offset));
        } else if (t == type::Real) {
          call_buf.append(is_reg
                              ? resolve<double>(long_args.get<Register>(offset))
                              : long_args.get<double>(offset));
        } else if (t == type::Type_) {
          call_buf.append(is_reg ? resolve<type::Type const *>(
                                       long_args.get<Register>(offset))
                                 : long_args.get<type::Type const *>(offset));
        } else if (t->is<type::CharBuffer>()) {
          call_buf.append(is_reg ? resolve<std::string_view>(
                                       long_args.get<Register>(offset))
                                 : long_args.get<std::string_view>(offset));
        } else if (t->is<type::Function>()) {
          call_buf.append(
              is_reg ? resolve<IR::Func *>(long_args.get<Register>(offset))
                     : long_args.get<IR::Func *>(offset));
        } else if (t->is<type::Scope>()) {
          call_buf.append(is_reg ? resolve<AST::ScopeLiteral *>(
                                       long_args.get<Register>(offset))
                                 : long_args.get<AST::ScopeLiteral *>(offset));
        } else if (t == type::Module) {
          call_buf.append(
              is_reg ? resolve<Module const *>(long_args.get<Register>(offset))
                     : long_args.get<Module const *>(offset));
        } else if (t == type::Generic) {
          // TODO mostly wrong.
          call_buf.append(
              is_reg ? resolve<AST::Function *>(long_args.get<Register>(offset))
                     : long_args.get<AST::Function *>(offset));
        } else if (t == type::Block || t == type::OptBlock) {
          call_buf.append(is_reg ? resolve<IR::BlockSequence>(
                                       long_args.get<Register>(offset))
                                 : long_args.get<IR::BlockSequence>(offset));
        } else if (t->is<type::Variant>()) {
          call_buf.append(
              is_reg ? resolve<IR::Addr>(long_args.get<Register>(offset))
                     : long_args.get<IR::Addr>(offset));
        } else {
          NOT_YET(t->to_string());
        }

        offset += is_reg ? sizeof(Register) : arch.bytes(t);
      }

      // TODO you need to be able to determine how many args there are
      switch (cmd.call_.which_active_) {
        case 0x00: {
          // TODO what if the register is a foerign fn?
          backend::Execute(resolve<IR::Func *>(cmd.call_.reg_), call_buf,
                           ret_slots, this);
        } break;
        case 0x01: {
          backend::Execute(cmd.call_.fn_, call_buf, ret_slots, this);

        } break;
        case 0x02:
          if (cmd.call_.foreign_fn_.name_ == "malloc") {
            IR::Addr addr;
            addr.kind    = Addr::Kind::Heap;
            addr.as_heap = malloc(call_buf.get<i32>(0));
            save(addr);
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
      save(type::Tup(std::move(types)));
    } break;
    case Op::Variant: {
      base::vector<const type::Type *> types;
      types.reserve(cmd.variant_.args_->size());
      for (const auto &val : *cmd.variant_.args_) {
        types.push_back(std::get<const type::Type *>(val.value));
      }
      save(type::Var(std::move(types)));
    } break;
    case Op::CastIntToReal:
      save(static_cast<double>(resolve<i32>(cmd.cast_int_to_real_.reg_)));
      break;
    case Op::CastPtr: save(resolve<IR::Addr>(cmd.cast_ptr_.reg_)); break;
    case Op::AddCodeBlock: NOT_YET();
    case Op::BlockSeq: {
      std::vector<Val> resolved_args;
      resolved_args.reserve(ASSERT_NOT_NULL(cmd.block_seq_.args_)->size());
      for (auto const &arg : *cmd.block_seq_.args_) {
        auto *r = std::get_if<Register>(&arg.value);
        resolved_args.push_back(
            (r == nullptr) ? arg
                           : IR::Val::BlockSeq(resolve<BlockSequence>(*r)));
      }
      save(std::get<BlockSequence>(MakeBlockSeq(resolved_args).value));
    } break;
    case Op::BlockSeqContains: {
      auto *seq = resolve<BlockSequence>(cmd.block_seq_contains_.reg_).seq_;
      save(std::any_of(seq->begin(), seq->end(), [&](AST::BlockLiteral *lit) {
        return lit == cmd.block_seq_contains_.lit_;
      }));
    } break;
    case Op::SetReturnBool:
      StoreValue(resolve(cmd.set_return_bool_.val_),
                 ret_slots.at(cmd.set_return_bool_.ret_num_), &stack_);
      break;
    case Op::SetReturnChar:
      StoreValue(resolve(cmd.set_return_char_.val_),
                 ret_slots.at(cmd.set_return_char_.ret_num_), &stack_);
      break;
    case Op::SetReturnInt:
      StoreValue(resolve(cmd.set_return_int_.val_),
                 ret_slots.at(cmd.set_return_int_.ret_num_), &stack_);
      break;
    case Op::SetReturnReal:
      StoreValue(resolve(cmd.set_return_real_.val_),
                 ret_slots.at(cmd.set_return_real_.ret_num_), &stack_);
      break;
    case Op::SetReturnType:
      StoreValue(resolve(cmd.set_return_type_.val_),
                 ret_slots.at(cmd.set_return_type_.ret_num_), &stack_);
      break;
    case Op::SetReturnCharBuf:
      StoreValue(resolve(cmd.set_return_char_buf_.val_),
                 ret_slots.at(cmd.set_return_char_buf_.ret_num_), &stack_);
      break;
    case Op::SetReturnAddr:
      StoreValue(resolve(cmd.set_return_addr_.val_),
                 ret_slots.at(cmd.set_return_addr_.ret_num_), &stack_);
      break;
    case Op::SetReturnEnum:
      StoreValue(resolve(cmd.set_return_enum_.val_),
                 ret_slots.at(cmd.set_return_enum_.ret_num_), &stack_);
      break;
    case Op::SetReturnFlags:
      StoreValue(resolve(cmd.set_return_flags_.val_),
                 ret_slots.at(cmd.set_return_flags_.ret_num_), &stack_);
      break;
    case Op::SetReturnFunc:
      StoreValue(resolve(cmd.set_return_func_.val_),
                 ret_slots.at(cmd.set_return_func_.ret_num_), &stack_);
      break;
    case Op::SetReturnScope:
      StoreValue(resolve(cmd.set_return_scope_.val_),
                 ret_slots.at(cmd.set_return_scope_.ret_num_), &stack_);
      break;
    case Op::SetReturnModule:
      StoreValue(resolve(cmd.set_return_module_.val_),
                 ret_slots.at(cmd.set_return_module_.ret_num_), &stack_);
      break;
    case Op::SetReturnBlock:
      StoreValue(resolve(cmd.set_return_block_.val_),
                 ret_slots.at(cmd.set_return_block_.ret_num_), &stack_);
      break;
    case Op::SetReturnGeneric:
      StoreValue(resolve(cmd.set_return_generic_.val_),
                 ret_slots.at(cmd.set_return_generic_.ret_num_), &stack_);
      break;
    case Op::StoreBool:
      StoreValue(resolve(cmd.store_bool_.val_),
                 resolve<IR::Addr>(cmd.store_bool_.addr_), &stack_);
      break;
    case Op::StoreChar:
      StoreValue(resolve(cmd.store_char_.val_),
                 resolve<IR::Addr>(cmd.store_char_.addr_), &stack_);
      break;
    case Op::StoreInt:
      StoreValue(resolve(cmd.store_int_.val_),
                 resolve<IR::Addr>(cmd.store_int_.addr_), &stack_);
      break;
    case Op::StoreReal:
      StoreValue(resolve(cmd.store_real_.val_),
                 resolve<IR::Addr>(cmd.store_real_.addr_), &stack_);
      break;
    case Op::StoreType:
      StoreValue(resolve(cmd.store_type_.val_),
                 resolve<IR::Addr>(cmd.store_type_.addr_), &stack_);
      break;
    case Op::StoreEnum:
      StoreValue(resolve(cmd.store_enum_.val_),
                 resolve<IR::Addr>(cmd.store_enum_.addr_), &stack_);
      break;
    case Op::StoreFlags:
      StoreValue(resolve(cmd.store_flags_.val_),
                 resolve<IR::Addr>(cmd.store_flags_.addr_), &stack_);
      break;
    case Op::StoreAddr:
      StoreValue(resolve(cmd.store_addr_.val_),
                 resolve<IR::Addr>(cmd.store_addr_.addr_), &stack_);
      break;
    case Op::PhiBlock: {
      size_t i         = 0;
      auto const *args = cmd.phi_block_.args_;
      for (; i < args->blocks_.size(); ++i) {
        if (call_stack.top().prev_ == args->blocks_[i]) { break; }
      }
      ASSERT(i < args->blocks_.size());
      save(resolve(args->vals_[i]));
    } break;
    case Op::PhiBool: {
      size_t i         = 0;
      auto const *args = cmd.phi_bool_.args_;
      for (; i < args->blocks_.size(); ++i) {
        if (call_stack.top().prev_ == args->blocks_[i]) { break; }
      }
      ASSERT(i < args->blocks_.size());
      save(resolve(args->vals_[i]));
    } break;
    case Op::Phi: {
      std::vector<Val> resolved_args;
      resolved_args.reserve(ASSERT_NOT_NULL(cmd.phi_.args_)->size());
      for (auto const &arg : *cmd.phi_.args_) {
        if (auto *r = std::get_if<Register>(&arg.value)) {
          if (cmd.type == type::Bool) {
            resolved_args.push_back(IR::Val::Bool(resolve<bool>(*r)));
          } else if (cmd.type == type::Char) {
            resolved_args.push_back(IR::Val::Char(resolve<char>(*r)));
          } else if (cmd.type == type::Int) {
            resolved_args.push_back(IR::Val::Int(resolve<i32>(*r)));
          } else if (cmd.type == type::Real) {
            resolved_args.push_back(IR::Val::Real(resolve<double>(*r)));
          } else if (cmd.type->is<type::Pointer>()) {
            resolved_args.push_back(IR::Val::Addr(
                resolve<IR::Addr>(*r), cmd.type->as<type::Pointer>().pointee));
          } else {
            NOT_YET(cmd.type->to_string());
          }
        } else {
          resolved_args.push_back(arg);
        }
      }

      for (size_t i = 0; i < resolved_args.size(); i += 2) {
        if (call_stack.top().prev_ ==
            std::get<IR::BlockIndex>(resolved_args[i].value)) {
          if (cmd.type == type::Bool) {
            save(std::get<bool>(resolved_args[i + 1].value));
          } else if (cmd.type == type::Char) {
            save(std::get<char>(resolved_args[i + 1].value));
          } else if (cmd.type == type::Int) {
            save(std::get<i32>(resolved_args[i + 1].value));
          } else if (cmd.type == type::Real) {
            save(std::get<double>(resolved_args[i + 1].value));
          } else if (cmd.type == type::Type_) {
            save(std::get<type::Type const *>(resolved_args[i + 1].value));
          } else if (cmd.type->is<type::Pointer>()) {
            save(std::get<IR::Addr>(resolved_args[i + 1].value));
          } else if (cmd.type == type::Block || cmd.type == type::OptBlock) {
            save(std::get<BlockSequence>(resolved_args[i + 1].value));
          } else {
            NOT_YET(cmd.type->to_string());
          }
          return BlockIndex{-2};
        }
      }
      call_stack.top().fn_->dump();
      UNREACHABLE("Previous block was ", call_stack.top().prev_,
                  "\nCurrent block is ", call_stack.top().current_);
    } break;
    case Op::Contextualize: NOT_YET();
    case Op::CondJump:
      return cmd.cond_jump_.blocks_[resolve<bool>(cmd.cond_jump_.cond_)];
    case Op::UncondJump: return cmd.uncond_jump_.block_;
    case Op::ReturnJump: return BlockIndex{-1};
  }
  return BlockIndex{-2};
}
}  // namespace IR

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
  backend::Execute(fn.get(), base::untyped_buffer(0), {}, &ctx);
}
