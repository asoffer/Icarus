#include "backend/exec.h"

#include <dlfcn.h>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstring>
#include <future>
#include <iostream>
#include <memory>
#include <random>
#include <thread>

#include "architecture.h"
#include "ast/block_literal.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/scope_node.h"
#include "base/util.h"
#include "error/log.h"
#include "ir/arguments.h"
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
  UNREACHABLE(DUMP(static_cast<int>(addr.kind)));
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

void CallForeignFn(ir::ForeignFn const &f,
                   base::untyped_buffer const &arguments, base::vector<ir::Addr> ret_slots,
                   base::untyped_buffer *stack) {
  // TODO handle failures gracefully.
  // TODO Consider caching these.
  // TODO Handle a bunch more function types in a coherent way.
  if (f.type() == type::Func({type::Int32}, {type::Int32})) {
    using fn_t = i32 (*)(i32);
    fn_t fn    = (fn_t)(ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, f.name().data())));
    StoreValue(fn(arguments.get<i32>(0)), ret_slots.at(0), stack);
  } else if (f.type() == type::Func({type::Int32}, {})) {
    using fn_t = void (*)(i32);
    fn_t fn    = (fn_t)(ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, f.name().data())));
    fn(arguments.get<i32>(0));
  } else if (f.type() == type::Func({type::Float64}, {type::Float64})) {
    using fn_t = double (*)(double);
    fn_t fn    = (fn_t)(ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, f.name().data())));
    StoreValue(fn(arguments.get<double>(0)), ret_slots.at(0), stack);
  } else if (f.type() == type::Func({type::Float32}, {type::Float32})) {
    using fn_t = float (*)(float);
    fn_t fn    = (fn_t)(ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, f.name().data())));
    StoreValue(fn(arguments.get<float>(0)), ret_slots.at(0), stack);
  }

  /*
  if (ff.name() == "malloc") {
    ir::Addr addr;
    addr.kind    = ir::Addr::Kind::Heap;
    addr.as_heap = malloc(call_buf.get<i32>(0));
    StoreValue(addr, ret_slot, stack);
  }
  */
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
    case ir::Op::NotBool: save(!resolve<bool>(cmd.reg_)); break;
    case ir::Op::NotFlags: {
      save(ir::NotFlags(resolve<ir::FlagsVal>(cmd.typed_reg_.get()),
                        &cmd.typed_reg_.type()->as<type::Flags>()));
    } break;
    case ir::Op::NegInt8: save(-resolve<i8>(cmd.reg_)); break;
    case ir::Op::NegInt16: save(-resolve<i16>(cmd.reg_)); break;
    case ir::Op::NegInt32: save(-resolve<i32>(cmd.reg_)); break;
    case ir::Op::NegInt64: save(-resolve<i64>(cmd.reg_)); break;
    case ir::Op::NegFloat32: save(-resolve<double>(cmd.reg_)); break;
    case ir::Op::NegFloat64: save(-resolve<double>(cmd.reg_)); break;
    case ir::Op::ArrayLength: save(resolve<ir::Addr>(cmd.reg_)); break;
    case ir::Op::ArrayData: {
      auto addr = resolve<ir::Addr>(cmd.reg_);
      switch (addr.kind) {
        case ir::Addr::Kind::Null: UNREACHABLE();
        case ir::Addr::Kind::Stack:
          addr.as_stack +=
              Architecture::InterprettingMachine().bytes(type::Int64);
          break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(
              static_cast<u8 *>(addr.as_heap) +
              Architecture::InterprettingMachine().bytes(type::Int64));
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
    case ir::Op::LoadInt8:
      save(LoadValue<i8>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadInt16:
      save(LoadValue<i16>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadInt32:
      save(LoadValue<i32>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadInt64:
      save(LoadValue<i64>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadNat8:
      save(LoadValue<u8>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadNat16:
      save(LoadValue<u16>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadNat32:
      save(LoadValue<u32>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
    case ir::Op::LoadNat64:
      save(LoadValue<u64>(resolve<ir::Addr>(cmd.reg_), stack_));
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
      save(LoadValue<ir::AnyFunc>(resolve<ir::Addr>(cmd.reg_), stack_));
      break;
#define CASE(op, member, fn)                                                   \
  case op: {                                                                   \
    save(fn(resolve(cmd.member.args_[0]), resolve(cmd.member.args_[1])));      \
  } break
      CASE(ir::Op::AddInt8, i8_args_, std::plus<i8>{});
      CASE(ir::Op::AddInt16, i16_args_, std::plus<i16>{});
      CASE(ir::Op::AddInt32, i32_args_, std::plus<i32>{});
      CASE(ir::Op::AddInt64, i64_args_, std::plus<i64>{});
      CASE(ir::Op::AddNat8, u8_args_, std::plus<u8>{});
      CASE(ir::Op::AddNat16, u16_args_, std::plus<u16>{});
      CASE(ir::Op::AddNat32, u32_args_, std::plus<u32>{});
      CASE(ir::Op::AddNat64, u64_args_, std::plus<u64>{});
      CASE(ir::Op::AddFloat32, float32_args_, std::plus<float>{});
      CASE(ir::Op::AddFloat64, float64_args_, std::plus<double>{});

      CASE(ir::Op::SubInt8, i8_args_, std::minus<i8>{});
      CASE(ir::Op::SubInt16, i16_args_, std::minus<i16>{});
      CASE(ir::Op::SubInt32, i32_args_, std::minus<i32>{});
      CASE(ir::Op::SubInt64, i64_args_, std::minus<i64>{});
      CASE(ir::Op::SubNat8, u8_args_, std::minus<u8>{});
      CASE(ir::Op::SubNat16, u16_args_, std::minus<u16>{});
      CASE(ir::Op::SubNat32, u32_args_, std::minus<u32>{});
      CASE(ir::Op::SubNat64, u64_args_, std::minus<u64>{});
      CASE(ir::Op::SubFloat32, float32_args_, std::minus<float>{});
      CASE(ir::Op::SubFloat64, float64_args_, std::minus<double>{});

      CASE(ir::Op::MulInt8, i8_args_, std::multiplies<i8>{});
      CASE(ir::Op::MulInt16, i16_args_, std::multiplies<i16>{});
      CASE(ir::Op::MulInt32, i32_args_, std::multiplies<i32>{});
      CASE(ir::Op::MulInt64, i64_args_, std::multiplies<i64>{});
      CASE(ir::Op::MulNat8, u8_args_, std::multiplies<u8>{});
      CASE(ir::Op::MulNat16, u16_args_, std::multiplies<u16>{});
      CASE(ir::Op::MulNat32, u32_args_, std::multiplies<u32>{});
      CASE(ir::Op::MulNat64, u64_args_, std::multiplies<u64>{});
      CASE(ir::Op::MulFloat32, float32_args_, std::multiplies<float>{});
      CASE(ir::Op::MulFloat64, float64_args_, std::multiplies<double>{});

      CASE(ir::Op::DivInt8, i8_args_, std::divides<i8>{});
      CASE(ir::Op::DivInt16, i16_args_, std::divides<i16>{});
      CASE(ir::Op::DivInt32, i32_args_, std::divides<i32>{});
      CASE(ir::Op::DivInt64, i64_args_, std::divides<i64>{});
      CASE(ir::Op::DivNat8, u8_args_, std::divides<u8>{});
      CASE(ir::Op::DivNat16, u16_args_, std::divides<u16>{});
      CASE(ir::Op::DivNat32, u32_args_, std::divides<u32>{});
      CASE(ir::Op::DivNat64, u64_args_, std::divides<u64>{});
      CASE(ir::Op::DivFloat32, float32_args_, std::divides<float>{});
      CASE(ir::Op::DivFloat64, float64_args_, std::divides<double>{});

      CASE(ir::Op::ModInt8, i8_args_, std::modulus<i8>{});
      CASE(ir::Op::ModInt16, i16_args_, std::modulus<i16>{});
      CASE(ir::Op::ModInt32, i32_args_, std::modulus<i32>{});
      CASE(ir::Op::ModInt64, i64_args_, std::modulus<i64>{});
      CASE(ir::Op::ModNat8, u8_args_, std::modulus<u8>{});
      CASE(ir::Op::ModNat16, u16_args_, std::modulus<u16>{});
      CASE(ir::Op::ModNat32, u32_args_, std::modulus<u32>{});
      CASE(ir::Op::ModNat64, u64_args_, std::modulus<u64>{});

      CASE(ir::Op::LtInt8, i8_args_, std::less<i8>{});
      CASE(ir::Op::LtInt16, i16_args_, std::less<i16>{});
      CASE(ir::Op::LtInt32, i32_args_, std::less<i32>{});
      CASE(ir::Op::LtInt64, i64_args_, std::less<i64>{});
      CASE(ir::Op::LtNat8, u8_args_, std::less<u8>{});
      CASE(ir::Op::LtNat16, u16_args_, std::less<u16>{});
      CASE(ir::Op::LtNat32, u32_args_, std::less<u32>{});
      CASE(ir::Op::LtNat64, u64_args_, std::less<u64>{});
      CASE(ir::Op::LtFloat32, float32_args_, std::less<float>{});
      CASE(ir::Op::LtFloat64, float64_args_, std::less<double>{});
      CASE(ir::Op::LtFlags, flags_args_, std::less<ir::FlagsVal>{});

       CASE(ir::Op::LeInt8, i8_args_, std::less_equal<i8>{});
      CASE(ir::Op::LeInt16, i16_args_, std::less_equal<i16>{});
      CASE(ir::Op::LeInt32, i32_args_, std::less_equal<i32>{});
      CASE(ir::Op::LeInt64, i64_args_, std::less_equal<i64>{});
     CASE(ir::Op::LeNat8, u8_args_, std::less_equal<u8>{});
      CASE(ir::Op::LeNat16, u16_args_, std::less_equal<u16>{});
      CASE(ir::Op::LeNat32, u32_args_, std::less_equal<u32>{});
      CASE(ir::Op::LeNat64, u64_args_, std::less_equal<u64>{});
      CASE(ir::Op::LeFloat32, float32_args_, std::less_equal<float>{});
      CASE(ir::Op::LeFloat64, float64_args_, std::less_equal<double>{});
      CASE(ir::Op::LeFlags, flags_args_, std::less_equal<ir::FlagsVal>{});

       CASE(ir::Op::GtInt8, i8_args_, std::greater<i8>{});
      CASE(ir::Op::GtInt16, i16_args_, std::greater<i16>{});
      CASE(ir::Op::GtInt32, i32_args_, std::greater<i32>{});
      CASE(ir::Op::GtInt64, i64_args_, std::greater<i64>{});
     CASE(ir::Op::GtNat8, u8_args_, std::greater<u8>{});
      CASE(ir::Op::GtNat16, u16_args_, std::greater<u16>{});
      CASE(ir::Op::GtNat32, u32_args_, std::greater<u32>{});
      CASE(ir::Op::GtNat64, u64_args_, std::greater<u64>{});
      CASE(ir::Op::GtFloat32, float32_args_, std::greater<float>{});
      CASE(ir::Op::GtFloat64, float64_args_, std::greater<double>{});
      CASE(ir::Op::GtFlags, flags_args_, std::greater<ir::FlagsVal>{});

       CASE(ir::Op::GeInt8, i8_args_, std::greater_equal<i8>{});
      CASE(ir::Op::GeInt16, i16_args_, std::greater_equal<i16>{});
      CASE(ir::Op::GeInt32, i32_args_, std::greater_equal<i32>{});
      CASE(ir::Op::GeInt64, i64_args_, std::greater_equal<i64>{});
     CASE(ir::Op::GeNat8, u8_args_, std::greater_equal<u8>{});
      CASE(ir::Op::GeNat16, u16_args_, std::greater_equal<u16>{});
      CASE(ir::Op::GeNat32, u32_args_, std::greater_equal<u32>{});
      CASE(ir::Op::GeNat64, u64_args_, std::greater_equal<u64>{});
      CASE(ir::Op::GeFloat32, float32_args_, std::greater_equal<float>{});
      CASE(ir::Op::GeFloat64, float64_args_, std::greater_equal<double>{});
      CASE(ir::Op::GeFlags, flags_args_, std::greater_equal<ir::FlagsVal>{});

      CASE(ir::Op::EqBool, bool_args_, std::equal_to<bool>{});
      CASE(ir::Op::EqChar, char_args_, std::equal_to<char>{});
      CASE(ir::Op::EqInt8, i8_args_, std::equal_to<i8>{});
      CASE(ir::Op::EqInt16, i16_args_, std::equal_to<i16>{});
      CASE(ir::Op::EqInt32, i32_args_, std::equal_to<i32>{});
      CASE(ir::Op::EqInt64, i64_args_, std::equal_to<i64>{});
      CASE(ir::Op::EqNat8, u8_args_, std::equal_to<u8>{});
      CASE(ir::Op::EqNat16, u16_args_, std::equal_to<u16>{});
      CASE(ir::Op::EqNat32, u32_args_, std::equal_to<u32>{});
      CASE(ir::Op::EqNat64, u64_args_, std::equal_to<u64>{});
      CASE(ir::Op::EqFloat32, float32_args_, std::equal_to<float>{});
      CASE(ir::Op::EqFloat64, float64_args_, std::equal_to<double>{});
      CASE(ir::Op::EqEnum, enum_args_, std::equal_to<ir::EnumVal>{});
      CASE(ir::Op::EqFlags, flags_args_, std::equal_to<ir::FlagsVal>{});
      CASE(ir::Op::EqType, type_args_, std::equal_to<type::Type const *>{});
      CASE(ir::Op::EqAddr, addr_args_, std::equal_to<ir::Addr>{});

      CASE(ir::Op::XorBool, bool_args_, std::not_equal_to<bool>{});
      CASE(ir::Op::NeChar, char_args_, std::not_equal_to<char>{});
      CASE(ir::Op::NeInt8, i8_args_, std::not_equal_to<i8>{});
      CASE(ir::Op::NeInt16, i16_args_, std::not_equal_to<i16>{});
      CASE(ir::Op::NeInt32, i32_args_, std::not_equal_to<i32>{});
      CASE(ir::Op::NeInt64, i64_args_, std::not_equal_to<i64>{});
      CASE(ir::Op::NeNat8, u8_args_, std::not_equal_to<u8>{});
      CASE(ir::Op::NeNat16, u16_args_, std::not_equal_to<u16>{});
      CASE(ir::Op::NeNat32, u32_args_, std::not_equal_to<u32>{});
      CASE(ir::Op::NeNat64, u64_args_, std::not_equal_to<u64>{});
      CASE(ir::Op::NeFloat32, float32_args_, std::not_equal_to<float>{});
      CASE(ir::Op::NeFloat64, float64_args_, std::not_equal_to<double>{});
      CASE(ir::Op::NeEnum, enum_args_, std::not_equal_to<ir::EnumVal>{});
      CASE(ir::Op::NeFlags, flags_args_, std::not_equal_to<ir::FlagsVal>{});
      CASE(ir::Op::NeType, type_args_, std::not_equal_to<type::Type const *>{});
      CASE(ir::Op::NeAddr, addr_args_, std::not_equal_to<ir::Addr>{});
#undef CASE

    case ir::Op::XorFlags:
      save(resolve(cmd.flags_args_.args_[0]) ^ resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::OrFlags:
      save(resolve(cmd.flags_args_.args_[0]) | resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::AndFlags:
      save(resolve(cmd.flags_args_.args_[0]) & resolve(cmd.flags_args_.args_[1]));
      break;
    case ir::Op::CreateStruct: save(type::Struct::Make(cmd.struct_lit_)); break;
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
        case ir::Addr::Kind::Null: UNREACHABLE();
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
        case ir::Addr::Kind::Null: UNREACHABLE();
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
    case ir::Op::PrintInt8: std::cerr << resolve(cmd.i8_arg_); break;
    case ir::Op::PrintInt16: std::cerr << resolve(cmd.i16_arg_); break;
    case ir::Op::PrintInt32: std::cerr << resolve(cmd.i32_arg_); break;
    case ir::Op::PrintInt64: std::cerr << resolve(cmd.i64_arg_); break;
    case ir::Op::PrintNat8: std::cerr << resolve(cmd.u8_arg_); break;
    case ir::Op::PrintNat16: std::cerr << resolve(cmd.u16_arg_); break;
    case ir::Op::PrintNat32: std::cerr << resolve(cmd.u32_arg_); break;
    case ir::Op::PrintNat64: std::cerr << resolve(cmd.u64_arg_); break;
    case ir::Op::PrintFloat32: std::cerr << resolve(cmd.float32_arg_); break;
    case ir::Op::PrintFloat64: std::cerr << resolve(cmd.float64_arg_); break;
    case ir::Op::PrintType:
      std::cerr << resolve(cmd.type_arg_)->to_string();
      break;
    case ir::Op::PrintEnum: {
      std::cerr << cmd.print_enum_.enum_type_->members_.at(
          resolve(cmd.print_enum_.arg_).value);
    } break;
    case ir::Op::PrintFlags: {
      size_t val = resolve(cmd.print_flags_.arg_).value;
      base::vector<std::string_view> vals;

      auto const &members = cmd.print_flags_.flags_type_->members_;
      size_t i            = 0;
      size_t pow          = 1;
      while (pow <= val) {
        if (val & pow) { vals.emplace_back(members[i]); }
        ++i;
        pow <<= 1;
      }
      if (vals.empty()) {
        std::cerr << "(empty)";
      } else {
        static auto seed = std::random_device{}();
        std::mt19937 gen(seed);
        std::shuffle(vals.begin(), vals.end(), gen);

        auto iter = vals.begin();
        std::cerr << *iter++;
        while (iter != vals.end()) { std::cerr << " | " << *iter++; }
      }
    } break;
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
      base::vector<ir::Addr> return_slots;
      if (cmd.call_.outs_ != nullptr) {
        return_slots.reserve(cmd.call_.outs_->size());
        for (size_t i = 0; i < cmd.call_.outs_->size(); ++i) {
          if (cmd.call_.outs_->is_loc_[i]) {
            return_slots.push_back(resolve<ir::Addr>(cmd.call_.outs_->regs_[i]));
          } else {
            ir::Addr addr;
            addr.kind    = ir::Addr::Kind::Heap;
            addr.as_heap =
                call_stack.top().regs_.raw(cmd.call_.outs_->regs_[i].value);
            return_slots.push_back(addr);
          }
        }
      }

      auto call_buf =
          cmd.call_.arguments_->PrepareCallBuffer(call_stack.top().regs_);
      ir::AnyFunc f = resolve(cmd.call_.fn_);

      // TODO you need to be able to determine how many args there are
      if (f.is_fn()) {
        backend::Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
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
    case ir::Op::CastToInt16: {
      if (cmd.typed_reg_.type() == type::Int8) {
        save(static_cast<u16>(resolve<i8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<u16>(resolve<u8>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToNat16: {
      if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<u16>(resolve<u8>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToInt32: {
      if (cmd.typed_reg_.type() == type::Int8) {
        save(static_cast<i32>(resolve<i8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<i32>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int16) {
        save(static_cast<i32>(resolve<i16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<i32>(resolve<u16>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToNat32: {
      if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<u32>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<u32>(resolve<u16>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToInt64: {
      if (cmd.typed_reg_.type() == type::Int8) {
        save(static_cast<i64>(resolve<i8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<i64>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int16) {
        save(static_cast<i64>(resolve<i16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<i64>(resolve<u16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int32) {
        save(static_cast<i64>(resolve<i32>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat32) {
        save(static_cast<i64>(resolve<i32>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToNat64: {
      if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<u64>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<u64>(resolve<u16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat32) {
        save(static_cast<u64>(resolve<i32>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToFloat32: {
      if (cmd.typed_reg_.type() == type::Int8) {
        save(static_cast<float>(resolve<i8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<float>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int16) {
        save(static_cast<float>(resolve<i16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<float>(resolve<u16>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
    case ir::Op::CastToFloat64: {
       if (cmd.typed_reg_.type() == type::Int8) {
        save(static_cast<double>(resolve<i8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat8) {
        save(static_cast<double>(resolve<u8>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int16) {
        save(static_cast<double>(resolve<i16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat16) {
        save(static_cast<double>(resolve<u16>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Int32) {
        save(static_cast<double>(resolve<i32>(cmd.typed_reg_.get())));
      } else if (cmd.typed_reg_.type() == type::Nat32) {
        save(static_cast<double>(resolve<u32>(cmd.typed_reg_.get())));
      } else {
        UNREACHABLE();
      }
    } break;
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
    case ir::Op::GetRet: save(ret_slots.at(cmd.get_ret_)); break;
    case ir::Op::SetRetBool:
      StoreValue(resolve(cmd.set_ret_bool_.val_),
                 ret_slots.at(cmd.set_ret_bool_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetChar:
      StoreValue(resolve(cmd.set_ret_char_.val_),
                 ret_slots.at(cmd.set_ret_char_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt8:
      StoreValue(resolve(cmd.set_ret_i8_.val_),
                 ret_slots.at(cmd.set_ret_i8_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt16:
      StoreValue(resolve(cmd.set_ret_i16_.val_),
                 ret_slots.at(cmd.set_ret_i16_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt32:
      StoreValue(resolve(cmd.set_ret_i32_.val_),
                 ret_slots.at(cmd.set_ret_i32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt64:
      StoreValue(resolve(cmd.set_ret_i64_.val_),
                 ret_slots.at(cmd.set_ret_i64_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat8:
      StoreValue(resolve(cmd.set_ret_u8_.val_),
                 ret_slots.at(cmd.set_ret_u8_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat16:
      StoreValue(resolve(cmd.set_ret_u16_.val_),
                 ret_slots.at(cmd.set_ret_u16_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat32:
      StoreValue(resolve(cmd.set_ret_u32_.val_),
                 ret_slots.at(cmd.set_ret_u32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat64:
      StoreValue(resolve(cmd.set_ret_u64_.val_),
                 ret_slots.at(cmd.set_ret_u64_.ret_num_), &stack_);
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
    case ir::Op::StoreInt8:
      StoreValue(resolve(cmd.store_i8_.val_),
                 resolve<ir::Addr>(cmd.store_i8_.addr_), &stack_);
      break;
    case ir::Op::StoreInt16:
      StoreValue(resolve(cmd.store_i16_.val_),
                 resolve<ir::Addr>(cmd.store_i16_.addr_), &stack_);
      break;
    case ir::Op::StoreInt32:
      StoreValue(resolve(cmd.store_i32_.val_),
                 resolve<ir::Addr>(cmd.store_i32_.addr_), &stack_);
      break;
    case ir::Op::StoreInt64:
      StoreValue(resolve(cmd.store_i64_.val_),
                 resolve<ir::Addr>(cmd.store_i64_.addr_), &stack_);
      break;
    case ir::Op::StoreNat8:
      StoreValue(resolve(cmd.store_u8_.val_),
                 resolve<ir::Addr>(cmd.store_u8_.addr_), &stack_);
      break;
    case ir::Op::StoreNat16:
      StoreValue(resolve(cmd.store_u16_.val_),
                 resolve<ir::Addr>(cmd.store_u16_.addr_), &stack_);
      break;
    case ir::Op::StoreNat32:
      StoreValue(resolve(cmd.store_u32_.val_),
                 resolve<ir::Addr>(cmd.store_u32_.addr_), &stack_);
      break;
    case ir::Op::StoreNat64:
      StoreValue(resolve(cmd.store_u64_.val_),
                 resolve<ir::Addr>(cmd.store_u64_.addr_), &stack_);
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
    case ir::Op::PhiInt8:
      save(resolve(cmd.phi_i8_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt16:
      save(resolve(cmd.phi_i16_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt32:
      save(resolve(cmd.phi_i32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt64:
      save(resolve(cmd.phi_i64_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat8:
      save(resolve(cmd.phi_u8_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat16:
      save(resolve(cmd.phi_u16_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat32:
      save(resolve(cmd.phi_u32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat64:
      save(resolve(cmd.phi_u64_->map_.at(call_stack.top().prev_)));
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
    case ir::Op::UncondJump: return cmd.block_;
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
