#include "backend/exec.h"

#include <dlfcn.h>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <future>
#include <iostream>
#include <memory>
#include <thread>

#include "ast/ast.h"
#include "backend/eval.h"
#include "base/util.h"
#include "core/arch.h"
#include "error/log.h"
#include "ir/arguments.h"
#include "ir/basic_block.h"
#include "ir/cmd.h"
#include "ir/cmd/jumps.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
#include "type/incomplete_enum.h"
#include "type/incomplete_flags.h"
#include "type/type.h"
#include "type/util.h"

// TODO CreateStruct, CreateEnum, etc should register a deleter so if we exit
// early we don't leak them. FinalizeStruct, FinalizeEnum, etc should dergeister
// the deleter without calling it.

// TODO compile-time failure. dump the stack trace and abort for Null address
// kinds

namespace ast {
struct Expression;
}  // namespace ast

namespace backend {
base::untyped_buffer ReadOnlyData(0);

void Execute(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
             const std::vector<ir::Addr> &ret_slots, ExecContext *exec_ctx) {
  // TODO: Understand why and how work-items may not be complete and add an
  // explanation here. I'm quite confident this is really possible with the
  // generics model I have, but I can't quite articulate exactly why it only
  // happens for generics and nothing else.
  if (fn->work_item && *fn->work_item) { (std::move(*fn->work_item))(); }

  // TODO what about bound constants?
  exec_ctx->call_stack.emplace(fn, arguments, exec_ctx);

  // TODO log an error if you're asked to execute a function that had an
  // error.

  auto arch     = core::Interpretter();
  auto offset = core::Bytes{0};
  for (auto *t : fn->type_->output) {
    offset = core::FwdAlign(offset, t->alignment(arch)) + t->bytes(arch);
  }
  base::untyped_buffer ret_buffer(offset.value());

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

ExecContext::ExecContext() : stack_(50u) {}

ir::BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(ir::CompiledFn *fn,
                          const base::untyped_buffer &arguments,
                          ExecContext *ctx)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_.value())) {
  regs_.write(0, arguments);

  auto arch = core::Interpretter();
  fn->allocs().for_each([&](type::Type const *t, ir::Reg r) {
    DEBUG_LOG("allocs")
    ("Allocating type = ", t->to_string(), ", reg = ", r,
     ", offset = ", fn_->compiler_reg_to_offset_.at(r));
    regs_.set(fn_->compiler_reg_to_offset_.at(r),
              ir::Addr::Stack(core::FwdAlign(core::Bytes{ctx->stack_.size()},
                                             t->alignment(arch))
                                  .value()));

    ctx->stack_.append_bytes(t->bytes(arch).value(),
                             t->alignment(arch).value());
  });
}

ir::BlockIndex ExecContext::ExecuteBlock(
    const std::vector<ir::Addr> &ret_slots) {
  return current_block().cmd_buffer_.Execute(ret_slots, this);
}

template <typename T>
static T LoadValue(ir::Addr addr, base::untyped_buffer const &stack) {
  switch (addr.kind) {
    case ir::Addr::Kind::Heap:
      return *ASSERT_NOT_NULL(static_cast<T *>(addr.as_heap));
    case ir::Addr::Kind::Stack: return stack.get<T>(addr.as_stack);
    case ir::Addr::Kind::ReadOnly: return ReadOnlyData.get<T>(addr.as_rodata);
  }
  UNREACHABLE(DUMP(static_cast<int>(addr.kind)));
}

template <typename T>
static void StoreValue(T val, ir::Addr addr, base::untyped_buffer *stack) {
  if constexpr (std::is_same_v<std::decay_t<T>, void *>) {
    auto start  = reinterpret_cast<uintptr_t>(stack->raw(0));
    auto end    = reinterpret_cast<uintptr_t>(stack->raw(stack->size()));
    auto result = reinterpret_cast<uintptr_t>(val);
    if (start <= result && result < end) {
      StoreValue(ir::Addr::Stack(result - start), addr, stack);
    } else {
      StoreValue(ir::Addr::Heap(val), addr, stack);
    }
    // TODO readonly data
  } else {
    switch (addr.kind) {
      case ir::Addr::Kind::Stack: stack->set(addr.as_stack, val); return;
      case ir::Addr::Kind::ReadOnly:
        ReadOnlyData.set(addr.as_rodata, val);
        return;
      case ir::Addr::Kind::Heap:
        *ASSERT_NOT_NULL(static_cast<T *>(addr.as_heap)) = val;
    }
  }
}


template <size_t N, typename... Ts>
struct RetrieveArgs;

template <size_t N, typename T, typename... Ts>
struct RetrieveArgs<N, T, Ts...> {
  template <typename OutTup>
  void operator()(base::untyped_buffer const &arguments, size_t *index,
                  OutTup *out_tup) {
    if constexpr (std::is_same_v<T, void *>) {
      // Any pointer-type alignment is fine. (TODO they're always the same,
      // right?)
      *index = ((*index - 1) | (alignof(ir::Addr) - 1)) + 1;
      auto addr = arguments.get<ir::Addr>(*index);
      void *ptr = nullptr;
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: NOT_YET(addr.as_stack);
        case ir::Addr::Kind::Heap:
          ptr = static_cast<void *>(addr.as_heap);
          break;
        case ir::Addr::Kind::ReadOnly:
          ptr = static_cast<void *>(ReadOnlyData.raw(addr.as_rodata));
          break;
        default: UNREACHABLE(static_cast<int>(addr.kind));
      }
      std::get<N>(*out_tup) = ptr;
      *index += sizeof(ir::Addr);
      RetrieveArgs<N + 1, Ts...>{}(arguments, index, out_tup);
    } else {
      auto arch = core::Interpretter();
      auto t    = type::Get<T>();
      *index =
          core::FwdAlign(core::Bytes{*index}, t->alignment(arch)).value();
      std::get<N>(*out_tup) = arguments.get<T>(*index);
      *index += t->bytes(arch).value();
      RetrieveArgs<N + 1, Ts...>{}(arguments, index, out_tup);
    }
  }
};

template <size_t N>
struct RetrieveArgs<N> {
  template <typename OutTup>
  void operator()(base::untyped_buffer const &arguments, size_t *index,
                  OutTup *out_tup) {}
};

template <typename... Ts>
std::tuple<Ts...> MakeTupleArgs(base::untyped_buffer const &arguments) {
  std::tuple<Ts...> results;
  size_t index = 0;
  RetrieveArgs<0, Ts...>{}(arguments, &index, &results);
  return results;
}

// TODO Generalize this based on calling convention.
template <typename Out, typename... Ins>
void FfiCall(ir::Foreign const &f, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> *ret_slots, base::untyped_buffer *stack) {
  using fn_t = Out (*)(Ins...);
  fn_t fn    = (fn_t)(f.get());

  if constexpr (std::is_same_v<Out, void>) {
    std::apply(fn, MakeTupleArgs<Ins...>(arguments));
  } else {
    StoreValue(std::apply(fn, MakeTupleArgs<Ins...>(arguments)),
               ret_slots->at(0), stack);
  }
}

void CallForeignFn(ir::Foreign const &f, base::untyped_buffer const &arguments,
                   std::vector<ir::Addr> ret_slots,
                   base::untyped_buffer *stack) {
  // TODO we can catch locally or in the same lexical scope stack if a
  // redeclaration of the same foreign symbol has a different type.
  // TODO handle failures gracefully.
  // TODO Consider caching these.
  // TODO Handle a bunch more function types in a coherent way.
  auto *fn_type = &f.type()->as<type::Function>();
  if (fn_type == type::Func({type::Int64}, {type::Int64})) {
    FfiCall<int64_t, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Int64}, {})) {
    FfiCall<void, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Float64}, {type::Float64})) {
    FfiCall<double, double>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Float32}, {type::Float32})) {
    FfiCall<float, float>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({}, {type::Int64})) {
    FfiCall<int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Nat8}, {type::Int64})) {
    FfiCall<int64_t, uint8_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 &&
             fn_type->input[0]->is<type::Pointer>() &&
             fn_type->output.size() == 1 && fn_type->output[0] == type::Int64) {
    FfiCall<int64_t, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 2 &&
             fn_type->input[0]->is<type::Pointer>() &&
             fn_type->input[1]->is<type::Pointer>() &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, void *, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 2 && fn_type->input[0] == type::Int64 &&
             fn_type->input[1]->is<type::Pointer>() &&
             fn_type->output.size() == 1 && fn_type->output[0] == type::Int64) {
    FfiCall<int64_t, int64_t, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->input[0] == type::Int64 &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->input[0] == type::Nat64 &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, uint64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->output.empty() &&
             fn_type->input[0]->is<type::Pointer>()) {
    FfiCall<void, void *>(f, arguments, &ret_slots, stack);
  } else {
    UNREACHABLE(fn_type->to_string());
  }
}

ir::BlockIndex ExecContext::ExecuteCmd(
    const ir::Cmd &cmd, const std::vector<ir::Addr> &ret_slots) {
  auto save = [&](auto val) {
    call_stack.top().regs_.set(
        call_stack.top().fn_->compiler_reg_to_offset_.at(cmd.result), val);
  };

  switch (cmd.op_code_) {
    case ir::Op::Death: UNREACHABLE(call_stack.top().fn_);
    case ir::Op::Bytes:
      save(resolve(cmd.type_arg_)->bytes(core::Interpretter()));
      break;
    case ir::Op::Align:
      save(resolve(cmd.type_arg_)->alignment(core::Interpretter()));
      break;
    case ir::Op::Move: {
      auto *t = cmd.special2_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr) * 2);
      call_buf.append(resolve(cmd.special2_.regs_[0]));
      call_buf.append(resolve(cmd.special2_.regs_[1]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->move_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->move_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->move_assign_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Copy: {
      auto *t = cmd.special2_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr) * 2);
      call_buf.append(resolve(cmd.special2_.regs_[0]));
      call_buf.append(resolve(cmd.special2_.regs_[1]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->copy_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->copy_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->copy_assign_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Init: {
      auto *t = cmd.special1_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr));
      call_buf.append(resolve(cmd.special1_.regs_[0]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
       f = s->init_func_;
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->init_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->init_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Destroy: {
      auto *t = cmd.special1_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr));
      call_buf.append(resolve(cmd.special1_.regs_[0]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->destroy_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->destroy_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->destroy_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::CreateStruct:
      save(new type::Struct(cmd.create_struct_.scope_,
                            cmd.create_struct_.scope_->module(),
                            cmd.create_struct_.parent_));
      break;
    case ir::Op::CreateStructField: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.create_struct_field_.struct_));
      struct_to_modify->add_field(resolve(cmd.create_struct_field_.type_));
    } break;
    case ir::Op::SetStructFieldName: {
      ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.set_struct_field_name_.struct_))
          ->set_last_name(cmd.set_struct_field_name_.name_);
    } break;
    case ir::Op::AddHashtagToField: {
      ASSERT_NOT_NULL(resolve<type::Struct *>(cmd.add_hashtag_.struct_))
          ->add_hashtag_to_last_field(cmd.add_hashtag_.hashtag_);
    } break;
    case ir::Op::AddHashtagToStruct: {
      ASSERT_NOT_NULL(resolve<type::Struct *>(cmd.add_hashtag_.struct_))
          ->add_hashtag(cmd.add_hashtag_.hashtag_);
    } break;
    case ir::Op::FinalizeStruct: {

      auto *s = resolve<type::Struct *>(cmd.reg_);
      auto &cache = s->mod_->generic_struct_cache_[s->parent_];

      // TODO this is horrendous. Find a better way to populate the back_ map.
      std::vector<type::Type const *> const *input_vals = nullptr;
      for (auto const &[vals, t] : cache.fwd_) {
        if (t != s) { continue; }
        input_vals = &vals;
        break;
      }
      if (input_vals != nullptr) {
        bool inserted = cache.back_.emplace(s, input_vals).second;
        ASSERT(inserted == true);
      }

      s->init_func_ = s->mod_->AddFunc(
          type::Func({type::Ptr(s)}, {}),
          core::FnParams(core::Param{"", type::Typed<ast::Expression const *>{
                                             nullptr, type::Ptr(s)}}));
      CURRENT_FUNC(s->init_func_.func()) {
        // TODO this context gets no constants which is not what we want.
        // Probably need to store the correct bound constants pointer in the
        // struct type when we create create it initially.
        Context ctx(s->mod_);

        ir::BasicBlock::Current = ir::CompiledFn::Current->entry();
        auto var                = ir::Reg::Arg(0);
        size_t i                = 0;
        visitor::EmitIr visitor;
        for (auto const &field : s->parent_->fields_) {
          auto ir_field = ir::Field(var, s, i);
          if (field.init_val()) {
            field.init_val()->EmitCopyInit(&visitor, ir_field, &ctx);
          } else {
            s->fields_.at(i).type->EmitDefaultInit(&visitor, ir_field.get(),
                                                   &ctx);
          }
          ++i;
        }
        visitor.CompleteDeferredBodies();

        ir::ReturnJump();
      }

      save(s);

      // TODO set backwards map.
    } break;
    case ir::Op::DebugIr: {
      std::stringstream ss;
      ss << *call_stack.top().fn_;
      DEBUG_LOG()(ss.str());
    } break;
    case ir::Op::Array: {
      save(type::Arr(resolve(cmd.array_.len_), resolve(cmd.array_.type_)));
    } break;
    case ir::Op::VariantType: save(resolve(cmd.addr_arg_)); break;
    case ir::Op::VariantValue: {
      auto arch      = core::Interpretter();
      auto bytes     = type::Type_->bytes(arch);
      auto bytes_fwd = core::FwdAlign(bytes, type::Type_->alignment(arch));
      auto addr      = resolve(cmd.addr_arg_);
      switch (addr.kind) {
        case ir::Addr::Kind::Stack:
          addr.as_stack += bytes_fwd.value();
          save(addr);
          break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                             bytes_fwd.value());
          save(addr);
          break;
        case ir::Addr::Kind::ReadOnly:
          addr.as_rodata += bytes_fwd.value();
          save(addr);
          break;
      }
    } break;
    case ir::Op::PtrIncr: {
      auto addr = resolve(cmd.ptr_incr_.ptr_);
      auto bytes_fwd =
          type::Array(resolve(cmd.ptr_incr_.incr_), cmd.ptr_incr_.pointee_type_)
              .bytes(core::Interpretter());
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: addr.as_stack += bytes_fwd.value(); break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<char *>(addr.as_heap) + bytes_fwd.value();
          break;
        case ir::Addr::Kind::ReadOnly:
          addr.as_rodata += bytes_fwd.value();
          break;
      }
      save(addr);
    } break;
    case ir::Op::Field: {
      auto addr   = resolve(cmd.field_.ptr_);
      auto offset = core::Bytes{0};
      auto arch   = core::Interpretter();
      if (cmd.field_.type_->is<type::Struct>()) {
        offset =
            cmd.field_.type_->as<type::Struct>().offset(cmd.field_.num_, arch);
      } else if (cmd.field_.type_->is<type::Tuple>()) {
        offset =
            cmd.field_.type_->as<type::Tuple>().offset(cmd.field_.num_, arch);
      } else {
        UNREACHABLE();
      }
      if (addr.kind == ir::Addr::Kind::Stack) {
        addr.as_stack += offset.value();
      } else {
        addr.as_heap =
            static_cast<void *>(static_cast<char *>(addr.as_heap) + offset.value());
      }
      save(addr);
    } break;
    case ir::Op::Call: {
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the
      // interpretter.
      std::vector<ir::Addr> return_slots;
      if (cmd.call_.outs_ != nullptr) {
        return_slots.reserve(cmd.call_.outs_->size());
        for (size_t i = 0; i < cmd.call_.outs_->size(); ++i) {
          if (cmd.call_.outs_->is_loc_[i]) {
            return_slots.push_back(resolve<ir::Addr>(cmd.call_.outs_->regs_[i]));
          } else {
            auto addr = ir::Addr::Heap(call_stack.top().regs_.raw(
                call_stack.top().fn_->compiler_reg_to_offset_.at(
                    cmd.call_.outs_->regs_[i])));
            return_slots.push_back(addr);
          }
        }
      }

      auto call_buf = cmd.call_.arguments_->PrepareCallBuffer(
          call_stack.top().fn_->compiler_reg_to_offset_,
          call_stack.top().regs_);
      ir::AnyFunc f = resolve(cmd.call_.fn_);

      // TODO you need to be able to determine how many args there are
      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::NewOpaqueType: save(new type::Opaque(cmd.mod_)); break;
    case ir::Op::LoadSymbol: {
      void *sym = [&]() -> void * {
        // TODO: this is a hack for now untill we figure out why we can load
        // stderr as a symbol but not write to it.
        if (cmd.load_sym_.name_ == "stderr") { return stderr; }
        if (cmd.load_sym_.name_ == "stdout") { return stdout; }
        return ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, cmd.load_sym_.name_.data()));
      }();
      if (cmd.load_sym_.type_->is<type::Function>()) {
        save(ir::AnyFunc{ir::Foreign(sym, cmd.load_sym_.type_)});
      } else if (cmd.load_sym_.type_->is<type::Pointer>()) {
        save(ir::Addr::Heap(sym));
      } else {
        NOT_YET(cmd.load_sym_.type_);
      }
    } break;
    case ir::Op::CreateEnum: save(new type::IncompleteEnum(cmd.mod_)); break;
    case ir::Op::AddEnumerator: {
      resolve<type::IncompleteEnum *>(cmd.add_enumerator_.enum_)
          ->add(cmd.add_enumerator_.name_);
    } break;
    case ir::Op::SetEnumerator: {
      resolve<type::IncompleteEnum *>(cmd.set_enumerator_.enum_)
          ->set_last_value(resolve(cmd.set_enumerator_.val_));
    } break;
    case ir::Op::FinalizeEnum: {
      auto *e = resolve<type::IncompleteEnum *>(cmd.reg_);
      save(std::move(*e).finalize());
      delete e;
    } break;
    case ir::Op::CreateFlags: save(new type::IncompleteFlags(cmd.mod_)); break;
    case ir::Op::AddFlag: {
      resolve<type::IncompleteFlags *>(cmd.add_enumerator_.enum_)
          ->add(cmd.add_enumerator_.name_);
    } break;
    case ir::Op::SetFlag: {
      resolve<type::IncompleteFlags *>(cmd.add_enumerator_.enum_)
          ->set_last_value(resolve(cmd.set_enumerator_.val_));
    } break;
    case ir::Op::FinalizeFlags: {
      auto *f = resolve<type::IncompleteFlags *>(cmd.reg_);
      save(std::move(*f).finalize());
      delete f;
    } break;
    case ir::Op::GetRet: save(ret_slots.at(cmd.get_ret_)); break;
    case ir::Op::PhiBool:
      save(resolve(cmd.phi_bool_->map_.at(call_stack.top().prev_)));
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
    case ir::Op::PhiEnum:
      save(resolve(cmd.phi_enum_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFlags:
      save(resolve(cmd.phi_flags_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFunc:
      save(resolve(cmd.phi_func_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::ArgumentCache: {
      // TODO currently cache is dependetn on all args but requires that they be
      // types.
      std::vector<type::Type const*> cached_vals;
      cached_vals.reserve(cmd.sl_->args_.size());
      for (uint64_t i = 0; i < cmd.sl_->args_.size(); ++i) {
        cached_vals.push_back(resolve<type::Type const *>(ir::Reg{i}));
      }
      auto &cache = cmd.sl_->module()->generic_struct_cache_[cmd.sl_];
      auto iter = cache.fwd_.try_emplace(std::move(cached_vals), nullptr).first;
      type::Type const **cache_slot = &iter->second;
      // Backwards direction set in FinalizeStruct
      save(ir::Addr::Heap(cache_slot));
    } break;
    case ir::Op::CreateContext: save(new Context(cmd.mod_)); break;
    case ir::Op::AddBoundConstant: {
      NOT_YET();
    } break;
    case ir::Op::DestroyContext: delete resolve<Context *>(cmd.reg_); break;
    case ir::Op::VerifyType: {
      visitor::VerifyType visitor;
      cmd.ast_.node_->VerifyType(&visitor, resolve<Context *>(cmd.ast_.ctx_));
    } break;
    case ir::Op::EvaluateAsType: {
      // TODO, you don't have a parent context... that could be problematic.
      save(EvaluateAs<type::Type const *>(
          type::Typed(&cmd.ast_.node_->as<ast::Expression const>(), type::Type_),
          resolve<Context *>(cmd.ast_.ctx_)));
    } break;
    case ir::Op::JumpPlaceholder: UNREACHABLE(call_stack.top().fn_);
    case ir::Op::CreateScopeDef: {
      // TODO consider the implications of leaking this. I'm not convinced there
      // exist scenarios where we're leaking and could clean it up. I.e., every
      // time we create one it's because we expect it to live forever. I suppose
      // we could ref-count the results of functions and delete these if they're
      // unused?
      call_stack.top().scope_defs_.push(cmd.create_scope_def_.scope_def_);
      save(cmd.create_scope_def_.scope_def_);
    } break;
    case ir::Op::FinishScopeDef: {
      ASSERT(call_stack.top().scope_defs_.size() != 0u);
      ASSERT(call_stack.top().block_defs_.size() == 0u);
      call_stack.top().scope_defs_.top()->work_item = nullptr;
      call_stack.top().scope_defs_.pop();
    } break;
    case ir::Op::AddScopeDefInit:
      resolve<ir::ScopeDef *>(cmd.add_scope_def_init_.reg_)
          ->AddInit(resolve(cmd.add_scope_def_init_.f_));
      break;
    case ir::Op::AddScopeDefDone:
      resolve<ir::ScopeDef *>(cmd.add_scope_def_done_.reg_)
          ->AddDone(resolve(cmd.add_scope_def_done_.f_));
      break;
    case ir::Op::CreateBlockDef:
      call_stack.top().block_defs_.emplace(
          resolve<ast::BlockLiteral const *>(cmd.block_lit_));
      break;
    case ir::Op::FinishBlockDef:
      ASSERT(call_stack.top().block_defs_.size() != 0u);
      call_stack.top().scope_defs_.top()->AddBlockDef(
          resolve(cmd.byte_view_arg_),
          std::move(call_stack.top().block_defs_.top()));
      call_stack.top().block_defs_.pop();
      break;
    case ir::Op::AddBlockDefBefore:
      call_stack.top().block_defs_.top().AddBefore(resolve(cmd.any_fn_));
      break;
    case ir::Op::AddBlockDefAfter:
      call_stack.top().block_defs_.top().AddAfter(resolve(cmd.any_fn_));
      break;
  }

  return ir::BlockIndex{-2};
}
}  // namespace backend
