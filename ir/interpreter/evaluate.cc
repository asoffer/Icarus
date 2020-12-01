#include "ir/interpreter/evaluate.h"

#include "ast/expression.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/interpreter/foreign.h"
#include "ir/value/generic_fn.h"
#include "ir/value/value.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace interpreter {

template <typename InstSet>
static void CallFn(ir::NativeFn fn, StackFrame *frame,
                   absl::Span<ir::Addr const> ret_slots, ExecutionContext &ctx);

static void CallFn(ir::BuiltinFn fn, base::untyped_buffer_view arguments,
                   absl::Span<ir::Addr const> ret_slots, ExecutionContext &) {
  switch (fn.which()) {
    case ir::BuiltinFn::Which::Alignment: {
      type::Type type = arguments.get<type::Type>(0);
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(ret_slots[0].heap())) =
          type.alignment(kArchitecture).value();
    } break;
    case ir::BuiltinFn::Which::Bytes: {
      type::Type type = arguments.get<type::Type>(0);
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(ret_slots[0].heap())) =
          type.bytes(kArchitecture).value();
    } break;
    default: NOT_YET();
  }

  // size_t i = 0;
  // for (auto const &result : fn.Call(arguments)) {
  //   ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
  //   *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;
  // }
}

namespace {

// clang-format off
template <typename T>
concept HasResolveMemberFunction = requires(T t){
  // TODO: Check that the type is supported in ir::Value::supported_types
  { (void)t.Resolve() } -> std::same_as<void>;
};

// clang-format on

template <typename T>
void ResolveField(ExecutionContext &ctx, T &field) {
  if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
    field = ctx.resolve(field);
  } else if constexpr (base::meta<T>.template is_a<std::vector>() or
                       base::meta<T>.template is_a<absl::flat_hash_set>()) {
    for (auto &f : field) { ResolveField(ctx, f); }
  } else if constexpr (base::meta<T>.template is_a<absl::flat_hash_map>()) {
    for (auto &[k, v] : field) { ResolveField(ctx, v); }
  }
}

template <typename InstSet>
void Execute(ExecutionContext &ctx, ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots);

// Maximum size of any primitive type we may write
inline constexpr size_t kMaxSize = ir::Value::value_size_v;

using exec_t = void (*)(base::untyped_buffer::const_iterator *,
                        interpreter::ExecutionContext &,
                        absl::Span<ir::Addr const>);

template <typename InstSet, typename Inst>
constexpr exec_t GetInstruction() {
  return [](base::untyped_buffer::const_iterator *iter,
            interpreter::ExecutionContext &ctx,
            absl::Span<ir::Addr const> ret_slots) {
    if constexpr (base::meta<Inst> == base::meta<ir::CallInstruction>) {
      ir::Fn f = ctx.resolve(iter->read<ir::RegOr<ir::Fn>>().get());
      iter->read<core::Bytes>().get();

      type::Function const *fn_type = f.type();
      LOG("call", "%s: %s", f, fn_type->to_string());

      // TODO you probably want interpreter::Arguments or something.
      size_t num_inputs = fn_type->params().size();
      size_t num_regs   = 0;
      if (f.kind() == ir::Fn::Kind::Native) {
        if (f.native()->work_item and *f.native()->work_item) {
          (std::move(*f.native()->work_item))();
        }
        num_regs = f.native()->num_regs();
      }
      size_t num_entries = num_inputs + num_regs;
      auto call_buf = base::untyped_buffer::MakeFull(num_entries * kMaxSize);

      // TODO not actually optional once we handle foreign functions, we just
      // need deferred construction?
      std::optional<interpreter::StackFrame> frame;
      if (f.kind() == ir::Fn::Kind::Native) {
        frame = ctx.MakeStackFrame(f.native());
      }

      for (size_t i = 0; i < num_inputs; ++i) {
        if (iter->read<bool>()) {
          ir::Reg reg = iter->read<ir::Reg>();
          LOG("call", "%s", reg);

          if (frame) {
            frame->regs_.set_raw(ir::Reg::Arg(i),
                                 ctx.current_frame().regs_.raw(reg), kMaxSize);
          }
          ctx.MemCpyRegisterBytes(
              /*    dst = */ call_buf.raw((num_regs + i) * kMaxSize),
              /*    src = */ reg,
              /* length = */ kMaxSize);

        } else {
          type::Type t = fn_type->params()[i].value.type();
          if (t.get()->is_big()) {
            NOT_YET();
          } else {
            if (frame) {
              frame->regs_.set_raw(ir::Reg::Arg(i), iter->raw(), kMaxSize);
            }
            std::memcpy(call_buf.raw((num_regs + i) * kMaxSize), iter->raw(),
                        kMaxSize);
            iter->skip(t.bytes(interpreter::kArchitecture).value());
          }
        }
      }

      uint16_t num_rets = iter->read<uint16_t>();

      std::vector<ir::Addr> return_slots;
      return_slots.reserve(num_rets);
      ASSERT(fn_type->output().size() == num_rets);
      for (uint16_t i = 0; i < num_rets; ++i) {
        ir::Reg reg = iter->read<ir::Reg>();
        // NOTE: This is a hack using heap address slots to represent
        // registers since they are both void* and are used identically in the
        // interpreter.
        ir::Addr addr =
            (fn_type->output()[i].get()->is_big())
                ? ctx.resolve<ir::Addr>(reg)
                : ir::Addr::Heap(ctx.current_frame().regs_.raw(reg));

        LOG("call", "Ret addr = %s", addr);
        return_slots.push_back(addr);
      }

      Execute<InstSet>(ctx, f, std::move(call_buf), return_slots);
    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::GetReturnInstruction>) {
      uint16_t index = iter->read<uint16_t>();
      ctx.current_frame().regs_.set(iter->read<ir::Reg>(), ret_slots[index]);
    } else if constexpr (base::meta<Inst>.template is_a<ir::PhiInstruction>()) {
      uint16_t num   = iter->read<uint16_t>();
      uint64_t index = std::numeric_limits<uint64_t>::max();
      for (uint16_t i = 0; i < num; ++i) {
        if (ctx.current_frame().prev_index_ == iter->read<uintptr_t>()) {
          index = i;
        }
      }
      ASSERT(index != std::numeric_limits<uint64_t>::max());

      using type                = typename Inst::type;

      type result;
      for (uint16_t i = 0; i < num; ++i) {
        if (i == index) {
          result = ctx.resolve(iter->read<ir::RegOr<type>>().get());
        } else {
          iter->read<ir::RegOr<type>>();
        }
      }

      ctx.current_frame().regs_.set(iter->read<ir::Reg>(), result);
    } else if constexpr (
        base::meta<Inst>.template is_a<ir::SetReturnInstruction>()) {
      using type = typename Inst::type;
      uint16_t n = iter->read<uint16_t>();
      ASSERT(ret_slots.size() > n);
      ir::Addr ret_slot = ret_slots[n];
      type val          = ctx.resolve(iter->read<ir::RegOr<type>>().get());
      ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
      *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;

    } else if constexpr (HasResolveMemberFunction<Inst>) {
      auto inst = Inst::ReadFromByteCode(iter);
      std::apply([&](auto &... fields) { (ResolveField(ctx, fields), ...); },
                 inst.field_refs());

      ctx.current_frame().regs_.set(inst.result, inst.Resolve());

    } else if constexpr (base::meta<decltype(std::declval<Inst>().Apply(
                             ctx))> == base::meta<void>) {
      Inst::ReadFromByteCode(iter).Apply(ctx);
    } else {
      auto frame = Inst::ReadFromByteCode(iter).Apply(ctx);
      CallFn<InstSet>(frame.fn_, &frame, {}, ctx);
    }
  };
}

template <typename InstSet, typename... Insts>
constexpr std::array<exec_t, sizeof...(Insts)> MakeExecuteFunctions(
    base::type_list<Insts...>) {
  return {GetInstruction<InstSet, Insts>()...};
}

// TODO rename the `arguments` parameter. It actually should be arguments and
// space for registers.
template <typename InstSet>
void Execute(ExecutionContext &ctx, ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: {
      auto frame = ctx.MakeStackFrame(fn.native(), std::move(arguments));
      CallFn<InstSet>(fn.native(), &frame, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Builtin: {
      CallFn(fn.builtin(), arguments, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Foreign: {
      CallFn(fn.foreign(), arguments, ret_slots, ctx.stack());
    } break;
  }
}

template <typename InstSet>
inline constexpr std::array ExecutionArray =
    MakeExecuteFunctions<InstSet>(typename InstSet::instructions_t{});

template <typename InstSet>
void ExecuteBlocks(ExecutionContext &ctx,
                   absl::Span<ir::Addr const> ret_slots) {
  auto &buffer = ctx.current_frame().fn_->byte_code();

  auto iter = buffer.begin();
  while (true) {
    ASSERT(iter < buffer.end());
    ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
    switch (cmd_index) {
      case ir::internal::kReturnInstruction: return;
      case ir::internal::kUncondJumpInstruction: {
        uintptr_t offset = iter.read<uintptr_t>();
        ctx.current_frame().MoveTo(offset);
        iter = ctx.current_frame().fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::internal::kCondJumpInstruction: {
        ir::Reg r             = iter.read<ir::Reg>();
        uintptr_t true_block  = iter.read<uintptr_t>();
        uintptr_t false_block = iter.read<uintptr_t>();
        uintptr_t offset      = ctx.resolve<bool>(r) ? true_block : false_block;
        ctx.current_frame().MoveTo(offset);
        iter = ctx.current_frame().fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::LoadInstruction::kIndex: {
        uint16_t num_bytes = iter.read<uint16_t>();
        ir::Addr addr   = ctx.resolve(iter.read<ir::RegOr<ir::Addr>>().get());
        auto result_reg = iter.read<ir::Reg>().get();
        ctx.Load(result_reg, addr, core::Bytes(num_bytes));
      } break;

      default: ExecutionArray<InstSet>[cmd_index](&iter, ctx, ret_slots);
    }
  }
}

}  // namespace

void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots) {
  ExecutionContext ctx;
  Execute<instruction_set_t>(ctx, fn, std::move(arguments), ret_slots);
}

// TODO: Return potential errors.
void Execute(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() == 0u);
  // TODO actually just have a good way to construct the buffer
  LOG("Execute", "%s", fn);
  ExecutionContext ctx;
  Execute<instruction_set_t>(
      ctx, &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      {});
}

base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() != 0u);
  core::Bytes required = fn.type()->output()[0].bytes(kArchitecture);
  auto ret_buf         = base::untyped_buffer::MakeFull(required.value());
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  // TODO actually just have a good way to construct the buffer
  LOG("EvaluateToBuffer", "%s", fn);
  ExecutionContext ctx;
  Execute<instruction_set_t>(
      ctx, &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      ret_slots);
  return ret_buf;
}

// TODO why an r-value reference?
base::expected<ir::Value, EvaluationFailure> Evaluate(ir::CompiledFn &&fn) {
  LOG("Evaluate", "%s", fn);
  auto buf = EvaluateToBuffer(std::move(fn));
  std::vector<ir::Value> values;
  values.reserve(fn.type()->output().size());

  auto iter = buf.begin();
  for (type::Type t : fn.type()->output()) {
    if (t.is<type::GenericStruct>()) {
      values.push_back(ir::Value(t));
    } else {
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double, type::Type,
                       ir::Addr, ir::String, ir::ModuleId, ir::Scope, ir::Fn,
                       ir::Jump, ir::Block, ir::GenericFn>(
          t, [&]<typename T>() {
            T val = iter.read<T>();
            values.push_back(ir::Value(val));
          });
    }
  }

  switch (values.size()) {
    case 0: return ir::Value();
    case 1: return values[0];
    default: NOT_YET();
  }
}

template <typename InstSet>
static void CallFn(ir::NativeFn fn, StackFrame *frame,
                   absl::Span<ir::Addr const> ret_slots,
                   ExecutionContext &ctx) {
  // TODO: Understand why and how work-items may not be complete and add an
  // explanation here. I'm quite confident this is really possible with the
  // generics model I have, but I can't quite articulate exactly why it only
  // happens for generics and nothing else.
  //
  // Also, we shouldn't pay this lookup cost every time when it's mostly
  // irrelevant.
  //
  // TODO log an error if you're asked to execute a function that had an
  // error.
  if (fn->work_item and *fn->work_item) { (std::move(*fn->work_item))(); }

  [[maybe_unused]] auto restore_frame_token = ctx.PushFrame(frame);
  ExecuteBlocks<InstSet>(ctx, ret_slots);
}

}  // namespace interpreter
