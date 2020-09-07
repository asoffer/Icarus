#ifndef ICARUS_IR_INSTRUCTION_SET_H
#define ICARUS_IR_INSTRUCTION_SET_H

#include <array>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/random/random.h"
#include "absl/strings/str_format.h"
#include "base/meta.h"
#include "ir/instruction/instructions.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/execute.h"
#include "ir/interpretter/foreign.h"
#include "ir/jump.h"
#include "ir/read_only_data.h"
#include "ir/scope_def.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/primitive.h"

namespace interpretter {

template <typename InstSet>
void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx);

template <typename InstSet>
void ExecuteBlocks(ExecutionContext &ctx,
                   absl::Span<ir::Addr const> ret_slots) {
  auto &buffer = ctx.current_frame()->fn_->byte_code();

  auto iter = buffer.begin();
  while (true) {
    ASSERT(iter < buffer.end());
    ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
    switch (cmd_index) {
      case ir::internal::kReturnInstruction: return;
      case ir::internal::kUncondJumpInstruction: {
        uintptr_t offset = iter.read<uintptr_t>();
        ctx.current_frame()->MoveTo(offset);
        iter = ctx.current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::internal::kCondJumpInstruction: {
        ir::Reg r             = iter.read<ir::Reg>();
        uintptr_t true_block  = iter.read<uintptr_t>();
        uintptr_t false_block = iter.read<uintptr_t>();
        uintptr_t offset      = ctx.resolve<bool>(r) ? true_block : false_block;
        ctx.current_frame()->MoveTo(offset);
        iter = ctx.current_frame()->fn_->byte_code().begin();
        iter.skip(offset);
      } break;
      case ir::LoadInstruction::kIndex: {
        uint16_t num_bytes = iter.read<uint16_t>();
        bool is_reg        = iter.read<bool>();
        ir::Addr addr      = ctx.ReadAndResolve<ir::Addr>(is_reg, &iter);
        auto result_reg    = iter.read<ir::Reg>().get();
        DEBUG_LOG("load-instruction")(num_bytes, " ", addr, " ", result_reg);
        switch (addr.kind()) {
          case ir::Addr::Kind::Stack: {
            ctx.current_frame()->regs_.set_raw(
                result_reg, ctx.stack_.raw(addr.stack()), num_bytes);
          } break;
          case ir::Addr::Kind::ReadOnly: {
            auto handle = ir::ReadOnlyData.lock();
            ctx.current_frame()->regs_.set_raw(
                result_reg, handle->raw(addr.rodata()), num_bytes);
          } break;
          case ir::Addr::Kind::Heap: {
            ctx.current_frame()->regs_.set_raw(result_reg, addr.heap(),
                                               num_bytes);
          } break;
        }
      } break;

      default: InstSet::Execute[cmd_index](&iter, &ctx, ret_slots);
    }
  }
}

template <typename InstSet>
inline void CallFn(ir::NativeFn fn, StackFrame *frame,
                   absl::Span<ir::Addr const> ret_slots,
                   interpretter::ExecutionContext *ctx) {
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

  auto *old_frame     = ctx->current_frame();
  ctx->current_frame_ = ASSERT_NOT_NULL(frame);
  ExecuteBlocks<InstSet>(*ctx, ret_slots);
  ctx->current_frame_ = old_frame;
}

}  // namespace interpretter

namespace ir {

template <typename Capabilities, typename... InstructionsOrSets>
struct InstructionSet;

namespace internal_instructions {

inline constexpr uint8_t ReverseByte(uint8_t byte) {
  byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
  byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
  byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
  return byte;
}

template <typename SizeType, typename Iter>
inline std::vector<bool> ReadBits(Iter *iter) {
  static_assert(std::disjunction_v<
                std::is_same<Iter, base::untyped_buffer::iterator>,
                std::is_same<Iter, base::untyped_buffer::const_iterator>>);
  SizeType num = iter->template read<SizeType>();

  uint8_t current = 0;

  std::vector<bool> bits;
  bits.reserve(num);
  for (SizeType i = 0; i < num; ++i) {
    if (i % 8 == 0) { current = ReverseByte(iter->template read<uint8_t>()); }
    bits.push_back(current & 1);
    current >>= 1;
  }
  return bits;
}

template <typename SizeType, typename T, typename Iter, typename Fn>
inline auto Deserialize(Iter *iter, Fn &&fn) {
  static_assert(std::disjunction_v<
                std::is_same<Iter, base::untyped_buffer::iterator>,
                std::is_same<Iter, base::untyped_buffer::const_iterator>>);
  auto bits = ReadBits<SizeType>(iter);

  using result_type =
      std::decay_t<decltype(fn(std::declval<base::unaligned_ref<ir::Reg>>()))>;
  if constexpr (std::is_void_v<result_type>) {
    for (bool b : bits) {
      if (b) {
        fn(iter->template read<ir::Reg>());
      } else {
        iter->template read<T>();
      }
    }
    return;
  } else {
    std::vector<result_type> vals;
    vals.reserve(bits.size());
    for (bool b : bits) {
      vals.push_back(b ? fn(iter->template read<ir::Reg>())
                       : static_cast<T>(iter->template read<T>()));
    }
    return vals;
  }
}

// Maximum size of any primitive type we may write
inline constexpr size_t kMaxSize = 8;


// Note: The ordering here is very important
using exec_t = void (*)(base::untyped_buffer::const_iterator *,
                        interpretter::ExecutionContext *,
                        absl::Span<ir::Addr const>);

template <typename>
struct IsInstructionSet : std::false_type {};

template <typename Capabilities, typename... InstructionsOrSets>
struct IsInstructionSet<InstructionSet<Capabilities, InstructionsOrSets...>>
    : std::true_type {};

template <typename... Processed>
auto ExpandedInstructions(base::type_list<>, base::type_list<Processed...>) {
  return static_cast<base::type_list<Processed...>>(nullptr);
}

template <typename T, typename... Ts, typename... Processed>
auto ExpandedInstructions(base::type_list<T, Ts...>,
                          base::type_list<Processed...>) {
  if constexpr (((base::meta<T> == base::meta<Processed>) or ...)) {
    return ExpandedInstructions(
        static_cast<base::type_list<Ts...>>(nullptr),
        static_cast<base::type_list<Processed...>>(nullptr));
  } else if constexpr (IsInstructionSet<T>::value) {
    return ExpandedInstructions(
        static_cast<base::type_list_cat<typename T::instructions_t,
                                        base::type_list<Ts...>>>(nullptr),
        static_cast<base::type_list<Processed...>>(nullptr));
  } else {
    return ExpandedInstructions(
        static_cast<base::type_list<Ts...>>(nullptr),
        static_cast<base::type_list<T, Processed...>>(nullptr));
  }
}

template <typename InstSet, typename Inst>
constexpr exec_t GetInstruction() {
  return [](base::untyped_buffer::const_iterator *iter,
            interpretter::ExecutionContext *ctx,
            absl::Span<ir::Addr const> ret_slots) {
    if constexpr (base::meta<Inst> == base::meta<ir::CallInstruction>) {
      bool fn_is_reg = iter->read<bool>();
      ir::Fn f       = ctx->ReadAndResolve<ir::Fn>(fn_is_reg, iter);
      iter->read<core::Bytes>().get();

      type::Function const *fn_type = f.type();
      DEBUG_LOG("call")(f, ": ", fn_type->to_string());

      // TODO you probably want interpretter::Arguments or something.
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
      std::optional<interpretter::StackFrame> frame;
      if (f.kind() == ir::Fn::Kind::Native) {
        frame.emplace(f.native(), &ctx->stack_);
      }

      for (size_t i = 0; i < num_inputs; ++i) {
        if (iter->read<bool>()) {
          ir::Reg reg = iter->read<ir::Reg>();
          DEBUG_LOG("call")(reg);

          if (frame) {
            frame->regs_.set_raw(ir::Reg::Arg(i),
                                 ctx->current_frame()->regs_.raw(reg),
                                 kMaxSize);
          }
          ctx->MemCpyRegisterBytes(
              /*    dst = */ call_buf.raw((num_regs + i) * kMaxSize),
              /*    src = */ reg,
              /* length = */ kMaxSize);

        } else {
          type::Type const *t = fn_type->params()[i].value.type();
          if (t->is_big()) {
            NOT_YET();
          } else {
            if (frame) {
              frame->regs_.set_raw(ir::Reg::Arg(i), iter->raw(), kMaxSize);
            }
            std::memcpy(call_buf.raw((num_regs + i) * kMaxSize), iter->raw(),
                        kMaxSize);
            iter->skip(t->bytes(interpretter::kArchitecture).value());
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
        // interpretter.
        ir::Addr addr =
            (fn_type->output()[i]->is_big())
                ? ctx->resolve<ir::Addr>(reg)
                : ir::Addr::Heap(ctx->current_frame()->regs_.raw(reg));

        DEBUG_LOG("call")("Ret addr = ", addr);
        return_slots.push_back(addr);
      }

      interpretter::Execute<InstSet>(f, std::move(call_buf), return_slots, ctx);
    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::EnumerationInstruction>) {
      using enum_t             = uint64_t;
      bool is_enum_not_flags   = iter->read<bool>();
      uint16_t num_enumerators = iter->read<uint16_t>();
      uint16_t num_specified   = iter->read<uint16_t>();
      module::BasicModule *mod = iter->read<module::BasicModule *>();
      std::vector<std::pair<std::string_view, std::optional<enum_t>>>
          enumerators;
      enumerators.reserve(num_enumerators);
      for (uint16_t i = 0; i < num_enumerators; ++i) {
        enumerators.emplace_back(iter->read<std::string_view>(), std::nullopt);
      }

      absl::flat_hash_set<enum_t> vals;

      for (uint16_t i = 0; i < num_specified; ++i) {
        uint64_t index            = iter->read<uint64_t>();
        auto b                    = iter->read<bool>();
        enum_t val                = ctx->ReadAndResolve<enum_t>(b, iter);
        enumerators[index].second = val;
        vals.insert(val);
      }

      type::Type *result = nullptr;
      absl::BitGen gen;

      if (is_enum_not_flags) {
        for (auto &[name, maybe_val] : enumerators) {
          DEBUG_LOG("enum")(name, " => ", maybe_val);

          if (not maybe_val.has_value()) {
            bool success;
            enum_t x;
            do {
              x       = absl::Uniform<enum_t>(gen);
              success = vals.insert(x).second;
              DEBUG_LOG("enum")("Adding value ", x, " for ", name);
              maybe_val = x;
            } while (not success);
          }
        }
        absl::flat_hash_map<std::string, ir::EnumVal> mapping;

        for (auto [name, maybe_val] : enumerators) {
          ASSERT(maybe_val.has_value() == true);
          mapping.emplace(std::string(name), ir::EnumVal{maybe_val.value()});
        }
        DEBUG_LOG("enum")(vals, ", ", mapping);
        result = type::Allocate<type::Enum>(mod, std::move(mapping));
      } else {
        for (auto &[name, maybe_val] : enumerators) {
          DEBUG_LOG("flags")(name, " => ", maybe_val);

          if (not maybe_val.has_value()) {
            bool success;
            enum_t x;
            do {
              x       = absl::Uniform<enum_t>(absl::IntervalClosedOpen, gen, 0,
                                        std::numeric_limits<enum_t>::digits);
              success = vals.insert(x).second;
              DEBUG_LOG("enum")("Adding value ", x, " for ", name);
              maybe_val = x;
            } while (not success);
          }
        }

        absl::flat_hash_map<std::string, ir::FlagsVal> mapping;

        for (auto [name, maybe_val] : enumerators) {
          ASSERT(maybe_val.has_value() == true);
          mapping.emplace(std::string(name),
                          ir::FlagsVal{enum_t{1} << maybe_val.value()});
        }

        DEBUG_LOG("flags")(vals, ", ", mapping);
        result = type::Allocate<type::Flags>(mod, std::move(mapping));
      }

      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(), result);
    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::MakeScopeInstruction>) {
      ir::ScopeDef *scope_def = iter->read<ir::ScopeDef *>();

      auto after = Deserialize<uint16_t, ir::Jump const *>(
          iter,
          [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump const *>(reg); });
      *scope_def->start_ = ir::BlockDef(
          absl::flat_hash_set<ir::Jump const *>(after.begin(), after.end()));

      scope_def->exit_->before_ = ir::OverloadSet(Deserialize<uint16_t, ir::Fn>(
          iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::Fn>(reg); }));

      uint16_t num_blocks = iter->read<uint16_t>();
      for (uint16_t i = 0; i < num_blocks; ++i) {
        std::string_view name = iter->read<std::string_view>();
        ir::BlockDef *block   = iter->read<ir::BlockDef *>();
        scope_def->blocks_.emplace(name, block);
      }

      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(), scope_def);

    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::MakeBlockInstruction>) {
      ir::BlockDef *block_def = iter->read<ir::BlockDef *>();
      auto before             = ir::OverloadSet(Deserialize<uint16_t, ir::Fn>(
          iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::Fn>(reg); }));
      auto after              = Deserialize<uint16_t, ir::Jump const *>(
          iter,
          [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump const *>(reg); });
      *block_def = ir::BlockDef(
          absl::flat_hash_set<ir::Jump const *>(after.begin(), after.end()));
      block_def->before_ = std::move(before);

      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(), block_def);

    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::StructInstruction>) {
      uint16_t num = iter->read<uint16_t>();
      module::BasicModule const *mod =
          iter->read<module::BasicModule const *>();
      type::Struct *struct_type = iter->read<type::Struct *>();

      std::vector<type::Struct::Field> fields;
      fields.reserve(num);
      for (uint16_t i = 0; i < num; ++i) {
        std::string_view name = iter->read<std::string_view>();
        if (iter->read<bool>()) {
          type::Type const *t = iter->read<type::Type const *>();

          ir::Value init_val = iter->read<ir::Value>();

          fields.push_back(type::Struct::Field{
              .name          = std::string(name),
              .type          = t,
              .initial_value = init_val,
              .hashtags_     = {},
          });
        } else {
          fields.push_back(type::Struct::Field{
              .name = std::string(name),
              .type = ctx->resolve(
                  iter->read<ir::RegOr<type::Type const *>>().get()),
              .initial_value = ir::Value(),
              .hashtags_     = {},
          });
        }
      }

      struct_type->AppendFields(std::move(fields));

      if (iter->read<bool>()) {
        struct_type->SetMoveAssignment(iter->read<ir::Fn>());
      }

      if (iter->read<bool>()) {
        struct_type->SetDestructor(iter->read<ir::Fn>());
      }

      type::Struct const *const_struct_type = struct_type;
      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(), const_struct_type);
    } else if constexpr (base::meta<Inst> ==
                         base::meta<ir::GetReturnInstruction>) {
      uint16_t index = iter->read<uint16_t>();
      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(), ret_slots[index]);
    } else if constexpr (base::meta<Inst>.template is_a<ir::PhiInstruction>()) {
      uint16_t num   = iter->read<uint16_t>();
      uint64_t index = std::numeric_limits<uint64_t>::max();
      for (uint16_t i = 0; i < num; ++i) {
        if (ctx->current_frame()->prev_index_ == iter->read<uintptr_t>()) {
          index = i;
        }
      }
      ASSERT(index != std::numeric_limits<uint64_t>::max());

      using type                = typename Inst::type;
      std::vector<type> results = Deserialize<uint16_t, type>(
          iter, [ctx](ir::Reg reg) { return ctx->resolve<type>(reg); });
      ctx->current_frame()->regs_.set(iter->read<ir::Reg>(),
                                      type{results[index]});
    } else if constexpr (
        base::meta<Inst>.template is_a<ir::SetReturnInstruction>()) {
      using type = typename Inst::type;
      uint16_t n = iter->read<uint16_t>();
      ASSERT(ret_slots.size() > n);
      ir::Addr ret_slot = ret_slots[n];
      bool is_reg       = iter->read<bool>();
      type val          = ctx->ReadAndResolve<type>(is_reg, iter);
      ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
      *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;

    } else if constexpr (base::meta<decltype(std::declval<Inst>().Apply(
                             *ctx))> == base::meta<void>) {
      Inst::ReadFromByteCode(iter).Apply(*ctx);
    } else {
      // TODO: If a stack-frame knew what function was being called, this
      // could be simplified. We could return just a stack frame and call it.
      auto [fn, frame] = Inst::ReadFromByteCode(iter).Apply(*ctx);
      interpretter::CallFn<InstSet>(fn.native(), &frame, {}, ctx);
    }
  };
}

template <typename InstSet, typename... Insts>
constexpr std::array<exec_t, sizeof...(Insts)> MakeExecuteFunctions(
    base::type_list<Insts...>) {
  return {GetInstruction<InstSet, Insts>()...};
}

template <typename... Insts>
absl::flat_hash_map<base::MetaValue, cmd_index_t> MakeInstructionIndexMapping(
    base::type_list<Insts...>) {
  cmd_index_t index = 0;
  return {{base::meta<LoadInstruction>, LoadInstruction::kIndex},
          {base::meta<Insts>, index++}...};
}

template <typename... Insts>
void Debug(base::type_list<Insts...>) {
  (std::cerr << ...
             << absl::StrFormat("Instruction:\n  Name: %s\n  Value: %d\n\n",
                                typeid(Insts).name(),
                                base::MetaValue(base::meta<Insts>).get()));
};

template <typename IndexSeq, typename... Instructions>
struct IndexInImpl;

template <size_t... Ns, typename... Instructions>
struct IndexInImpl<std::index_sequence<Ns...>, Instructions...> {
  static_assert(sizeof...(Instructions) == sizeof...(Ns));
  template <typename T>
  static constexpr int Of() {
    return (((base::meta<T> == base::meta<Instructions>) ? Ns : 0) + ...);
  }
};

template <typename InstructionList>
struct IndexIn;

template <typename... Instructions>
struct IndexIn<base::type_list<Instructions...>>
    : IndexInImpl<std::make_index_sequence<sizeof...(Instructions)>,
                  Instructions...> {};

}  // namespace internal_instructions

struct RequiredCapabilities;

template <typename Capabilities, typename... InstructionsOrSets>
struct InstructionSet;

template <typename... Capabilities, typename... InstructionsOrSets>
struct InstructionSet<RequiredCapabilities(Capabilities...),
                      InstructionsOrSets...> {
  using instructions_t = decltype(internal_instructions::ExpandedInstructions(
      static_cast<base::type_list<InstructionsOrSets...>>(nullptr),
      static_cast<base::type_list<>>(nullptr)));
  static constexpr std::array Execute =
      internal_instructions::MakeExecuteFunctions<InstructionSet<
          RequiredCapabilities(Capabilities...), InstructionsOrSets...>>(
          static_cast<instructions_t>(nullptr));

  static void Debug() {
    internal_instructions::Debug(static_cast<instructions_t>(nullptr));
  }

  template <typename T>
  static constexpr cmd_index_t Index() {
    return internal_instructions::IndexIn<instructions_t>::template Of<T>();
  }

  static cmd_index_t Index(Inst const &inst) {
    DEBUG_LOG()
    (typeid(Inst).name(), ": ", inst.rtti().get(), " ", inst.rtti().name());
    DEBUG_LOG()(typeid(Inst).name(), " => ", index_mapping_.at(inst.rtti()));
    return index_mapping_.at(inst.rtti());
  }

 private:
  static absl::flat_hash_map<base::MetaValue, cmd_index_t> const index_mapping_;
};

template <typename... Capabilities, typename... InstructionsOrSets>
absl::flat_hash_map<base::MetaValue, cmd_index_t> const
    InstructionSet<RequiredCapabilities(Capabilities...),
                   InstructionsOrSets...>::index_mapping_ =
        internal_instructions::MakeInstructionIndexMapping(
            static_cast<instructions_t>(nullptr));

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_SET_H
