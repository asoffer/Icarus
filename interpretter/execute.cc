#include "interpretter/execute.h"

#include <vector>

#include "absl/random/random.h"
#include "base/meta.h"
#include "interpretter/architecture.h"
#include "interpretter/foreign.h"
#include "ir/block_def.h"
#include "ir/instructions.h"
#include "ir/jump.h"
#include "ir/read_only_data.h"
#include "ir/scope_def.h"
#include "type/opaque.h"

namespace interpretter {
namespace {

// Maximum size of any primitive type we may write
constexpr size_t kMaxSize = 16;

constexpr uint8_t ReverseByte(uint8_t byte) {
  byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
  byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
  byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
  return byte;
}

template <typename SizeType, typename Iter>
std::vector<bool> ReadBits(Iter *iter) {
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
auto Deserialize(Iter *iter, Fn &&fn) {
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

type::Function const *GetType(ir::AnyFunc f) {
  return f.is_fn() ? f.func()->type_
                   : &f.foreign().type()->as<type::Function>();
}

template <typename T>
T ReadAndResolve(bool is_reg, base::untyped_buffer::const_iterator *iter,
                 ExecutionContext *ctx) {
  if (is_reg) {
    ir::Reg r = iter->read<ir::Reg>();
    return ctx->resolve<T>(r);
  } else {
    return iter->read<T>();
  }
}

void CallFunction(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
                  absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  ASSERT(fn != nullptr);
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
  if (fn->work_item and *fn->work_item) {
    (std::move(*fn->work_item))();
    fn->WriteByteCode();
  }

  ctx->call_stack_.emplace_back(fn, arguments, &ctx->stack_);
  ctx->ExecuteBlocks(ret_slots);
  ctx->call_stack_.pop_back();
}

}  // namespace

void Execute(ir::AnyFunc fn, base::untyped_buffer const &arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  if (fn.is_fn()) {
    CallFunction(fn.func(), arguments, ret_slots, ctx);
  } else {
    CallForeignFn(fn.foreign(), arguments, ret_slots, &ctx->stack_);
  }
}

template <typename BinInst,
          typename std::enable_if_t<
              std::is_base_of_v<ir::BinaryInstruction<typename BinInst::type>,
                                BinInst>,
              int> = 0>
void ExecuteInstruction(base::untyped_buffer::const_iterator *iter,
                        ExecutionContext *ctx,
                        absl::Span<ir::Addr const> ret_slots) {
  using ctrl_t = typename BinInst::control_bits;
  using type   = typename BinInst::type;
  ctrl_t ctrl  = iter->read<ctrl_t>();
  type lhs     = ReadAndResolve<type>(ctrl.lhs_is_reg, iter, ctx);
  type rhs     = ReadAndResolve<type>(ctrl.rhs_is_reg, iter, ctx);
  auto result  = BinInst::Apply(lhs, rhs);
  ctx->current_frame().regs_.set(iter->read<ir::Reg>(), result);
}

template <
    typename UnInst,
    typename std::enable_if_t<
        std::is_base_of_v<ir::UnaryInstruction<typename UnInst::type>, UnInst>,
        int> = 0>
void ExecuteInstruction(base::untyped_buffer::const_iterator *iter,
                        ExecutionContext *ctx,
                        absl::Span<ir::Addr const> ret_slots) {
  using type  = typename UnInst::type;
  bool is_reg = iter->read<bool>();
  auto result = UnInst::Apply(ReadAndResolve<type>(is_reg, iter, ctx));
  ctx->current_frame().regs_.set(iter->read<ir::Reg>(), result);
}

template <typename VarInst,
          typename std::enable_if_t<
              std::is_base_of_v<ir::VariadicInstruction<typename VarInst::type>,
                                VarInst>,
              int> = 0>
void ExecuteInstruction(base::untyped_buffer::const_iterator *iter,
                        ExecutionContext *ctx,
                        absl::Span<ir::Addr const> ret_slots) {
  using type = typename VarInst::type;

  auto vals = Deserialize<uint16_t, type>(
      iter, [ctx](ir::Reg reg) { return ctx->resolve<type>(reg); });
  ctx->current_frame().regs_.set(iter->read<ir::Reg>(),
                                 VarInst::Apply(std::move(vals)));
}

template <typename Inst>
void ExecuteAdHocInstruction(base::untyped_buffer::const_iterator *iter,
                             ExecutionContext *ctx,
                             absl::Span<ir::Addr const> ret_slots) {
  if constexpr (std::is_same_v<Inst, ir::EnumerationInstruction>) {
    using enum_t             = uint64_t;
    bool is_enum_not_flags   = iter->read<bool>();
    uint16_t num_enumerators = iter->read<uint16_t>();
    uint16_t num_specified   = iter->read<uint16_t>();
    module::BasicModule *mod = iter->read<module::BasicModule *>();
    std::vector<std::pair<std::string_view, std::optional<enum_t>>> enumerators;
    enumerators.reserve(num_enumerators);
    for (uint16_t i = 0; i < num_enumerators; ++i) {
      enumerators.emplace_back(iter->read<std::string_view>(), std::nullopt);
    }

    absl::flat_hash_set<enum_t> vals;

    for (uint16_t i = 0; i < num_specified; ++i) {
      uint64_t index        = iter->read<uint64_t>();
      auto b                = iter->read<bool>();
      enum_t val            = ReadAndResolve<enum_t>(b, iter, ctx);
      enumerators[i].second = val;
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
      result = new type::Enum(mod, std::move(mapping));
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
      result = new type::Flags(mod, std::move(mapping));
    }

    ctx->current_frame().regs_.set(iter->read<ir::Reg>(), result);

  } else if constexpr (std::is_same_v<Inst, ir::OpaqueTypeInstruction>) {
    module::BasicModule const *mod = iter->read<module::BasicModule const *>();
    ctx->current_frame().regs_.set(iter->read<ir::Reg>(),
                                   new type::Opaque(mod));

  } else if constexpr (std::is_same_v<Inst, ir::ArrowInstruction>) {
    std::vector<type::Type const *> ins =
        Deserialize<uint16_t, type::Type const *>(iter, [ctx](ir::Reg reg) {
          return ctx->resolve<type::Type const *>(reg);
        });
    std::vector<type::Type const *> outs =
        Deserialize<uint16_t, type::Type const *>(iter, [ctx](ir::Reg reg) {
          return ctx->resolve<type::Type const *>(reg);
        });

    core::FnParams<type::Type const*> in_params;
    in_params.reserve(ins.size());
    for (auto *t : ins) { in_params.append(core::AnonymousParam(t)); }

    ctx->current_frame().regs_.set(
        iter->read<ir::Reg>(),
        type::Func(std::move(in_params), std::move(outs)));
  } else if constexpr (std::is_same_v<Inst, ir::PrintInstruction<bool>>) {
    bool is_reg = iter->read<bool>();
    std::cerr << (ReadAndResolve<bool>(is_reg, iter, ctx) ? "true" : "false");
  } else if constexpr (std::is_same_v<Inst, ir::PrintInstruction<uint8_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<int8_t>>) {
    using type  = typename Inst::type;
    bool is_reg = iter->read<bool>();
    // Cast to a larger type to ensure we print as an integer rather than a
    // character.
    std::cerr << static_cast<int16_t>(ReadAndResolve<type>(is_reg, iter, ctx));
  } else if constexpr (std::is_same_v<Inst, ir::PrintInstruction<uint16_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<int16_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<uint32_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<int32_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<uint64_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<int64_t>> or
                       std::is_same_v<Inst, ir::PrintInstruction<float>> or
                       std::is_same_v<Inst, ir::PrintInstruction<double>> or
                       std::is_same_v<Inst, ir::PrintInstruction<ir::Addr>> or
                       std::is_same_v<
                           Inst, ir::PrintInstruction<type::Type const *>> or
                       std::is_same_v<Inst,
                                      ir::PrintInstruction<std::string_view>>) {
    using type  = typename Inst::type;
    bool is_reg = iter->read<bool>();
    if constexpr (std::is_same_v<type, ::type::Type const *>) {
      std::cerr << ReadAndResolve<type>(is_reg, iter, ctx)->to_string();

    } else if constexpr (std::is_same_v<type, ir::Addr>) {
      std::cerr << ReadAndResolve<type>(is_reg, iter, ctx).to_string();

    } else {
      std::cerr << ReadAndResolve<type>(is_reg, iter, ctx);
    }
  } else if constexpr (std::is_same_v<Inst, ir::PrintEnumInstruction>) {
    bool is_reg = iter->read<bool>().get();
    auto val    = ReadAndResolve<ir::EnumVal>(is_reg, iter, ctx);
    std::optional<std::string_view> name =
        iter->read<type::Enum const *>().get()->name(val);
    std::cerr << name.value_or(absl::StrCat(val.value));
  } else if constexpr (std::is_same_v<Inst, ir::PrintFlagsInstruction>) {
    bool is_reg      = iter->read<bool>().get();
    auto val         = ReadAndResolve<ir::FlagsVal>(is_reg, iter, ctx);
    auto numeric_val = val.value;
    std::vector<std::string> vals;
    type::Flags const *flags_type = iter->read<type::Flags const *>();

    while (numeric_val != 0) {
      size_t mask = (numeric_val & ((~numeric_val) + 1));
      numeric_val -= mask;

      std::optional<std::string_view> name =
          flags_type->name(ir::FlagsVal(mask));
      vals.emplace_back(name.has_value() ? std::string{*name}
                                         : std::to_string(mask));
    }

    if (vals.empty()) {
      std::cerr << "(empty)";
    } else {
      auto iter = vals.begin();
      std::cerr << *iter++;
      while (iter != vals.end()) { std::cerr << " | " << *iter++; }
    }
  } else if constexpr (ir::internal::kStoreInstructionRange.contains(
                           Inst::kIndex)) {
    using type    = typename Inst::type;
    auto ctrl     = iter->read<typename Inst::control_bits>().get();
    type val      = ReadAndResolve<type>(ctrl.value_is_reg, iter, ctx);
    ir::Addr addr = ReadAndResolve<ir::Addr>(ctrl.location_is_reg, iter, ctx);
    switch (addr.kind()) {
      case ir::Addr::Kind::Stack: ctx->stack_.set(addr.stack(), val); break;
      case ir::Addr::Kind::ReadOnly:
        NOT_YET(
            "Storing into read-only data seems suspect. Is it just for "
            "initialization?");
        break;
      case ir::Addr::Kind::Heap:
        *ASSERT_NOT_NULL(static_cast<type *>(addr.heap())) = val;
    }
  } else if constexpr (ir::internal::kPhiInstructionRange.contains(
                           Inst::kIndex)) {
    uint16_t num   = iter->read<uint16_t>();
    uint64_t index = std::numeric_limits<uint64_t>::max();
    for (uint16_t i = 0; i < num; ++i) {
      if (ctx->current_frame().prev_index_ == iter->read<uintptr_t>()) {
        index = i;
      }
    }
    ASSERT(index != std::numeric_limits<uint64_t>::max());

    using type                = typename Inst::type;
    std::vector<type> results = Deserialize<uint16_t, type>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<type>(reg); });
    ctx->current_frame().regs_.set(iter->read<ir::Reg>(), type{results[index]});
    DEBUG_LOG("phi-instruction")(results[index]);
  } else if constexpr (std::is_same_v<Inst, ir::TypeManipulationInstruction>) {
    ir::AnyFunc f;
    base::untyped_buffer call_buf(sizeof(ir::Addr));
    auto kind = iter->read<ir::TypeManipulationInstruction::Kind>();
    type::Type const *t =
        ASSERT_NOT_NULL(iter->read<type::Type const *>().get());
    switch (kind) {
      case ir::TypeManipulationInstruction::Kind::Init: {
        call_buf.append(ctx->resolve<ir::Addr>(iter->read<ir::Reg>().get()));
        if (auto *s = t->if_as<type::Struct>()) {
          f = s->init_func_.get();
        } else if (auto *tup = t->if_as<type::Tuple>()) {
          f = tup->init_func_.get();
        } else if (auto *a = t->if_as<type::Array>()) {
          f = a->init_func_.get();
        } else {
          NOT_YET();
        }
      } break;
      case ir::TypeManipulationInstruction::Kind::Destroy: {
        call_buf.append(ctx->resolve<ir::Addr>(iter->read<ir::Reg>().get()));

        if (auto *s = t->if_as<type::Struct>()) {
          f = s->destroy_func_.get();
        } else if (auto *tup = t->if_as<type::Tuple>()) {
          f = tup->destroy_func_.get();
        } else if (auto *a = t->if_as<type::Array>()) {
          f = a->destroy_func_.get();
        } else {
          NOT_YET();
        }
      } break;
      case ir::TypeManipulationInstruction::Kind::Move: {
        auto from   = ctx->resolve<ir::Addr>(iter->read<ir::Reg>().get());
        bool is_reg = iter->read<bool>();
        auto to     = ReadAndResolve<ir::Addr>(is_reg, iter, ctx);
        call_buf    = base::untyped_buffer::MakeFull(kMaxSize * 2);
        call_buf.set(0, from);
        call_buf.set(kMaxSize, to);

        if (auto *s = t->if_as<type::Struct>()) {
          f = s->move_assign_func_.get();
        } else if (auto *tup = t->if_as<type::Tuple>()) {
          f = tup->move_assign_func_.get();
        } else if (auto *a = t->if_as<type::Array>()) {
          f = a->move_assign_func_.get();
        } else {
          NOT_YET();
        }
      } break;
      case ir::TypeManipulationInstruction::Kind::Copy: {
        auto from   = ctx->resolve<ir::Addr>(iter->read<ir::Reg>().get());
        bool is_reg = iter->read<bool>();
        auto to     = ReadAndResolve<ir::Addr>(is_reg, iter, ctx);
        call_buf    = base::untyped_buffer::MakeFull(kMaxSize * 2);
        call_buf.set(0, from);
        call_buf.set(kMaxSize, to);

        if (auto *s = t->if_as<type::Struct>()) {
          f = s->copy_assign_func_.get();
        } else if (auto *tup = t->if_as<type::Tuple>()) {
          f = tup->copy_assign_func_.get();
        } else if (auto *a = t->if_as<type::Array>()) {
          f = a->copy_assign_func_.get();
        } else {
          NOT_YET();
        }
      } break;
    }

    Execute(f, call_buf, {}, ctx);
  } else if constexpr (std::is_same_v<Inst, ir::DebugIrInstruction>) {
    std::cerr << *ctx->current_frame().fn_;

  } else if constexpr (ir::IsSetReturnInstruction<Inst>) {
    using type = typename Inst::type;
    uint16_t n = iter->read<uint16_t>();
    ASSERT(ret_slots.size() > n);
    ir::Addr ret_slot = ret_slots[n];
    bool is_reg       = iter->read<bool>();
    type val          = ReadAndResolve<type>(is_reg, iter, ctx);
    ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
    *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;

  } else if constexpr (std::is_same_v<Inst, ir::GetReturnInstruction>) {
    uint16_t index = iter->read<uint16_t>();
    ctx->current_frame().regs_.set(iter->read<ir::Reg>(), ret_slots[index]);

  } else if constexpr (std::is_same_v<Inst, ir::MakeScopeInstruction>) {
    ir::ScopeDef *scope_def = iter->read<ir::ScopeDef *>();

    scope_def->inits_ = Deserialize<uint16_t, ir::Jump *>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump *>(reg); });
    scope_def->dones_ = ir::OverloadSet(Deserialize<uint16_t, ir::AnyFunc>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::AnyFunc>(reg); }));

    uint16_t num_blocks = iter->read<uint16_t>();
    for (uint16_t i = 0; i < num_blocks; ++i) {
      std::string_view name = iter->read<std::string_view>();
      ir::BlockDef *block   = iter->read<ir::BlockDef *>();
      scope_def->blocks_.emplace(name, block);
    }

    ctx->current_frame().regs_.set(iter->read<ir::Reg>(), scope_def);

  } else if constexpr (std::is_same_v<Inst, ir::MakeBlockInstruction>) {
    ir::BlockDef *block_def = iter->read<ir::BlockDef *>();
    block_def->before_ = ir::OverloadSet(Deserialize<uint16_t, ir::AnyFunc>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::AnyFunc>(reg); }));
    block_def->after_  = Deserialize<uint16_t, ir::Jump *>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump *>(reg); });
    ctx->current_frame().regs_.set(iter->read<ir::Reg>(), block_def);

  } else if constexpr (std::is_same_v<Inst, ir::StructInstruction>) {
    uint16_t num            = iter->read<uint16_t>();
    ast::Scope const *scope = iter->read<ast::Scope const *>();

    std::vector<type::Struct::Field> fields;
    fields.reserve(num);

    for (uint16_t i = 0; i < num; ++i) {
      auto &field = fields.emplace_back();
      field.name  = iter->read<std::string_view>();
    }

    std::vector<type::Type const *> types =
        Deserialize<uint16_t, type::Type const *>(iter, [&](ir::Reg reg) {
          return ctx->resolve<type::Type const *>(reg);
        });
    for (uint16_t i = 0; i < num; ++i) { fields[i].type = types[i]; }

    ctx->current_frame().regs_.set(iter->read<ir::Reg>(),
                                   new type::Struct(scope, fields));

  } else if constexpr (std::is_same_v<Inst, ir::ArrayInstruction>) {
    using length_t = ir::ArrayInstruction::length_t;
    auto ctrl_bits = iter->read<ir::ArrayInstruction::control_bits>().get();
    auto len = ReadAndResolve<length_t>(ctrl_bits.length_is_reg, iter, ctx);
    auto data_type =
        ReadAndResolve<type::Type const *>(ctrl_bits.type_is_reg, iter, ctx);

    ctx->current_frame().regs_.set(iter->read<ir::Reg>(),
                                   type::Arr(len, data_type));
  } else if constexpr (std::is_same_v<Inst, ir::CallInstruction>) {
    bool fn_is_reg = iter->read<bool>();
    ir::AnyFunc f  = ReadAndResolve<ir::AnyFunc>(fn_is_reg, iter, ctx);
    iter->read<core::Bytes>().get();

    std::vector<bool> is_reg_bits = ReadBits<uint16_t>(iter);

    type::Function const *fn_type = GetType(f);
    DEBUG_LOG("call")(f, ": ", fn_type->to_string());
    DEBUG_LOG("call")(is_reg_bits);

    // TODO you probably want interpretter::Arguments or something.
    auto call_buf =
        base::untyped_buffer::MakeFull(is_reg_bits.size() * kMaxSize);
    ASSERT(fn_type->input().size() == is_reg_bits.size());
    for (size_t i = 0; i < is_reg_bits.size(); ++i) {
      type::Type const *t = fn_type->input().at(i).value;
      if (is_reg_bits[i]) {
        ir::Reg reg = iter->read<ir::Reg>();

        if (t->is_big()) { t = type::Ptr(t); }
        ctx->MemCpyRegisterBytes(/*    dst = */ call_buf.raw(i * kMaxSize),
                                 /*    src = */ reg,
                                 /* length = */ kMaxSize);
      } else if (t->is_big()) {
        NOT_YET();
      } else {
        std::memcpy(call_buf.raw(i * kMaxSize), iter->raw(), kMaxSize);
        iter->skip(t->bytes(kArchitecture).value());
      }
    }

    uint16_t num_rets = iter->read<uint16_t>();

    std::vector<ir::Addr> return_slots;
    return_slots.reserve(num_rets);
    ASSERT(fn_type->output().size() == num_rets);
    for (uint16_t i = 0; i < num_rets; ++i) {
      ir::Reg reg = iter->read<ir::Reg>();
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the interpretter.
      ir::Addr addr = (fn_type->output()[i]->is_big())
                          ? ctx->resolve<ir::Addr>(reg)
                          : ir::Addr::Heap(ctx->current_frame().regs_.raw(reg));

      DEBUG_LOG("call")("Ret addr = ", addr);
      return_slots.push_back(addr);
    }

    Execute(f, call_buf, return_slots, ctx);
  } else if constexpr (std::is_same_v<Inst, ir::LoadSymbolInstruction>) {
    std::string_view name  = iter->read<std::string_view>();
    type::Type const *type = iter->read<type::Type const *>();
    ir::Reg reg            = iter->read<ir::Reg>();

    if (auto *fn_type = type->if_as<type::Function>()) {
      void (*sym)() = LoadFunctionSymbol(name);
      ctx->current_frame().regs_.set(reg,
                                     ir::AnyFunc{ir::ForeignFn(sym, fn_type)});
    } else if (type->is<type::Pointer>()) {
      void *sym = LoadDataSymbol(name);
      ctx->current_frame().regs_.set(
          reg, ir::Addr::Heap(*static_cast<void **>(sym)));
    } else {
      NOT_YET(type->to_string());
    }

  } else if constexpr (std::is_same_v<Inst, ir::TypeInfoInstruction>) {
    uint8_t ctrl_bits = iter->read<uint8_t>();
    auto type = ReadAndResolve<type::Type const *>(ctrl_bits & 0x01, iter, ctx);
    ir::Reg reg = iter->read<ir::Reg>();

    if (ctrl_bits & 0x02) {
      ctx->current_frame().regs_.set(reg, type->alignment(kArchitecture));

    } else {
      ctx->current_frame().regs_.set(reg, type->bytes(kArchitecture));
    }

  } else if constexpr (std::is_same_v<Inst, ir::StructIndexInstruction> or
                       std::is_same_v<Inst, ir::TupleIndexInstruction> or
                       std::is_same_v<Inst, ir::PtrIncrInstruction>) {
    using ctrl_bits_t = typename Inst::control_bits;
    auto ctrl_bits    = iter->read<ctrl_bits_t>().get();
    auto const *type  = iter->read<typename Inst::type>().get();

    ir::Addr addr = ReadAndResolve<ir::Addr>(ctrl_bits.reg_addr, iter, ctx);
    int64_t index = ReadAndResolve<int64_t>(ctrl_bits.reg_index, iter, ctx);
    ir::Reg reg   = iter->read<ir::Reg>();

    if constexpr (std::is_same_v<Inst, ir::PtrIncrInstruction>) {
      core::Bytes offset =
          core::FwdAlign(type->pointee->bytes(kArchitecture),
                         type->pointee->alignment(kArchitecture)) *
          index;
      ctx->current_frame().regs_.set(reg, addr + offset);
    } else {
      ctx->current_frame().regs_.set(reg,
                                     addr + type->offset(index, kArchitecture));
    }
  } else if constexpr (std::is_same_v<Inst, ir::ByteViewLengthInstruction>) {
    int64_t length =
        ctx->resolve<std::string_view>(iter->read<ir::Reg>().get()).size();
    ir::Reg result = iter->read<ir::Reg>();
    ctx->current_frame().regs_.set(result, length);
  } else if constexpr (std::is_same_v<Inst, ir::ByteViewDataInstruction>) {
    auto data_addr = ir::Addr::Heap(const_cast<char *>(
        ctx->resolve<std::string_view>(iter->read<ir::Reg>().get()).data()));
    ir::Reg result = iter->read<ir::Reg>();
    ctx->current_frame().regs_.set(result, data_addr);
  } else if constexpr (std::is_same_v<Inst, ir::VariantAccessInstruction>) {
    bool get_val = iter->read<bool>();
    bool is_reg  = iter->read<bool>();

    ir::Addr addr = ReadAndResolve<ir::Addr>(is_reg, iter, ctx);
    DEBUG_LOG("variant")(addr);
    if (get_val) { addr += type::Type_->bytes(kArchitecture); }

    ir::Reg reg = iter->read<ir::Reg>();
    DEBUG_LOG("variant")(reg);
    ctx->current_frame().regs_.set(reg, addr);

  } else if constexpr (ir::IsCastInstruction<Inst>) {
    using to_type   = typename Inst::to_type;
    using from_type = typename Inst::from_type;
    bool is_reg     = iter->read<bool>();
    from_type value = ReadAndResolve<from_type>(is_reg, iter, ctx);
    ir::Reg reg     = iter->read<ir::Reg>();
    ctx->current_frame().regs_.set(reg, static_cast<to_type>(value));

  } else {
    static_assert(base::always_false<Inst>());
  }
}

void ExecutionContext::MemCpyRegisterBytes(void *dst, ir::Reg reg,
                                           size_t length) {
  std::memcpy(dst, call_stack_.back().regs_.raw(reg), length);
}

// Note: The ordering here is very important
inline constexpr auto kNullInstruction =
    static_cast<void (*)(base::untyped_buffer::const_iterator *,
                         ExecutionContext *, absl::Span<ir::Addr const>)>(
        nullptr);

auto kInstructions = std::array{
    ExecuteInstruction<ir::AddInstruction<uint8_t>>,
    ExecuteInstruction<ir::AddInstruction<int8_t>>,
    ExecuteInstruction<ir::AddInstruction<uint16_t>>,
    ExecuteInstruction<ir::AddInstruction<int16_t>>,
    ExecuteInstruction<ir::AddInstruction<uint32_t>>,
    ExecuteInstruction<ir::AddInstruction<int32_t>>,
    ExecuteInstruction<ir::AddInstruction<uint64_t>>,
    ExecuteInstruction<ir::AddInstruction<int64_t>>,
    ExecuteInstruction<ir::AddInstruction<float>>,
    ExecuteInstruction<ir::AddInstruction<double>>,

    ExecuteInstruction<ir::SubInstruction<uint8_t>>,
    ExecuteInstruction<ir::SubInstruction<int8_t>>,
    ExecuteInstruction<ir::SubInstruction<uint16_t>>,
    ExecuteInstruction<ir::SubInstruction<int16_t>>,
    ExecuteInstruction<ir::SubInstruction<uint32_t>>,
    ExecuteInstruction<ir::SubInstruction<int32_t>>,
    ExecuteInstruction<ir::SubInstruction<uint64_t>>,
    ExecuteInstruction<ir::SubInstruction<int64_t>>,
    ExecuteInstruction<ir::SubInstruction<float>>,
    ExecuteInstruction<ir::SubInstruction<double>>,

    ExecuteInstruction<ir::MulInstruction<uint8_t>>,
    ExecuteInstruction<ir::MulInstruction<int8_t>>,
    ExecuteInstruction<ir::MulInstruction<uint16_t>>,
    ExecuteInstruction<ir::MulInstruction<int16_t>>,
    ExecuteInstruction<ir::MulInstruction<uint32_t>>,
    ExecuteInstruction<ir::MulInstruction<int32_t>>,
    ExecuteInstruction<ir::MulInstruction<uint64_t>>,
    ExecuteInstruction<ir::MulInstruction<int64_t>>,
    ExecuteInstruction<ir::MulInstruction<float>>,
    ExecuteInstruction<ir::MulInstruction<double>>,

    ExecuteInstruction<ir::DivInstruction<uint8_t>>,
    ExecuteInstruction<ir::DivInstruction<int8_t>>,
    ExecuteInstruction<ir::DivInstruction<uint16_t>>,
    ExecuteInstruction<ir::DivInstruction<int16_t>>,
    ExecuteInstruction<ir::DivInstruction<uint32_t>>,
    ExecuteInstruction<ir::DivInstruction<int32_t>>,
    ExecuteInstruction<ir::DivInstruction<uint64_t>>,
    ExecuteInstruction<ir::DivInstruction<int64_t>>,
    ExecuteInstruction<ir::DivInstruction<float>>,
    ExecuteInstruction<ir::DivInstruction<double>>,

    ExecuteInstruction<ir::ModInstruction<uint8_t>>,
    ExecuteInstruction<ir::ModInstruction<int8_t>>,
    ExecuteInstruction<ir::ModInstruction<uint16_t>>,
    ExecuteInstruction<ir::ModInstruction<int16_t>>,
    ExecuteInstruction<ir::ModInstruction<uint32_t>>,
    ExecuteInstruction<ir::ModInstruction<int32_t>>,
    ExecuteInstruction<ir::ModInstruction<uint64_t>>,
    ExecuteInstruction<ir::ModInstruction<int64_t>>,

    ExecuteInstruction<ir::LtInstruction<uint8_t>>,
    ExecuteInstruction<ir::LtInstruction<int8_t>>,
    ExecuteInstruction<ir::LtInstruction<uint16_t>>,
    ExecuteInstruction<ir::LtInstruction<int16_t>>,
    ExecuteInstruction<ir::LtInstruction<uint32_t>>,
    ExecuteInstruction<ir::LtInstruction<int32_t>>,
    ExecuteInstruction<ir::LtInstruction<uint64_t>>,
    ExecuteInstruction<ir::LtInstruction<int64_t>>,
    ExecuteInstruction<ir::LtInstruction<float>>,
    ExecuteInstruction<ir::LtInstruction<double>>,

    ExecuteInstruction<ir::LeInstruction<uint8_t>>,
    ExecuteInstruction<ir::LeInstruction<int8_t>>,
    ExecuteInstruction<ir::LeInstruction<uint16_t>>,
    ExecuteInstruction<ir::LeInstruction<int16_t>>,
    ExecuteInstruction<ir::LeInstruction<uint32_t>>,
    ExecuteInstruction<ir::LeInstruction<int32_t>>,
    ExecuteInstruction<ir::LeInstruction<uint64_t>>,
    ExecuteInstruction<ir::LeInstruction<int64_t>>,
    ExecuteInstruction<ir::LeInstruction<float>>,
    ExecuteInstruction<ir::LeInstruction<double>>,

    ExecuteInstruction<ir::EqInstruction<uint8_t>>,
    ExecuteInstruction<ir::EqInstruction<int8_t>>,
    ExecuteInstruction<ir::EqInstruction<uint16_t>>,
    ExecuteInstruction<ir::EqInstruction<int16_t>>,
    ExecuteInstruction<ir::EqInstruction<uint32_t>>,
    ExecuteInstruction<ir::EqInstruction<int32_t>>,
    ExecuteInstruction<ir::EqInstruction<uint64_t>>,
    ExecuteInstruction<ir::EqInstruction<int64_t>>,
    ExecuteInstruction<ir::EqInstruction<float>>,
    ExecuteInstruction<ir::EqInstruction<double>>,
    ExecuteInstruction<ir::EqInstruction<type::Type const *>>,
    ExecuteInstruction<ir::EqInstruction<ir::Addr>>,
    ExecuteInstruction<ir::EqInstruction<ir::EnumVal>>,
    ExecuteInstruction<ir::EqInstruction<ir::FlagsVal>>,

    ExecuteInstruction<ir::NeInstruction<uint8_t>>,
    ExecuteInstruction<ir::NeInstruction<int8_t>>,
    ExecuteInstruction<ir::NeInstruction<uint16_t>>,
    ExecuteInstruction<ir::NeInstruction<int16_t>>,
    ExecuteInstruction<ir::NeInstruction<uint32_t>>,
    ExecuteInstruction<ir::NeInstruction<int32_t>>,
    ExecuteInstruction<ir::NeInstruction<uint64_t>>,
    ExecuteInstruction<ir::NeInstruction<int64_t>>,
    ExecuteInstruction<ir::NeInstruction<float>>,
    ExecuteInstruction<ir::NeInstruction<double>>,
    ExecuteInstruction<ir::NeInstruction<type::Type const *>>,
    ExecuteInstruction<ir::NeInstruction<ir::Addr>>,
    ExecuteInstruction<ir::NeInstruction<ir::EnumVal>>,
    ExecuteInstruction<ir::NeInstruction<ir::FlagsVal>>,

    kNullInstruction,
    ExecuteInstruction<ir::NegInstruction<int8_t>>,
    kNullInstruction,
    ExecuteInstruction<ir::NegInstruction<int16_t>>,
    kNullInstruction,
    ExecuteInstruction<ir::NegInstruction<int32_t>>,
    kNullInstruction,
    ExecuteInstruction<ir::NegInstruction<int64_t>>,
    ExecuteInstruction<ir::NegInstruction<float>>,
    ExecuteInstruction<ir::NegInstruction<double>>,

    ExecuteInstruction<ir::RegisterInstruction<uint8_t>>,
    ExecuteInstruction<ir::RegisterInstruction<int8_t>>,
    ExecuteInstruction<ir::RegisterInstruction<uint16_t>>,
    ExecuteInstruction<ir::RegisterInstruction<int16_t>>,
    ExecuteInstruction<ir::RegisterInstruction<uint32_t>>,
    ExecuteInstruction<ir::RegisterInstruction<int32_t>>,
    ExecuteInstruction<ir::RegisterInstruction<uint64_t>>,
    ExecuteInstruction<ir::RegisterInstruction<int64_t>>,
    ExecuteInstruction<ir::RegisterInstruction<float>>,
    ExecuteInstruction<ir::RegisterInstruction<double>>,
    ExecuteInstruction<ir::RegisterInstruction<type::Type const *>>,
    ExecuteInstruction<ir::RegisterInstruction<ir::Addr>>,
    ExecuteInstruction<ir::RegisterInstruction<ir::EnumVal>>,
    ExecuteInstruction<ir::RegisterInstruction<ir::FlagsVal>>,
    ExecuteInstruction<ir::RegisterInstruction<bool>>,

    ExecuteAdHocInstruction<ir::PrintInstruction<uint8_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<int8_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<uint16_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<int16_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<uint32_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<int32_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<uint64_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<int64_t>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<float>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<double>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<type::Type const *>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<ir::Addr>>,
    ExecuteAdHocInstruction<ir::PrintEnumInstruction>,
    ExecuteAdHocInstruction<ir::PrintFlagsInstruction>,
    ExecuteAdHocInstruction<ir::PrintInstruction<bool>>,
    ExecuteAdHocInstruction<ir::PrintInstruction<std::string_view>>,

    ExecuteAdHocInstruction<ir::StoreInstruction<uint8_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<int8_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<uint16_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<int16_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<uint32_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<int32_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<uint64_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<int64_t>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<float>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<double>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<type::Type const *>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<ir::Addr>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<ir::EnumVal>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<ir::FlagsVal>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<bool>>,
    ExecuteAdHocInstruction<ir::StoreInstruction<std::string_view>>,

    ExecuteAdHocInstruction<ir::PhiInstruction<uint8_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<int8_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<uint16_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<int16_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<uint32_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<int32_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<uint64_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<int64_t>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<float>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<double>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<type::Type const *>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<ir::Addr>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<ir::EnumVal>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<ir::FlagsVal>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<bool>>,
    ExecuteAdHocInstruction<ir::PhiInstruction<std::string_view>>,

};

// TODO if these instructions were laid out in a relatively sane order, this could be entirely flat.
absl::flat_hash_map<
    ir::cmd_index_t,
    void (*)(base::untyped_buffer::const_iterator *, ExecutionContext *,
             absl::Span<ir::Addr const>)> const kInstructionMap = {

    {ir::NotInstruction::kIndex, ExecuteInstruction<ir::NotInstruction>},

    {ir::PtrInstruction::kIndex, ExecuteInstruction<ir::PtrInstruction>},
    {ir::BufPtrInstruction::kIndex, ExecuteInstruction<ir::BufPtrInstruction>},
    {ir::TupleInstruction::kIndex, ExecuteInstruction<ir::TupleInstruction>},
    {ir::VariantInstruction::kIndex,
     ExecuteInstruction<ir::VariantInstruction>},
    {ir::EnumerationInstruction::kIndex,
     ExecuteAdHocInstruction<ir::EnumerationInstruction>},
    {ir::OpaqueTypeInstruction::kIndex,
     ExecuteAdHocInstruction<ir::OpaqueTypeInstruction>},
    {ir::ArrowInstruction::kIndex,
     ExecuteAdHocInstruction<ir::ArrowInstruction>},

    {ir::DebugIrInstruction::kIndex,
     ExecuteAdHocInstruction<ir::DebugIrInstruction>},

    {ir::TypeManipulationInstruction::kIndex,
     ExecuteAdHocInstruction<ir::TypeManipulationInstruction>},

    {ir::SetReturnInstruction<bool>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<bool>>},
    {ir::SetReturnInstruction<uint8_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<uint8_t>>},
    {ir::SetReturnInstruction<int8_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<int8_t>>},
    {ir::SetReturnInstruction<uint16_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<uint16_t>>},
    {ir::SetReturnInstruction<int16_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<int16_t>>},
    {ir::SetReturnInstruction<uint32_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<uint32_t>>},
    {ir::SetReturnInstruction<int32_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<int32_t>>},
    {ir::SetReturnInstruction<uint64_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<uint64_t>>},
    {ir::SetReturnInstruction<int64_t>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<int64_t>>},
    {ir::SetReturnInstruction<float>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<float>>},
    {ir::SetReturnInstruction<double>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<double>>},
    {ir::SetReturnInstruction<core::Bytes>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<core::Bytes>>},
    {ir::SetReturnInstruction<core::Alignment>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<core::Alignment>>},
    {ir::SetReturnInstruction<type::Type const *>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<type::Type const *>>},
    {ir::SetReturnInstruction<ir::AnyFunc>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<ir::AnyFunc>>},
    {ir::SetReturnInstruction<ir::ScopeDef const *>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<ir::ScopeDef const *>>},
    {ir::SetReturnInstruction<ir::BlockDef const *>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<ir::BlockDef const *>>},
    {ir::SetReturnInstruction<module::BasicModule const *>::kIndex,
     ExecuteAdHocInstruction<
         ir::SetReturnInstruction<module::BasicModule const *>>},
    {ir::SetReturnInstruction<ir::EnumVal>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<ir::EnumVal>>},
    {ir::SetReturnInstruction<ir::FlagsVal>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<ir::FlagsVal>>},
    {ir::SetReturnInstruction<std::string_view>::kIndex,
     ExecuteAdHocInstruction<ir::SetReturnInstruction<std::string_view>>},
    {ir::GetReturnInstruction::kIndex,
     ExecuteAdHocInstruction<ir::GetReturnInstruction>},
    {ir::MakeScopeInstruction::kIndex,
     ExecuteAdHocInstruction<ir::MakeScopeInstruction>},
    {ir::MakeBlockInstruction::kIndex,
     ExecuteAdHocInstruction<ir::MakeBlockInstruction>},
    {ir::StructInstruction::kIndex,
     ExecuteAdHocInstruction<ir::StructInstruction>},
    {ir::ArrayInstruction::kIndex,
     ExecuteAdHocInstruction<ir::ArrayInstruction>},

    {ir::XorFlagsInstruction::kIndex,
     ExecuteInstruction<ir::XorFlagsInstruction>},
    {ir::OrFlagsInstruction::kIndex,
     ExecuteInstruction<ir::OrFlagsInstruction>},
    {ir::AndFlagsInstruction::kIndex,
     ExecuteInstruction<ir::AndFlagsInstruction>},

    {ir::CallInstruction::kIndex, ExecuteAdHocInstruction<ir::CallInstruction>},

    {ir::LoadSymbolInstruction::kIndex,
     ExecuteAdHocInstruction<ir::LoadSymbolInstruction>},
    {ir::TypeInfoInstruction::kIndex,
     ExecuteAdHocInstruction<ir::TypeInfoInstruction>},
    {ir::StructIndexInstruction::kIndex,
     ExecuteAdHocInstruction<ir::StructIndexInstruction>},
    {ir::TupleIndexInstruction::kIndex,
     ExecuteAdHocInstruction<ir::TupleIndexInstruction>},
    {ir::PtrIncrInstruction::kIndex,
     ExecuteAdHocInstruction<ir::PtrIncrInstruction>},
    {ir::ByteViewLengthInstruction::kIndex,
     ExecuteAdHocInstruction<ir::ByteViewLengthInstruction>},
    {ir::ByteViewDataInstruction::kIndex,
     ExecuteAdHocInstruction<ir::ByteViewDataInstruction>},
    {ir::VariantAccessInstruction::kIndex,
     ExecuteAdHocInstruction<ir::VariantAccessInstruction>},
};

void ExecutionContext::ExecuteBlocks(absl::Span<ir::Addr const> ret_slots) {
  DEBUG_LOG("dbg-buffer")(*current_frame().current_block());
  auto &buffer = current_frame().fn_->byte_code();

  auto iter = buffer.begin();
  while (true) {
    ASSERT(iter < buffer.end());
    ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
    DEBUG_LOG("dbg-buffer")(cmd_index);

    if (cmd_index < ir::internal::kEndRangedInstructions) {
      kInstructions[cmd_index](&iter, this, ret_slots);
      continue;
    }

    switch (cmd_index) {
      case ir::internal::kUncondJumpInstruction: {
        uintptr_t offset = iter.read<uintptr_t>();
        current_frame().MoveTo(offset);
        iter = buffer.begin();
        iter.skip(offset);
      } break;
      case ir::internal::kCondJumpInstruction: {
        ir::Reg r             = iter.read<ir::Reg>();
        uintptr_t true_block  = iter.read<uintptr_t>();
        uintptr_t false_block = iter.read<uintptr_t>();
        uintptr_t offset      = resolve<bool>(r) ? true_block : false_block;
        current_frame().MoveTo(offset);
        iter = buffer.begin();
        iter.skip(offset);
      } break;
      case ir::internal::kReturnInstruction: return;
      case ir::LoadInstruction::kIndex: {
        uint16_t num_bytes = iter.read<uint16_t>();
        ir::Addr addr      = resolve<ir::Addr>(iter.read<ir::Reg>());
        auto result_reg    = iter.read<ir::Reg>().get();
        DEBUG_LOG("load-instruction")(num_bytes, " ", addr, " ", result_reg);
        switch (addr.kind()) {
          case ir::Addr::Kind::Stack: {
            current_frame().regs_.set_raw(result_reg, stack_.raw(addr.stack()),
                                          num_bytes);
          } break;
          case ir::Addr::Kind::ReadOnly:
            current_frame().regs_.set_raw(
                result_reg, ir::ReadOnlyData.raw(addr.rodata()), num_bytes);
            break;
          case ir::Addr::Kind::Heap: {
            current_frame().regs_.set_raw(result_reg, addr.heap(), num_bytes);
          } break;
        }
      } break;

      default: {
        auto it = kInstructionMap.find(cmd_index);
        ASSERT(it != kInstructionMap.end()) << cmd_index;
        it->second(&iter, this, ret_slots);
      } break;
    }
  }
}

}  // namespace interpretter
