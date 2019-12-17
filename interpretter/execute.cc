#include "interpretter/execute.h"

#include <vector>

#include "interpretter/foreign.h"
#include "ir/block_def.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jump.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/cmd/util.h"
#include "ir/jump.h"
#include "ir/scope_def.h"

namespace interpretter {
namespace {

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

template <typename CmdType, typename T>
auto BinaryApply(base::untyped_buffer::const_iterator *iter, bool reg0,
                 bool reg1, ExecutionContext *ctx) {
  using fn_type = typename CmdType::fn_type;
  if constexpr (CmdType::template IsSupported<T>()) {
    ASSERT((reg0 or reg1) == true);
    auto lhs = ReadAndResolve<T>(reg0, iter, ctx);
    auto rhs = ReadAndResolve<T>(reg1, iter, ctx);
    DEBUG_LOG("BinaryApply")("lhs = ", lhs, ", rhs = ", rhs);
    return fn_type{}(lhs, rhs);
  } else {
    return T{};
  }
}

template <typename CmdType, typename T>
auto UnaryApply(base::untyped_buffer::const_iterator *iter, bool reg0,
                ExecutionContext *ctx) {
  using fn_type = typename CmdType::fn_type;
  if constexpr (CmdType::template IsSupported<T>()) {
    auto val = ReadAndResolve<T>(reg0, iter, ctx);
    DEBUG_LOG("UnaryApply")("val = ", val);
    return fn_type{}(val);
  } else {
    return T{};
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
  // TODO log an error if you're asked to execute a function that had an
  // error.
  if (fn->work_item and *fn->work_item) { (std::move(*fn->work_item))(); }

  ctx->call_stack_.emplace_back(fn, arguments, &ctx->stack_);

  while (true) {
    ctx->ExecuteBlock(ret_slots);
    auto const &j = ctx->current_frame().current_block()->jump_;
    switch (j.kind()) {
      case ir::JumpCmd::Kind::Return: ctx->call_stack_.pop_back(); return;
      case ir::JumpCmd::Kind::Uncond:
        ctx->current_frame().MoveTo(j.UncondTarget());
        break;
      case ir::JumpCmd::Kind::Cond:
        ctx->current_frame().MoveTo(
            j.CondTarget(ctx->resolve<bool>(j.CondReg())));
        break;
      default: UNREACHABLE();
    }
  }
}

template <typename CmdType>
void ExecuteCmd(base::untyped_buffer::const_iterator *iter,
                absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  ICARUS_DEBUG_ONLY(auto iter_copy = *iter;)
  DEBUG_LOG("cmd")(CmdType::DebugString(&iter_copy));
  auto &frame = ctx->current_frame();
  if constexpr (std::is_same_v<CmdType, ir::PrintCmd>) {
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();
    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      T val   = ReadAndResolve<T>(ctrl.reg, iter, ctx);
      if constexpr (std::is_same_v<T, bool>) {
        std::cerr << (val ? "true" : "false");
      } else if constexpr (std::is_same_v<T, uint8_t>) {
        std::cerr << static_cast<unsigned int>(val);
      } else if constexpr (std::is_same_v<T, int8_t>) {
        std::cerr << static_cast<int>(val);
      } else if constexpr (std::is_same_v<T, type::Type const *>) {
        std::cerr << val->to_string();
      } else if constexpr (std::is_same_v<T, ir::Addr>) {
        std::cerr << val.to_string();
      } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
        std::optional<std::string_view> name =
            static_cast<type::Enum const *>(iter->read<type::Enum const *>())
                ->name(val);
        std::cerr << (name.has_value() ? *name : absl::StrCat(val.value));
      } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
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
      } else {
        std::cerr << val;
      }
    });

  } else if constexpr (std::is_same_v<CmdType, ir::NegCmd> or
                       std::is_same_v<CmdType, ir::NotCmd> or
                       std::is_same_v<CmdType, ir::PtrCmd> or
                       std::is_same_v<CmdType, ir::BufPtrCmd> or
                       std::is_same_v<CmdType, ir::RegisterCmd>) {
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();
    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type  = typename std::decay_t<decltype(tag)>::type;
      auto result = UnaryApply<CmdType, type>(iter, ctrl.reg0, ctx);
      frame.regs_.set(iter->read<ir::Reg>(), result);
    });

  } else if constexpr (std::is_same_v<CmdType, ir::AddCmd> or
                       std::is_same_v<CmdType, ir::SubCmd> or
                       std::is_same_v<CmdType, ir::MulCmd> or
                       std::is_same_v<CmdType, ir::DivCmd> or
                       std::is_same_v<CmdType, ir::ModCmd> or
                       std::is_same_v<CmdType, ir::LtCmd> or
                       std::is_same_v<CmdType, ir::LeCmd> or
                       std::is_same_v<CmdType, ir::EqCmd> or
                       std::is_same_v<CmdType, ir::NeCmd> or
                       std::is_same_v<CmdType, ir::GeCmd> or
                       std::is_same_v<CmdType, ir::GtCmd>) {
    DEBUG_LOG("BinaryApply")(typeid(CmdType).name());
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();

    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type  = typename std::decay_t<decltype(tag)>::type;
      auto result = BinaryApply<CmdType, type>(iter, ctrl.reg0, ctrl.reg1, ctx);
      frame.regs_.set(iter->read<ir::Reg>(), result);
    });

  } else if constexpr (std::is_same_v<CmdType, ir::VariantCmd> or
                       std::is_same_v<CmdType, ir::TupleCmd>) {
    std::vector<type::Type const *> vals =
        ir::internal::Deserialize<uint16_t, type::Type const *>(
            iter, [ctx](ir::Reg reg) {
              return ctx->resolve<type::Type const *>(reg);
            });

    frame.regs_.set(iter->read<ir::Reg>(), CmdType::fn_ptr(std::move(vals)));

  } else if constexpr (std::is_same_v<CmdType, ir::StoreCmd>) {
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();
    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T       = typename std::decay_t<decltype(tag)>::type;
      T val         = ReadAndResolve<T>(ctrl.reg, iter, ctx);
      ir::Addr addr = ReadAndResolve<ir::Addr>(ctrl.reg_addr, iter, ctx);
      static_assert(not std::is_same_v<T, void *>,
                    "Not handling addresses yet");
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: ctx->stack_.set(addr.as_stack, val); break;
        case ir::Addr::Kind::ReadOnly:
          NOT_YET(
              "Storing into read-only data seems suspect. Is it just for "
              "initialization?");
          break;
        case ir::Addr::Kind::Heap:
          *ASSERT_NOT_NULL(static_cast<T *>(addr.as_heap)) = val;
      }
    });

  } else if constexpr (std::is_same_v<CmdType, ir::LoadCmd>) {
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();
    ir::Addr addr      = ReadAndResolve<ir::Addr>(ctrl.reg, iter, ctx);
    ir::Reg result_reg = iter->read<ir::Reg>();
    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: {
          DEBUG_LOG("LoadCmd")
          ("Loading ", ctx->stack_.get<type>(addr.as_stack), " into ",
           result_reg);
          frame.regs_.set(result_reg, ctx->stack_.get<type>(addr.as_stack));
        } break;
        case ir::Addr::Kind::ReadOnly: NOT_YET(); break;
        case ir::Addr::Kind::Heap: {
          DEBUG_LOG("LoadCmd")
          ("Loading ", *static_cast<type *>(addr.as_heap), " into ",
           result_reg);
          frame.regs_.set(result_reg, *static_cast<type *>(addr.as_heap));
        }
      }
    });

  } else if constexpr (std::is_same_v<CmdType, ir::ArrowCmd>) {
    std::vector<type::Type const *> ins =
        ir::internal::Deserialize<uint16_t, type::Type const *>(
            iter, [ctx](ir::Reg reg) {
              return ctx->resolve<type::Type const *>(reg);
            });
    std::vector<type::Type const *> outs =
        ir::internal::Deserialize<uint16_t, type::Type const *>(
            iter, [ctx](ir::Reg reg) {
              return ctx->resolve<type::Type const *>(reg);
            });

    frame.regs_.set(iter->read<ir::Reg>(),
                    type::Func(std::move(ins), std::move(outs)));

  } else if constexpr (std::is_same_v<CmdType, ir::CallCmd>) {
    bool fn_is_reg                = iter->read<bool>();
    std::vector<bool> is_reg_bits = ir::internal::ReadBits<uint16_t>(iter);

    ir::AnyFunc f = ReadAndResolve<ir::AnyFunc>(fn_is_reg, iter, ctx);
    type::Function const *fn_type = GetType(f);
    DEBUG_LOG("call")(f, ": ", fn_type->to_string());
    DEBUG_LOG("call")(is_reg_bits);

    iter->read<core::Bytes>();

    base::untyped_buffer call_buf;
    ASSERT(fn_type->input.size() == is_reg_bits.size());
    for (size_t i = 0; i < is_reg_bits.size(); ++i) {
      type::Type const *t = fn_type->input[i];
      if (is_reg_bits[i]) {
        ir::Reg reg = iter->read<ir::Reg>();
        ir::PrimitiveDispatch(ir::PrimitiveIndex(t), [&](auto tag) {
          using type = typename std::decay_t<decltype(tag)>::type;
          call_buf.append(ctx->resolve<type>(reg));
        });

      } else if (t->is_big()) {
        NOT_YET();
      } else {
        ir::PrimitiveDispatch(ir::PrimitiveIndex(t), [&](auto tag) {
          using type = typename std::decay_t<decltype(tag)>::type;
          call_buf.append(iter->read<type>());
        });
      }
    }

    uint16_t num_rets = iter->read<uint16_t>();
    std::vector<ir::Addr> return_slots;
    return_slots.reserve(num_rets);
    for (uint16_t i = 0; i < num_rets; ++i) {
      ir::Reg reg = iter->read<ir::Reg>();
      // TODO: handle is_loc outparams.
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the interpretter.
      auto addr = ir::Addr::Heap(ctx->current_frame().regs_.raw(reg));
      DEBUG_LOG("call")("Ret addr = ", addr);
      return_slots.push_back(addr);
    }

    Execute(f, call_buf, return_slots, ctx);

  } else if constexpr (std::is_same_v<CmdType, ir::ReturnCmd>) {
    typename CmdType::control_bits ctrl =
        iter->read<typename CmdType::control_bits>();
    uint16_t n        = iter->read<uint16_t>();
    ir::Addr ret_slot = ret_slots[n];

    if (ctrl.only_get) {
      ir::Reg reg = iter->read<ir::Reg>();
      frame.regs_.set(reg, ret_slot);
      return;
    }
    DEBUG_LOG("return")("return slot #", n, " = ", ret_slot);

    ASSERT(ret_slot.kind == ir::Addr::Kind::Heap);
    ir::PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      T val   = ReadAndResolve<T>(ctrl.reg, iter, ctx);
      DEBUG_LOG("return")("val = ", val);
      *ASSERT_NOT_NULL(static_cast<T *>(ret_slot.as_heap)) = val;
    });

  } else if constexpr (std::is_same_v<CmdType, ir::PhiCmd>) {
    uint8_t primitive_type = iter->read<uint8_t>();
    uint16_t num           = iter->read<uint16_t>();
    uint64_t index         = std::numeric_limits<uint64_t>::max();
    for (uint16_t i = 0; i < num; ++i) {
      if (ctx->current_frame().prev_ == iter->read<ir::BasicBlock const *>()) {
        index = i;
      }
    }
    ASSERT(index != std::numeric_limits<uint64_t>::max());

    ir::PrimitiveDispatch(primitive_type, [&](auto tag) {
      using T                = typename std::decay_t<decltype(tag)>::type;
      std::vector<T> results = ir::internal::Deserialize<uint16_t, T>(
          iter, [ctx](ir::Reg reg) { return ctx->resolve<T>(reg); });

      if constexpr (std::is_same_v<T, bool>) {
        frame.regs_.set(iter->read<ir::Reg>(), bool{results[index]});
      } else {
        frame.regs_.set(iter->read<ir::Reg>(), results[index]);
      }
    });

  } else if constexpr (std::is_same_v<CmdType, ir::ScopeCmd>) {
    ir::ScopeDef *scope_def = iter->read<ir::ScopeDef *>();

    scope_def->inits_ = ir::internal::Deserialize<uint16_t, ir::Jump const *>(
        iter,
        [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump const *>(reg); });
    scope_def->dones_ = ir::internal::Deserialize<uint16_t, ir::AnyFunc>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::AnyFunc>(reg); });

    uint16_t num_blocks = iter->read<uint16_t>();
    for (uint16_t i = 0; i < num_blocks; ++i) {
      std::string_view name = iter->read<std::string_view>();
      ir::BlockDef *block   = iter->read<ir::BlockDef *>();
      scope_def->blocks_.emplace(name, block);
    }

    ir::Reg result_reg = iter->read<ir::Reg>();
    frame.regs_.set(result_reg, scope_def);

  } else if constexpr (std::is_same_v<CmdType, ir::BlockCmd>) {
    ir::BlockDef *block_def = iter->read<ir::BlockDef *>();
    block_def->before_      = ir::internal::Deserialize<uint16_t, ir::AnyFunc>(
        iter, [ctx](ir::Reg reg) { return ctx->resolve<ir::AnyFunc>(reg); });
    block_def->after_ = ir::internal::Deserialize<uint16_t, ir::Jump const *>(
        iter,
        [ctx](ir::Reg reg) { return ctx->resolve<ir::Jump const *>(reg); });
    ir::Reg result_reg = iter->read<ir::Reg>();
    frame.regs_.set(result_reg, block_def);

  } else if constexpr (std::is_same_v<CmdType, ir::EnumerationCmd>) {
    using enum_t             = typename CmdType::enum_t;
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

    frame.regs_.set(iter->read<ir::Reg>(), result);

  } else if constexpr (std::is_same_v<CmdType, ir::StructCmd>) {
    uint16_t num = iter->read<uint16_t>();

    ast::Scope const *scope  = iter->read<ast::Scope const *>();

    std::vector<type::Struct::Field> fields;
    fields.reserve(num);

    for (uint16_t i = 0; i < num; ++i) {
      auto &field = fields.emplace_back();
      field.name  = iter->read<std::string_view>();
    }

    std::vector<type::Type const *> types =
        ir::internal::Deserialize<uint16_t, type::Type const *>(
            iter,
            [&](ir::Reg reg) { return ctx->resolve<type::Type const *>(reg); });
    for (uint16_t i = 0; i < num; ++i) { fields[i].type = types[i]; }

    frame.regs_.set(iter->read<ir::Reg>(), new type::Struct(scope, fields));

  } else if constexpr (std::is_same_v<CmdType, ir::OpaqueTypeCmd>) {
    module::BasicModule const *mod = iter->read<module::BasicModule const *>();
    frame.regs_.set(iter->read<ir::Reg>(), new type::Opaque(mod));

  } else if constexpr (std::is_same_v<CmdType, ir::ArrayCmd>) {
    using length_t = typename CmdType::length_t;
    typename CmdType::control_bits ctrl_bits =
        iter->read<typename CmdType::control_bits>();
    auto len = ReadAndResolve<length_t>(ctrl_bits.length_is_reg, iter, ctx);
    auto data_type =
        ReadAndResolve<type::Type const *>(ctrl_bits.type_is_reg, iter, ctx);

    frame.regs_.set(iter->read<ir::Reg>(), type::Arr(len, data_type));
  } else if constexpr (std::is_same_v<CmdType, ir::XorFlagsCmd>) {
    NOT_YET();  // TODO could this be included in binary commands? How is it
                // working already?!
  } else if constexpr (std::is_same_v<CmdType, ir::OrFlagsCmd>) {
    NOT_YET();  // TODO could this be included in binary commands? How is it
                // working already?!
  } else if constexpr (std::is_same_v<CmdType, ir::AndFlagsCmd>) {
    NOT_YET();  // TODO could this be included in binary commands? How is it
                // working already?!
#if defined(ICARUS_DEBUG)
  } else if constexpr (std::is_same_v<CmdType, ir::DebugIrCmd>) {
    size_t i = 0;
    for (auto *block : ctx->current_frame().fn_->blocks()) {
      std::cerr << "\n block #" << i << " (" << block << ")\n" << *block;
      DEBUG_LOG("debug_ir-buffer")(block->cmd_buffer_.to_string());
      ++i;
    }
#endif
  } else if constexpr (std::is_same_v<CmdType, ir::CastCmd>) {
    uint8_t to_type   = iter->read<uint8_t>();
    uint8_t from_type = iter->read<uint8_t>();
    ir::PrimitiveDispatch(from_type, [&](auto from_tag) {
      using FromType = typename std::decay_t<decltype(from_tag)>::type;
      [[maybe_unused]] auto val =
          ctx->resolve<FromType>(ir::Reg(iter->read<ir::Reg>()));
      [[maybe_unused]] auto r = iter->read<ir::Reg>();
      if constexpr (std::is_integral_v<FromType>) {
        switch (to_type) {
          case ir::PrimitiveIndex<int8_t>():
            frame.regs_.set(r, static_cast<int8_t>(val));
            break;
          case ir::PrimitiveIndex<int16_t>():
            frame.regs_.set(r, static_cast<int16_t>(val));
            break;
          case ir::PrimitiveIndex<int32_t>():
            frame.regs_.set(r, static_cast<int32_t>(val));
            break;
          case ir::PrimitiveIndex<int64_t>():
            frame.regs_.set(r, static_cast<int64_t>(val));
            break;
          case ir::PrimitiveIndex<uint8_t>():
            frame.regs_.set(r, static_cast<uint8_t>(val));
            break;
          case ir::PrimitiveIndex<uint16_t>():
            frame.regs_.set(r, static_cast<uint16_t>(val));
            break;
          case ir::PrimitiveIndex<uint32_t>():
            frame.regs_.set(r, static_cast<uint32_t>(val));
            break;
          case ir::PrimitiveIndex<uint64_t>():
            frame.regs_.set(r, static_cast<uint64_t>(val));
            break;
          case ir::PrimitiveIndex<float>():
            frame.regs_.set(r, static_cast<float>(val));
            break;
          case ir::PrimitiveIndex<double>():
            frame.regs_.set(r, static_cast<double>(val));
            break;
          case ir::PrimitiveIndex<ir::EnumVal>():
            frame.regs_.set(r, ir::EnumVal(val));
            break;
          case ir::PrimitiveIndex<ir::FlagsVal>():
            frame.regs_.set(r, ir::FlagsVal(val));
            break;
        }
      } else if constexpr (std::is_floating_point_v<FromType>) {
        switch (to_type) {
          case ir::PrimitiveIndex<float>():
            frame.regs_.set(r, static_cast<float>(val));
            break;
          case ir::PrimitiveIndex<double>():
            frame.regs_.set(r, static_cast<double>(val));
            break;
        }
      } else if constexpr (std::is_same_v<FromType, ir::EnumVal> or
                           std::is_same_v<FromType, ir::FlagsVal>) {
        switch (to_type) {
          case ir::PrimitiveIndex<int8_t>():
            frame.regs_.set(r, static_cast<int8_t>(val.value));
            break;
          case ir::PrimitiveIndex<int16_t>():
            frame.regs_.set(r, static_cast<int16_t>(val.value));
            break;
          case ir::PrimitiveIndex<int32_t>():
            frame.regs_.set(r, static_cast<int32_t>(val.value));
            break;
          case ir::PrimitiveIndex<int64_t>():
            frame.regs_.set(r, static_cast<int64_t>(val.value));
            break;
          case ir::PrimitiveIndex<uint8_t>():
            frame.regs_.set(r, static_cast<uint8_t>(val.value));
            break;
          case ir::PrimitiveIndex<uint16_t>():
            frame.regs_.set(r, static_cast<uint16_t>(val.value));
            break;
          case ir::PrimitiveIndex<uint32_t>():
            frame.regs_.set(r, static_cast<uint32_t>(val.value));
            break;
          case ir::PrimitiveIndex<uint64_t>():
            frame.regs_.set(r, static_cast<uint64_t>(val.value));
            break;
        }
      } else if constexpr (std::is_pointer_v<FromType>) {
        NOT_YET(r, val);
      } else {
        UNREACHABLE(r, val);
      }
    });
  } else if constexpr (std::is_same_v<CmdType, ir::SemanticCmd>) {
    ir::AnyFunc f;
    base::untyped_buffer call_buf(sizeof(ir::Addr));
    switch (iter->read<typename CmdType::Kind>()) {
      case CmdType::Kind::Init: {
        type::Type const *t = iter->read<type::Type const *>();
        call_buf.append(ctx->resolve<ir::Addr>(ir::Reg(iter->read<ir::Reg>())));

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
      case CmdType::Kind::Destroy: {
        type::Type const *t = iter->read<type::Type const *>();
        call_buf.append(ctx->resolve<ir::Addr>(ir::Reg(iter->read<ir::Reg>())));

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
      case CmdType::Kind::Move: {
        bool to_reg         = iter->read<bool>();
        type::Type const *t = iter->read<type::Type const *>();
        call_buf.append(ctx->resolve<ir::Addr>(ir::Reg(iter->read<ir::Reg>())));
        call_buf.append(ReadAndResolve<ir::Addr>(to_reg, iter, ctx));

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
      } break;
      case CmdType::Kind::Copy: {
        bool to_reg         = iter->read<bool>();
        type::Type const *t = iter->read<type::Type const *>();
        call_buf.append(ctx->resolve<ir::Addr>(ir::Reg(iter->read<ir::Reg>())));
        call_buf.append(ReadAndResolve<ir::Addr>(to_reg, iter, ctx));
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

    Execute(f, call_buf, ret_slots, ctx);

  } else if constexpr (std::is_same_v<CmdType, ir::LoadSymbolCmd>) {
    std::string_view name  = iter->read<std::string_view>();
    type::Type const *type = iter->read<type::Type const *>();
    ir::Reg reg            = iter->read<ir::Reg>();

    if (auto *fn_type = type->if_as<type::Function>()) {
      void (*sym)() = LoadFunctionSymbol(name);
      frame.regs_.set(reg, ir::AnyFunc{ir::ForeignFn(sym, fn_type)});
    } else if (type->is<type::Pointer>()) {
      void *sym = LoadDataSymbol(name);
      frame.regs_.set(reg, ir::Addr::Heap(*static_cast<void **>(sym)));
    } else {
      NOT_YET(type->to_string());
    }
  } else if constexpr (std::is_same_v<CmdType, ir::TypeInfoCmd>) {
    uint8_t ctrl_bits = iter->read<uint8_t>();
    auto type = ReadAndResolve<type::Type const *>(ctrl_bits & 0x01, iter, ctx);
    ir::Reg reg = iter->read<ir::Reg>();

    if (ctrl_bits & 0x02) {
      frame.regs_.set(reg, type->alignment(core::Interpretter()));

    } else {
      frame.regs_.set(reg, type->bytes(core::Interpretter()));
    }

  } else if constexpr (std::is_same_v<CmdType, ir::AccessCmd>) {
    typename CmdType::control_bits ctrl_bits =
        iter->read<typename CmdType::control_bits>();
    type::Type const *type = iter->read<type::Type const *>();

    ir::Addr addr = ReadAndResolve<ir::Addr>(ctrl_bits.reg_ptr, iter, ctx);
    int64_t index = ReadAndResolve<int64_t>(ctrl_bits.reg_index, iter, ctx);
    ir::Reg reg   = iter->read<ir::Reg>();

    auto arch = core::Interpretter();
    core::Bytes offset;
    if (ctrl_bits.is_array) {
      offset = core::FwdAlign(type->bytes(arch), type->alignment(arch)) * index;
    } else if (auto *struct_type = type->if_as<type::Struct>()) {
      offset = struct_type->offset(index, arch);
    } else if (auto *tuple_type = type->if_as<type::Tuple>()) {
      offset = struct_type->offset(index, arch);
    }

    frame.regs_.set(reg, addr + offset);
  } else if constexpr (std::is_same_v<CmdType, ir::VariantAccessCmd>) {
    bool get_val = iter->read<bool>();
    bool is_reg  = iter->read<bool>();

    ir::Addr addr = ReadAndResolve<ir::Addr>(is_reg, iter, ctx);
    DEBUG_LOG("variant")(addr);
    if (get_val) { addr += type::Type_->bytes(core::Interpretter()); }

    ir::Reg reg = iter->read<ir::Reg>();
    DEBUG_LOG("variant")(reg);
    frame.regs_.set(reg, addr);
  } else {
    static_assert(base::always_false<CmdType>());
  }
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

void ExecutionContext::ExecuteBlock(absl::Span<ir::Addr const> ret_slots) {
  auto iter = current_frame().current_block()->cmd_buffer_.begin();
  while (iter < current_frame().current_block()->cmd_buffer_.end()) {
    auto cmd_index = iter.read<ir::cmd_index_t>();
    switch (cmd_index) {
#define ICARUS_IR_CMD_X(type)                                                  \
  case ir::type::index: {                                                      \
    DEBUG_LOG("dbg")(#type);                                                   \
    ExecuteCmd<ir::type>(&iter, ret_slots, this);                              \
  } break;
#include "ir/cmd/cmd.xmacro.h"
#undef ICARUS_IR_CMD_X
      default: UNREACHABLE(static_cast<int>(cmd_index));
    }
  }
}

}  // namespace interpretter
