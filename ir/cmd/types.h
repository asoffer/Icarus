#ifndef ICARUS_IR_CMD_TYPES_H
#define ICARUS_IR_CMD_TYPES_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/random/distributions.h"
#include "absl/random/random.h"
#include "absl/types/span.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "module/module.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ir {
namespace internal {

template <typename R, typename ArgTup>
struct FunctionPointer {};
template <typename R, typename... Args>
struct FunctionPointer<R, std::tuple<Args...>> {
  using type = R (*)(Args...);
};

template <typename R, typename ArgTup>
using FunctionPointerT = typename FunctionPointer<R, ArgTup>::type;

template <typename R, typename ArgTup, FunctionPointerT<R, ArgTup> FnPtr>
struct Functor {
  template <typename... Args>
  R operator()(Args... args) const {
    return FnPtr(std::forward<Args>(args)...);
  }
};

}  // namespace internal

struct EnumerationCmd {
  using enum_t                       = uint64_t;
  constexpr static cmd_index_t index = 29;

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

struct StructCmd {
  constexpr static cmd_index_t index = 30;

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

struct OpaqueTypeCmd {
  constexpr static cmd_index_t index = 31;
  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

struct ArrayCmd {
  constexpr static cmd_index_t index = 32;
  using length_t                     = int64_t;
  struct control_bits {
    uint8_t length_is_reg : 1;
    uint8_t type_is_reg : 1;
  };

  static control_bits MakeControlBits(bool length_is_reg, bool type_is_reg) {
    control_bits ctrl;
    ctrl.length_is_reg = length_is_reg;
    ctrl.type_is_reg   = type_is_reg;
    return ctrl;
  }

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

using VariantCmd = internal::VariadicCmd<16, type::Type const *, type::Var>;
using TupleCmd   = internal::VariadicCmd<17, type::Type const *, type::Tup>;
using PtrCmd     = internal::UnaryCmd<
    18,
    internal::Functor<type::Pointer const *, std::tuple<type::Type const *>,
                      type::Ptr>,
    type::Type const *>;
using BufPtrCmd = internal::UnaryCmd<
    19,
    internal::Functor<type::BufferPointer const *,
                      std::tuple<type::Type const *>, type::BufPtr>,
    type::Type const *>;

inline RegOr<type::Type const *> Var(
    absl::Span<RegOr<type::Type const *> const> types) {
  return internal::MakeVariadicImpl<VariantCmd>(types);
}

inline RegOr<type::Type const *> Tup(
    absl::Span<RegOr<type::Type const *> const> types) {
  return internal::MakeVariadicImpl<TupleCmd>(types);
}

Reg Enum(module::BasicModule *mod, absl::Span<std::string_view const> names,
         absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
             &specified_values);

Reg Flags(module::BasicModule *mod, absl::Span<std::string_view const> names,
          absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
              &specified_values);

// TODO handle initial values.
Reg Struct(core::Scope const *scope, module::BasicModule *mod,
           std::vector<std::tuple<std::string_view, RegOr<type::Type const *>>>
               fields);

constexpr inline auto Ptr    = internal::UnaryHandler<PtrCmd>{};
constexpr inline auto BufPtr = internal::UnaryHandler<BufPtrCmd>{};

struct ArrowCmd {
  constexpr static cmd_index_t index = 22;
  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx) {
    std::vector<type::Type const *> ins =
        internal::Deserialize<uint16_t, type::Type const *>(
            iter,
            [ctx](Reg reg) { return ctx->resolve<type::Type const *>(reg); });
    std::vector<type::Type const *> outs =
        internal::Deserialize<uint16_t, type::Type const *>(
            iter,
            [ctx](Reg reg) { return ctx->resolve<type::Type const *>(reg); });

    auto &frame = ctx->call_stack.top();
    frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                    type::Func(std::move(ins), std::move(outs)));

    return nullptr;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    return "NOT_YET";
  }

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner) {
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    internal::Deserialize<uint16_t, type::Type const *>(
        iter, [&inliner](Reg &reg) { inliner.Inline(&reg); });
    inliner.Inline(&iter->read<Reg>(), type::Type_);  // Result value
  }
};

RegOr<type::Function const *> Arrow(
    absl::Span<RegOr<type::Type const *> const> ins,
    absl::Span<RegOr<type::Type const *> const> outs);

RegOr<type::Type const *> Array(RegOr<ArrayCmd::length_t> len,
                                RegOr<type::Type const *> data_type);

Reg OpaqueType(module::BasicModule const *mod);

}  // namespace ir

#endif  // ICARUS_IR_CMD_TYPES_H
