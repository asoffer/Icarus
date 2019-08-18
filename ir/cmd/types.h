#ifndef ICARUS_IR_CMD_TYPES_H
#define ICARUS_IR_CMD_TYPES_H

#include <optional>
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
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

struct Module;

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

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
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

Reg Enum(
    ::Module *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values);

Reg Flags(
    ::Module *mod, absl::Span<std::string_view const> names,
    absl::flat_hash_map<uint64_t, RegOr<EnumerationCmd::enum_t>> const
        &specified_values);

constexpr inline auto Ptr    = internal::UnaryHandler<PtrCmd>{};
constexpr inline auto BufPtr = internal::UnaryHandler<BufPtrCmd>{};

struct ArrowCmd {
  constexpr static cmd_index_t index = 22;
  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
                                           std::vector<Addr> const &ret_slots,
                                           backend::ExecContext *ctx) {
    std::vector<type::Type const *> ins =
        internal::Deserialize<uint16_t, type::Type const *>(
            iter,
            [ctx](Reg &reg) { return ctx->resolve<type::Type const *>(reg); });
    std::vector<type::Type const *> outs =
        internal::Deserialize<uint16_t, type::Type const *>(
            iter,
            [ctx](Reg &reg) { return ctx->resolve<type::Type const *>(reg); });

    auto &frame = ctx->call_stack.top();
    frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                    type::Func(std::move(ins), std::move(outs)));

    return std::nullopt;
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

inline RegOr<type::Function const *> Arrow(
    absl::Span<RegOr<type::Type const *> const> ins,
    absl::Span<RegOr<type::Type const *> const> outs) {
  if (absl::c_all_of(ins,
                     [](RegOr<type::Type const *> r) { return !r.is_reg_; }) &&
      absl::c_all_of(outs,
                     [](RegOr<type::Type const *> r) { return !r.is_reg_; })) {
    std::vector<type::Type const *> in_vec, out_vec;
    in_vec.reserve(ins.size());
    for (auto in : ins) { in_vec.push_back(in.val_); }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.val_); }
    return type::Func(std::move(in_vec), std::move(out_vec));
  }

  auto &blk = GetBlock();
  blk.cmd_buffer_.append_index<ArrowCmd>();
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, ins);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, outs);

  Reg result = MakeResult<type::Type const *>();
  blk.cmd_buffer_.append(result);
  return RegOr<type::Function const *>{result};
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_TYPES_H
