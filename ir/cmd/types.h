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
#include "ir/cmd/util.h"
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
  constexpr static cmd_index_t index = 29 * 256;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct StructCmd {
  constexpr static cmd_index_t index = 30;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct OpaqueTypeCmd {
  constexpr static cmd_index_t index = 31 * 256;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
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

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

using VariantCmd = internal::VariadicCmd<256 * 16, type::Type const *, type::Var>;
using TupleCmd   = internal::VariadicCmd<256 * 17, type::Type const *, type::Tup>;
using PtrCmd     = internal::UnaryCmd<
    18 * 256,
    internal::Functor<type::Pointer const *, std::tuple<type::Type const *>,
                      type::Ptr>,
    type::Type const *>;
using BufPtrCmd = internal::UnaryCmd<
    19 * 256,
    internal::Functor<type::BufferPointer const *,
                      std::tuple<type::Type const *>, type::BufPtr>,
    type::Type const *>;

struct ArrowCmd {
  constexpr static cmd_index_t index = 22 * 256;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    return "NOT_YET";
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_TYPES_H
