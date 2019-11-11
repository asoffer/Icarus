#ifndef ICARUS_IR_CMD_MISC_H
#define ICARUS_IR_CMD_MISC_H

#include <string>
#include <string_view>
#include <vector>

#include "backend/exec.h"
#include "base/debug.h"
#include "ir/cmd/util.h"
#include "ir/inliner.h"
#include "ir/reg_or.h"
#include "type/type.h"

namespace ir {
struct SemanticCmd {
  constexpr static cmd_index_t index = 33;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct LoadSymbolCmd {
  constexpr static cmd_index_t index = 34;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct TypeInfoCmd {
  constexpr static cmd_index_t index = 35;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct AccessCmd {
  constexpr static cmd_index_t index = 36;

  struct control_bits {
    uint8_t is_array : 1;
    uint8_t reg_ptr : 1;
    uint8_t reg_index : 1;
  };
  static control_bits MakeControlBits(bool is_array, bool ptr, bool index) {
    control_bits ctrl_bits;
    ctrl_bits.is_array  = is_array;
    ctrl_bits.reg_ptr   = ptr;
    ctrl_bits.reg_index = index;
    return ctrl_bits;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct VariantAccessCmd {
  constexpr static cmd_index_t index = 37;

  // TODO you store a bool for val vs type and a bool for addr.is_reg(). These
  // should be compresseed.

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

base::Tagged<core::Alignment, Reg> Align(RegOr<type::Type const *> r);
base::Tagged<core::Bytes, Reg> Bytes(RegOr<type::Type const *> r);

void Init(type::Type const *t, Reg r);
void Destroy(type::Type const *t, Reg r);
void Move(type::Type const *t, Reg from, RegOr<Addr> to);
void Copy(type::Type const *t, Reg from, RegOr<Addr> to);

type::Typed<Reg> LoadSymbol(std::string_view name, type::Type const *type);

base::Tagged<Addr, Reg> PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                type::Pointer const *t);
type::Typed<Reg> Field(RegOr<Addr> r, type::Struct const *t, int64_t n);
type::Typed<Reg> Field(RegOr<Addr> r, type::Tuple const *t, int64_t n);

Reg VariantType(RegOr<Addr> const &r);
Reg VariantValue(type::Variant const *v, RegOr<Addr> const &r);

#if defined(ICARUS_DEBUG)
struct DebugIrCmd {
  constexpr static cmd_index_t index =
      (std::numeric_limits<cmd_index_t>::max)();

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    return "debug-ir";
  }
};
inline void DebugIr() {}
#endif  // ICARUS_DEBUG

}  // namespace ir

#endif  // ICARUS_IR_CMD_MISC_H
