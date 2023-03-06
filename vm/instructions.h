#ifndef ICARUS_VM_INSTRUCTIONS_H
#define ICARUS_VM_INSTRUCTIONS_H

#include <cstdint>
#include <type_traits>

#include "jasmin/instruction.h"
#include "jasmin/value_stack.h"
#include "vm/immediate_values.h"

namespace vm {

template <bool FromSigned, bool ToSigned>
struct ZeroExtend
    : jasmin::StackMachineInstruction<ZeroExtend<FromSigned, ToSigned>> {
  using Options = ZeroExtendOptions;

  static void execute(jasmin::ValueStack& value_stack, Options options) {
    std::conditional_t<FromSigned, int64_t, uint64_t> scratch;
    if (options.from_bits <= 8) {
      using from_type = std::conditional_t<FromSigned, int8_t, uint8_t>;
      scratch         = value_stack.pop<from_type>();
    } else if (options.from_bits <= 16) {
      using from_type = std::conditional_t<FromSigned, int16_t, uint16_t>;
      scratch         = value_stack.pop<from_type>();
    } else if (options.from_bits <= 32) {
      using from_type = std::conditional_t<FromSigned, int32_t, uint32_t>;
      scratch         = value_stack.pop<from_type>();
    } else {
      using from_type = std::conditional_t<FromSigned, int64_t, uint64_t>;
      scratch         = value_stack.pop<from_type>();
    }

    if (options.to_bits <= 8) {
      using to_type = std::conditional_t<ToSigned, int8_t, uint8_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else if (options.to_bits <= 16) {
      using to_type = std::conditional_t<ToSigned, int16_t, uint16_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else if (options.to_bits <= 32) {
      using to_type = std::conditional_t<ToSigned, int32_t, uint32_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else {
      using to_type = std::conditional_t<ToSigned, int64_t, uint64_t>;
      value_stack.push(static_cast<to_type>(scratch));
    }
  }
};

}  // namespace vm

#endif  // ICARUS_VM_INSTRUCTIONS_H
