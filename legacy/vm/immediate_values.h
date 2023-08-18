#ifndef ICARUS_VM_IMMEDIATE_VALUES_H
#define ICARUS_VM_IMMEDIATE_VALUES_H

#include <cstdint>

namespace vm {

struct ZeroExtendOptions {
  uint32_t from_bits;
  uint32_t to_bits;
};

}  // namespace vm

#endif  // ICARUS_VM_IMMEDIATE_VALUES_H
