#ifndef ICARUS_IR_VALUE_ADDR_H
#define ICARUS_IR_VALUE_ADDR_H

#include <cstddef>

namespace ir {

using memory_t       = std::byte;
using addr_t         = memory_t *;
using memory_const_t = std::byte const;
using addr_const_t   = memory_const_t *;

constexpr addr_t Null() { return nullptr; }

inline addr_t Addr(void *ptr) { return reinterpret_cast<addr_t>(ptr); }
inline addr_t Addr(void const *ptr) {
  return reinterpret_cast<addr_t>(const_cast<void *>(ptr));
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_ADDR_H
