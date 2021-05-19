#ifndef ICARUS_IR_VALUE_ADDR_H
#define ICARUS_IR_VALUE_ADDR_H

namespace ir {

using addr_t = char *;

constexpr addr_t Null() { return nullptr; }

inline addr_t Addr(void *ptr) { return reinterpret_cast<addr_t>(ptr); }
inline addr_t Addr(void const *ptr) {
  return reinterpret_cast<addr_t>(const_cast<void *>(ptr));
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_ADDR_H
