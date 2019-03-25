#include "base/untyped_buffer.h"

namespace base {
std::string untyped_buffer::to_string() const {
  constexpr char char_lookup[32] =
      "0\0001\0002\0003\0004\0005\0006\0007\000"
      "8\0009\000a\000b\000c\000d\000e\000f";
  std::string s = "[ ";
  for (size_t i = 0; i < size_; ++i) {
    if (i % 8 == 0) { s += "\n  "; }
    uint8_t num   = *reinterpret_cast<uint8_t const *>(raw(i));
    uint8_t upper = num / 16;
    s += char_lookup + upper * 2;
    uint8_t lower = num & 0xf;
    s += char_lookup + lower * 2;
    s += " ";
  }
  return s + "]";
}
}  // namespace base
