#ifndef ICARUS_BASE_TYPES_H
#define ICARUS_BASE_TYPES_H

#include <cstdint>
#include <string>

#define DEFINE_TYPE(new_name, old_name, size)                                  \
  using new_name = old_name;                                                   \
  static_assert(sizeof(new_name) == size, "")

DEFINE_TYPE(i8, int8_t, 1);
DEFINE_TYPE(i16, int16_t, 2);
DEFINE_TYPE(i32, int32_t, 4);
DEFINE_TYPE(i64, int64_t, 8);

DEFINE_TYPE(u8, uint8_t, 1);
DEFINE_TYPE(u16, uint16_t, 2);
DEFINE_TYPE(u32, uint32_t, 4);
DEFINE_TYPE(u64, uint64_t, 8);
#undef DEFINE_TYPE

#endif // ICARUS_BASE_TYPES_H
