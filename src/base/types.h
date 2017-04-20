#ifndef ICARUS_BASE_TYPES_H
#define ICARUS_BASE_TYPES_H

#include <cstdint>

using i16 = int16_t;
static_assert(sizeof(i16) == 2, "");
using i32 = int32_t;
static_assert(sizeof(i32) == 4, "");
using i64 = int64_t;
static_assert(sizeof(i64) == 8, "");
using u16 = uint16_t;
static_assert(sizeof(u16) == 2, "");
using u32 = uint32_t;
static_assert(sizeof(u32) == 4, "");
using u64 = uint64_t;
static_assert(sizeof(u64) == 8, "");

#endif // ICARUS_BASE_TYPES_H
