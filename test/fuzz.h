#ifndef ICARUS_TEST_FUZZ_H
#define ICARUS_TEST_FUZZ_H

#include <cstdint>
#include <cstring>
#include <type_traits>

namespace test {

template <typename T>
T Fuzzy(uint8_t const *data, size_t length) {
  if constexpr (nth::type < T >= = nth::type<std::string>) {
    return std::string(reinterpret_cast<char const *>(data), length);

  } else {
    static_assert(std::is_trivially_copyable_v<T>);
    if (length < sizeof(T)) { std::terminate(); }
    T val;
    std::memcpy(&val, data, sizeof(T));
    return val;
  }
}

}  // namespace test

#endif  // ICARUS_TEST_FUZZ_H
