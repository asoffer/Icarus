#ifndef ICARUS_COMMON_TO_BYTES_H
#define ICARUS_COMMON_TO_BYTES_H

#include <cstddef>
#include <span>
#include <string>
#include <string_view>

namespace ic {

inline std::span<std::byte> ToBytes(std::string& content) {
  return std::span<std::byte>(reinterpret_cast<std::byte*>(content.data()),
                              content.size());
}

inline std::span<std::byte const> ToBytes(std::string_view content) {
  return std::span<std::byte const>(
      reinterpret_cast<std::byte const*>(content.data()), content.size());
}

}  // namespace ic

#endif // ICARUS_COMMON_TO_BYTES_H
