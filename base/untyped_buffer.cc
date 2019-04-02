#include "base/untyped_buffer.h"

#include "absl/strings/str_join.h"

namespace base {
std::string untyped_buffer::to_string(size_t width, size_t indent) const {
  constexpr char char_lookup[32] ="0123456789abcdef";

  std::vector<std::string> lines;

  size_t num_left = size_;
  while (num_left != 0) {
    size_t row_width = std::min(width, num_left);
    std::string line(3 * row_width - 1 + indent, ' ');
    char *index = &line[indent];
    for (size_t i = 0; i < row_width; ++i) {
      uint8_t num =
          *reinterpret_cast<uint8_t const *>(raw(size_ - num_left + i));
      uint8_t upper = num / 16;
      uint8_t lower = num & 0x0f;
      *index++      = char_lookup[upper];
      *index++      = char_lookup[lower];
      ++index;  // Skip the next space.
    }
    num_left -= std::min(num_left, width);
    lines.push_back(std::move(line));
  }
  return absl::StrJoin(lines, "\n");
}
}  // namespace base
