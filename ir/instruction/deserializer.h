#include <cstddef>

#include "absl/types/span.h"

namespace ir {

struct InstructionDeserializer {
  explicit InstructionDeserializer(std::string const *content)
      : content_(*ASSERT_NOT_NULL(content)) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    auto span = absl::MakeConstSpan(
        reinterpret_cast<std::byte const *>(content_.data()), num_bytes);
    content_.remove_prefix(num_bytes);
    return span;
  }

  template <typename T>
  bool read(T &t) requires(std::is_trivially_copyable_v<T>) {
    std::memcpy(&t, content_.data(), sizeof(t));
    content_.remove_prefix(sizeof(t));
    return true;
  }

 private:
  std::string_view content_;
};

}  // namespace ir
