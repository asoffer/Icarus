#ifndef ICARUS_FRONTEND_SOURCE_ID_H
#define ICARUS_FRONTEND_SOURCE_ID_H

#include <cstdint>
#include <utility>

namespace frontend {

struct SourceId {
  static SourceId Make();

  SourceId() = delete;

  constexpr SourceId(SourceId const &) noexcept = default;
  constexpr SourceId(SourceId &&) noexcept      = default;
  constexpr SourceId &operator=(SourceId const &) noexcept = default;
  constexpr SourceId &operator=(SourceId &&) noexcept = default;

  friend constexpr bool operator==(SourceId lhs, SourceId rhs) {
    return lhs.value_ == rhs.value_;
  }

  template <typename H>
  friend H AbslHashValue(H h, SourceId id) {
    return H::combine(std::move(h), id.value_);
  }

 private:
  constexpr SourceId(uint64_t value) : value_(value) {}
  uint64_t value_;
};

constexpr bool operator!=(SourceId lhs, SourceId rhs) { return not (lhs == rhs); }

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_ID_H
