#ifndef ICARUS_TYPE_ALIGNMENT_H
#define ICARUS_TYPE_ALIGNMENT_H

#include <compare>
#include <cstdint>

namespace ic::type {

struct Alignment {
  explicit constexpr Alignment(uint16_t n) : alignment_(n) {}
  constexpr uint32_t value() const { return alignment_; }

  friend auto operator<=>(Alignment, Alignment) = default;

  friend void NthPrint(auto& p, auto& f, Alignment a) {
    f(p, a.value());
    p.write(" alignment");
  }

 private:
  uint16_t alignment_;
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_ALIGNMENT_H
