#ifndef ICARUS_IR_VALUE_HASHTAG_H
#define ICARUS_IR_VALUE_HASHTAG_H

#include <array>
#include <iostream>
#include <string_view>

namespace ir {

enum class Hashtag : uint8_t {
  // Denotes that an object should be made visible across module boundaries.
  Export     = 0,
  Uncopyable = 1,
  Immovable  = 2,
};

inline constexpr std::string_view ToStringView(Hashtag h) {
  switch (h) {
    case Hashtag::Export: return "{export}";
    case Hashtag::Uncopyable: return "{uncopyable}";
    case Hashtag::Immovable: return "{immovable}";
  }
}

inline std::ostream& operator<<(std::ostream& os, Hashtag h) {
  return os << ToStringView(h);
}

inline std::array BuiltinHashtagsByName{
    std::pair(std::string_view("{export}"), Hashtag::Export),
    std::pair(std::string_view("{uncopyable}"), Hashtag::Uncopyable),
    std::pair(std::string_view("{immovable}"), Hashtag::Immovable),
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_HASHTAG_H
