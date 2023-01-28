#ifndef ICARUS_IR_VALUE_HASHTAG_H
#define ICARUS_IR_VALUE_HASHTAG_H

#include <array>
#include <iostream>
#include <string_view>

namespace ir {

enum class Hashtag : uint8_t {
  // Denotes that a scope is to be evaluated and blocks instantiated.
  Const = 0,
  // Denotes that an object should be made visible across module boundaries.
  Export     = 1,
  Uncopyable = 2,
  Immovable  = 3,
};

inline std::array BuiltinHashtagsByName{
    std::pair(std::string_view("{const}"), Hashtag::Const),
    std::pair(std::string_view("{export}"), Hashtag::Export),
    std::pair(std::string_view("{uncopyable}"), Hashtag::Uncopyable),
    std::pair(std::string_view("{immovable}"), Hashtag::Immovable),
};

inline std::ostream& operator<<(std::ostream& os, Hashtag h) {
  return os << BuiltinHashtagsByName[static_cast<uint8_t>(h)].first;
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_HASHTAG_H
