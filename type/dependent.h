#ifndef ICARUS_TYPE_DEPENDENT_H
#define ICARUS_TYPE_DEPENDENT_H

#include <cstdint>
#include <vector>

#include "type/basic.h"

namespace ic::type {
namespace internal_dependent {

struct Term {
  struct Node {
    enum class Kind : uint8_t { Function, Type, DeBruijnIndex };
    Kind kind;
    uint32_t subtree_size = 1;
    union {
      uint32_t index;
      Type type;
    } payload;
  };

  static Term DeBruijnIndex(uint32_t index);
  static Term Type(type::Type t);
  static Term Function(Term const &type, Term term);

 private:
  std::vector<Node> nodes_;
};

}  // namespace internal_dependent
}  // namespace ic::type

#endif  // ICARUS_TYPE_DEPENDENT_H
