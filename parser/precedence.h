#ifndef ICARUS_PARSER_PRECEDENCE_H
#define ICARUS_PARSER_PRECEDENCE_H

#include <cstdint>
#include <limits>
#include <string_view>

namespace ic {

enum class Priority : int8_t {
  // The left-hand operator binds tighter than the right-hand operator
  Left = -1,

  // The operators have no precedence relationship. Using them adjacently
  // without parentheses is an error.
  Ambiguous = 0,

  // The right-hand operator binds tighter than the left-hand operator
  Right = 1,

  // The operators are in the same precedence group.
  Same = 2,
};

struct Precedence {
  enum class Kind : uint8_t {
#define IC_XMACRO_PRECEDENCE_GROUP(group) group,
#include "parser/precedence_groups.xmacro.h"
  };

  constexpr Kind kind() const { return kind_; }

  static Priority Priority(Precedence lhs, Precedence rhs);

  friend bool operator==(Precedence, Precedence) = default;
  friend bool operator!=(Precedence, Precedence) = default;

#define IC_XMACRO_PRECEDENCE_GROUP(group)                                      \
  static constexpr Precedence group() { return Precedence(Kind::group); }
#include "parser/precedence_groups.xmacro.h"


  friend void NthPrint(auto& printer, auto& , Precedence p) {
    static constexpr std::string_view PrecedenceStrings[] = {
#define IC_XMACRO_PRECEDENCE_GROUP(group) #group,
#include "parser/precedence_groups.xmacro.h"
    };
    printer.write(PrecedenceStrings[static_cast<uint8_t>(p.kind_)]);
  }

 private:
  explicit constexpr Precedence(Kind k) : kind_(k) {}

  Kind kind_;
};

}  // namespace ic

#endif  // ICARUS_PARSER_PRECEDENCE_H
