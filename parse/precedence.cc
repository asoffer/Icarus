#include "parse/precedence.h"

#include <array>
#include <cstddef>
#include <cstdlib>
#include <utility>

namespace ic {
namespace {

constexpr size_t PrecedenceGroupCount = (1  // For `Loosest`
#define IC_XMACRO_PRECEDENCE_GROUP(group) +1
#include "common/language/precedence.xmacro.h"
);

struct PrecedenceTable {
  constexpr Priority& operator()(Precedence lhs, Precedence rhs) {
    return table_[static_cast<int>(lhs.kind())][static_cast<int>(rhs.kind())];
  }
  constexpr Priority operator()(Precedence lhs, Precedence rhs) const {
    return table_[static_cast<int>(lhs.kind())][static_cast<int>(rhs.kind())];
  }

  std::array<std::array<Priority, PrecedenceGroupCount>, PrecedenceGroupCount>
      table_;
};

constexpr PrecedenceTable MakePrecedenceTable() {
  PrecedenceTable result;

  // Initialize everything to be ambiguous.
  for (auto& a : result.table_) {
    for (auto& p : a) { p = Priority::Ambiguous; }
  }

  // Loosest should be less than everything.
#define IC_XMACRO_PRECEDENCE_GROUP(group)                                      \
  result(Precedence::Loosest(), Precedence::group()) = Priority::Right;
#define IC_XMACRO_PRECEDENCE_ORDER(lower, higher)                              \
  result(Precedence::lower(), Precedence::higher()) = Priority::Right;
#include "common/language/precedence.xmacro.h"

  bool changed = false;
  while (not changed) {
    changed = false;

    for (size_t i = 0; i < PrecedenceGroupCount; ++i) {
      for (size_t j = i + 1; j < PrecedenceGroupCount; ++j) {
        for (size_t k = j + 1; k < PrecedenceGroupCount; ++k) {
          if (result.table_[i][j] == Priority::Right and
              result.table_[j][k] == Priority::Right) {
            changed |= (std::exchange(result.table_[i][k], Priority::Right) ==
                        Priority::Right);
          }
        }
      }
    }
  }

  // Ensure symmetry.
  for (size_t i = 0; i < PrecedenceGroupCount; ++i) {
    for (size_t j = i + 1; j < PrecedenceGroupCount; ++j) {
      result.table_[j][i] =
          static_cast<Priority>(-static_cast<int8_t>(result.table_[i][j]));
    }
  }

  // Initialize the diagonal
  for (size_t i = 0; i < PrecedenceGroupCount; ++i) {
    result.table_[i][i] = Priority::Same;
  }

  return result;
}

constexpr PrecedenceTable Table = MakePrecedenceTable();

}  // namespace

Priority Precedence::Priority(Precedence lhs, Precedence rhs) {
  return Table(lhs, rhs);
}

}  // namespace ic
