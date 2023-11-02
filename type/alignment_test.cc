#include "type/alignment.h"

#include "nth/test/test.h"

namespace ic::type {
namespace {

NTH_TEST("alignment/construction") {
  Alignment a(4);
  NTH_EXPECT(a.value() == 4);
  NTH_EXPECT(a == Alignment(4));
}

NTH_TEST("alignment/ordering") {
  Alignment a(2);
  NTH_EXPECT(a <= Alignment(4));
}

}  // namespace
}  // namespace ic::type
