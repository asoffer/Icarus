#include "ir/values.h"

#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("bitwise operators") {
  CHECK((FlagsVal(3) | FlagsVal(5)) == FlagsVal(7));
  CHECK((FlagsVal(3) & FlagsVal(5)) == FlagsVal(1));
  CHECK((FlagsVal(3) ^ FlagsVal(5)) == FlagsVal(6));

  CHECK((FlagsVal(3) | FlagsVal(3)) == FlagsVal(3));
  CHECK((FlagsVal(3) & FlagsVal(3)) == FlagsVal(3));
  CHECK((FlagsVal(3) ^ FlagsVal(3)) == FlagsVal());
}

}  // namespace
}  // namespace ir
