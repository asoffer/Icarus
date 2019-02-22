#include "ir/results.h"

#include "test/test.h"

namespace ir {
namespace {

TEST(Construction) {
  CHECK(Results{}.size() == 0u);
  CHECK(Results{3}.size() == 1u);
  CHECK(Results(3, true, 4).size() == 3u);
}

TEST(AccessValues) {
  Results r{3, true, 4};
  CHECK(r.get<int>(0) == RegisterOr{3});
  CHECK(r.get<bool>(1) == RegisterOr{true});
  CHECK(r.get<int>(2) == RegisterOr{4});
}

TEST(AccessRegisters) {
  Results r;
  r.append(3);
  r.append(Reg{17});
  r.append(true);
  CHECK(r.get<int>(1) == RegisterOr<int>{Reg{17}});
  CHECK(r.get<Reg>(1) == Reg{17});
}

}  // namespace
}  // namespace ir
