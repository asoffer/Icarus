#include "ir/instruction/inliner.h"

#include <vector>

#include "base/traverse.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {
namespace {
using ::testing::ElementsAre;

TEST(Inliner, Reg) {
  Inliner inliner(4, 0);
  Reg r(3);
  base::Traverse(inliner, r);
  EXPECT_EQ(r, Reg(7));
}

TEST(Inliner, RegOr) {
  Inliner inliner(4, 0);
  RegOr<int> r(3);
  base::Traverse(inliner, r);
  EXPECT_EQ(r, 3);
  r = Reg(3);
  base::Traverse(inliner, r);
  EXPECT_EQ(r, Reg(7));
}

TEST(Inliner, Container) {
  std::vector<RegOr<double>> v = {
      RegOr<double>(1.1),        RegOr<double>(2.2),
      RegOr<double>(Reg(3)), RegOr<double>(4.4),
      RegOr<double>(Reg(5)),
  };

  Inliner inliner(4, 0);
  RegOr<int> r(3);
  base::Traverse(inliner, v);

  EXPECT_THAT(v,
              ElementsAre(RegOr<double>(1.1), RegOr<double>(2.2),
                          RegOr<double>(Reg(7)), RegOr<double>(4.4),
                          RegOr<double>(Reg(9))));
}

}  // namespace
}  // namespace ir
