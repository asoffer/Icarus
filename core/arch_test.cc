#include "core/arch.h"

#include "gtest/gtest.h"

namespace core {
namespace {

TEST(Arch, ForwardAlignment) {
  EXPECT_EQ(FwdAlign(Bytes{0}, Alignment{1}), Bytes{0});

  EXPECT_EQ(FwdAlign(Bytes{0}, Alignment{2}), Bytes{0});
  EXPECT_EQ(FwdAlign(Bytes{1}, Alignment{2}), Bytes{2});

  EXPECT_EQ(FwdAlign(Bytes{0}, Alignment{8}), Bytes{0});
  EXPECT_EQ(FwdAlign(Bytes{1}, Alignment{8}), Bytes{8});
  EXPECT_EQ(FwdAlign(Bytes{8}, Alignment{8}), Bytes{8});
}

}  // namespace
}  // namespace core
