#include "ir/value/result_buffer.h"

#include "gtest/gtest.h"

namespace ir {
namespace {

// TODO: Test entire API
TEST(ResultBuffer, AppendEmpty) {
  {
    CompleteResultBuffer buffer;
    buffer.append();
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_TRUE(buffer.back().empty());

    buffer.append(6);
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_FALSE(buffer[1].empty());
    EXPECT_FALSE(buffer.back().empty());

    buffer.append();
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_FALSE(buffer[1].empty());
    EXPECT_TRUE(buffer[2].empty());
    EXPECT_TRUE(buffer.back().empty());
  }

  {
    PartialResultBuffer buffer;
    buffer.append();
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_TRUE(buffer.back().empty());

    buffer.append(6);
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_FALSE(buffer[1].empty());
    EXPECT_FALSE(buffer.back().empty());

    buffer.append();
    EXPECT_TRUE(buffer[0].empty());
    EXPECT_FALSE(buffer[1].empty());
    EXPECT_TRUE(buffer[2].empty());
    EXPECT_TRUE(buffer.back().empty());
  }
}

}  // namespace
}  // namespace ir
