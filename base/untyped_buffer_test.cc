#include "base/untyped_buffer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace base {
namespace {
using ::testing::IsEmpty;
using ::testing::Not;

TEST(UntypedBuffer, DefaultConstructorInvariants) {
  untyped_buffer buf;
  EXPECT_EQ(buf.size(), 0);
  EXPECT_THAT(buf, IsEmpty());
}

TEST(UntypedBuffer, Conversion) {
  untyped_buffer buf;
  {
    untyped_buffer_view view(buf);
    EXPECT_EQ(view.size(), 0);
    EXPECT_TRUE(view.empty());
  }

  buf.append(1);
  buf.append(2);
  {
    untyped_buffer_view view(buf);
    EXPECT_EQ(view.size(), sizeof(int) * 2);
    EXPECT_FALSE(view.empty());
  }
}

TEST(UntypedBuffer, MakeFull) {
  untyped_buffer buf = untyped_buffer::MakeFull(10);
  EXPECT_EQ(buf.size(), 10);
}

TEST(UntypedBUffer, Append) {
  untyped_buffer buf;
  buf.append(3);
  buf.append(4);
  EXPECT_EQ(buf.size(), 2 * sizeof(int));
  EXPECT_THAT(buf, Not(IsEmpty()));
}

TEST(UntypedBuffer, Assignment) {
  untyped_buffer buf1, buf2;
  buf1.append(3);

  EXPECT_THAT(buf1, Not(IsEmpty()));
  buf1 = std::move(buf2);
  EXPECT_THAT(buf1, IsEmpty());
}

TEST(UntypedBuffer, Access) {
  untyped_buffer buf;

  auto access_1 = buf.append(1);
  EXPECT_EQ(buf.get<int>(access_1), 1);

  auto access_2 = buf.append(2);
  auto access_3 = buf.append(3);
  auto access_4 = buf.append(4);
  EXPECT_EQ(buf.get<int>(access_1), 1);
  EXPECT_EQ(buf.get<int>(access_2), 2);
  EXPECT_EQ(buf.get<int>(access_3), 3);
  EXPECT_EQ(buf.get<int>(access_4), 4);

  buf.set(access_2, -2);
  EXPECT_EQ(buf.get<int>(access_1), 1);
  EXPECT_EQ(buf.get<int>(access_2), -2);
  EXPECT_EQ(buf.get<int>(access_3), 3);
  EXPECT_EQ(buf.get<int>(access_4), 4);
}

TEST(UntypedBuffer, ToString) {
  EXPECT_EQ(UniversalPrintToString(untyped_buffer{}), "");
  EXPECT_EQ(UniversalPrintToString(untyped_buffer{10}), "");

  {
    untyped_buffer buf;
    buf.append(char{0});
    buf.append(char{1});
    EXPECT_EQ(UniversalPrintToString(buf), "00 01\n");
  }

  {
    untyped_buffer buf;
    for (char i = 0; i < 13; ++i) { buf.append(i); }
    EXPECT_EQ(UniversalPrintToString(buf),
              "00 01 02 03 04 05 06 07\n"
              "08 09 0a 0b 0c\n");
  }
}

TEST(UntypedBuffer, Clear) {
  untyped_buffer buf;
  buf.append(123);
  buf.append(true);
  EXPECT_GT(buf.size(), 0);
  buf.clear();
  EXPECT_EQ(buf.size(), 0);
}

}  // namespace
}  // namespace base
