#include "base/untyped_buffer.h"

#include "gtest/gtest.h"
#include "gmock/gmock.h"

namespace base {
namespace {
using ::testing::IsEmpty;
using ::testing::Not;

TEST(UntypedBuffer, DefaultConstructorInvariants) {
  untyped_buffer buf;
  EXPECT_EQ(buf.size(), 0);
  EXPECT_THAT(buf, IsEmpty());
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
  EXPECT_EQ(untyped_buffer{}.to_string(), "");
  EXPECT_EQ(untyped_buffer{10}.to_string(), "");

  {
    untyped_buffer buf;
    buf.append(char{0});
    buf.append(char{1});
    EXPECT_EQ(buf.to_string(), "00 01");
  }

  {
    untyped_buffer buf;
    for (char i = 0; i < 13; ++i) { buf.append(i); }
    EXPECT_EQ(buf.to_string(3, 4),
              "    00 01 02\n"
              "    03 04 05\n"
              "    06 07 08\n"
              "    09 0a 0b\n"
              "    0c");
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
