#include "frontend/source/string.h"

#include "gtest/gtest.h"

namespace frontend {
namespace {

TEST(StringSource, SingleChunk) {
  StringSource src("abc");
  auto chunk = src.ReadUntil('\0');
  EXPECT_EQ(chunk.view, "abc");
  EXPECT_FALSE(chunk.more_to_read);
}

TEST(StringSource, DelimiterNotFound) {
  StringSource src("abc");
  auto chunk = src.ReadUntil('x');
  EXPECT_EQ(chunk.view, "abc");
  EXPECT_FALSE(chunk.more_to_read);
}

TEST(StringSource, MultipleChunks) {
  StringSource src("abcdefg");
  auto chunk = src.ReadUntil('d');
  EXPECT_EQ(chunk.view, "abc");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src.ReadUntil('d');
  EXPECT_EQ(chunk.view, "efg");
  EXPECT_FALSE(chunk.more_to_read);
}

TEST(StringSource, SmallChunks) {
  StringSource src("aaa");
  auto chunk = src.ReadUntil('a');
  EXPECT_EQ(chunk.view, "");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src.ReadUntil('a');
  EXPECT_EQ(chunk.view, "");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src.ReadUntil('a');
  EXPECT_EQ(chunk.view, "");
  EXPECT_FALSE(chunk.more_to_read);
}

}  // namespace
}  // namespace frontend
