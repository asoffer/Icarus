#include "frontend/source/file.h"

#include "gtest/gtest.h"

namespace frontend {
namespace {

TEST(FileSource, EmptyFile) {
  auto name = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/empty_file.txt"});
  ASSERT_TRUE(name.has_value());
  auto src = FileSource::Make(*name);
  ASSERT_TRUE(src.has_value());

  auto chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "");
  EXPECT_FALSE(chunk.more_to_read);
}

TEST(FileSource, OneLine) {
  auto name = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/one_line_file.txt"});
  ASSERT_TRUE(name.has_value());
  auto src = FileSource::Make(*name);
  ASSERT_TRUE(src.has_value());

  auto chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "hello");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "");
  EXPECT_FALSE(chunk.more_to_read);
}

TEST(FileSource, MultilineFile) {
  auto name = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/multi_line_file.txt"});
  ASSERT_TRUE(name.has_value());
  auto src = FileSource::Make(*name);
  ASSERT_TRUE(src.has_value());

  auto chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "hello");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "world!");
  EXPECT_TRUE(chunk.more_to_read);

  chunk = src->ReadUntil('\n');
  EXPECT_EQ(chunk.view, "");
  EXPECT_FALSE(chunk.more_to_read);
}

}  // namespace
}  // namespace frontend
