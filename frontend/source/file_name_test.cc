#include "frontend/source/file_name.h"

#include "gtest/gtest.h"

namespace frontend {
namespace {

TEST(CanonicalizeFileName, DifferentFiles) {
  auto name1 = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/empty_file.txt"});
  auto name2 = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/one_line_file.txt"});
  ASSERT_TRUE(name1.has_value());
  ASSERT_TRUE(name2.has_value());
  EXPECT_NE(*name1, *name2);
}

TEST(CanonicalizeFileName, SameFile) {
  auto name1 = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/empty_file.txt"});
  auto name2 = CanonicalFileName::Make(FileName{
      "frontend/source/testdata/./../../source/./testdata/empty_file.txt"});
  ASSERT_TRUE(name1.has_value());
  ASSERT_TRUE(name2.has_value());
  EXPECT_EQ(*name1, *name2);
}

TEST(CanonicalFileName, OpenReadOnly) {
  auto name = CanonicalFileName::Make(
      FileName{"frontend/source/testdata/one_line_file.txt"});
  ASSERT_TRUE(name.has_value());
  auto file         = name->OpenReadOnly();
  char *buf         = nullptr;
  size_t n          = 0;
  ssize_t num_chars = getdelim(&buf, &n, '\n', file.get());
  EXPECT_EQ(buf, std::string{"hello\n"});
  std::free(buf);
}

}  // namespace
}  // namespace frontend
