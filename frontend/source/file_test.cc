#include "frontend/source/file.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("Failed to open FileSource") {
  CHECK_FALSE(FileSource::Make("not_a_file.txt").has_value());
}

TEST_CASE("FileSource reading empty file") {
  REQUIRE_ASSIGN(auto src,
                 FileSource::Make("frontend/source/testdata/empty_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSource one-line file") {
  REQUIRE_ASSIGN(
      auto src, FileSource::Make("frontend/source/testdata/one_line_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSource multiple lines") {
  REQUIRE_ASSIGN(auto src, FileSource::Make(
                               "frontend/source/testdata/multi_line_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "world!");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

}  // namespace
}  // namespace frontend
