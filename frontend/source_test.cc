#include "frontend/source.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("Single-chunk StringSrc") {
  StringSrc src("abc");
  auto chunk = src.ReadUntil('\0');
  CHECK(chunk.view == "abc");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("Single-chunk StringSrc not finding delimiter") {
  StringSrc src("abc");
  auto chunk = src.ReadUntil('x');
  CHECK(chunk.view == "abc");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("StringSrc with multiple chunks") {
  StringSrc src("abcdefg");
  auto chunk = src.ReadUntil('d');
  CHECK(chunk.view == "abc");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('d');
  CHECK(chunk.view == "efg");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("StringSrc with small chunks") {
  StringSrc src("aaa");
  auto chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("Failed to open FileSrc") {
  CHECK_FALSE(FileSrc::Make("not_a_file.txt").has_value());
}

TEST_CASE("FileSrc reading empty file") {
  REQUIRE_ASSIGN(auto src, FileSrc::Make("frontend/testdata/empty_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSrc one-line file") {
  REQUIRE_ASSIGN(auto src,
                 FileSrc::Make("frontend/testdata/one_line_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSrc multiple lines") {
  REQUIRE_ASSIGN(auto src,
                 FileSrc::Make("frontend/testdata/multi_line_file.txt"));

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
