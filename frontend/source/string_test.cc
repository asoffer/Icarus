#include "frontend/source/string.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("Single-chunk StringSource") {
  StringSource src("abc");
  auto chunk = src.ReadUntil('\0');
  CHECK(chunk.view == "abc");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("Single-chunk StringSource not finding delimiter") {
  StringSource src("abc");
  auto chunk = src.ReadUntil('x');
  CHECK(chunk.view == "abc");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("StringSource with multiple chunks") {
  StringSource src("abcdefg");
  auto chunk = src.ReadUntil('d');
  CHECK(chunk.view == "abc");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('d');
  CHECK(chunk.view == "efg");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("StringSource with small chunks") {
  StringSource src("aaa");
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

}  // namespace
}  // namespace frontend
