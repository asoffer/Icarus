#include "frontend/source/file.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("FileSource reading empty file") {
  REQUIRE_ASSIGN(auto name, CanonicalFileName::Make(FileName{
                                "frontend/source/testdata/empty_file.txt"}));
  REQUIRE_ASSIGN(auto src, FileSource::Make(name));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSource one-line file") {
  REQUIRE_ASSIGN(auto name, CanonicalFileName::Make(FileName{
                                "frontend/source/testdata/one_line_file.txt"}));
  REQUIRE_ASSIGN(auto src, FileSource::Make(name));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK_FALSE(chunk.more_to_read);
}

TEST_CASE("FileSource multiple lines") {
  REQUIRE_ASSIGN(auto name,
                 CanonicalFileName::Make(
                     FileName{"frontend/source/testdata/multi_line_file.txt"}));
  REQUIRE_ASSIGN(auto src, FileSource::Make(name));

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
