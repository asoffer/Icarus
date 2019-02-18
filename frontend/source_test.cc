#include "frontend/source.h"

#include "test/test.h"

namespace frontend {
namespace {

TEST(StringSrcSingleChunk) {
  StringSrc src("abc");
  auto chunk = src.ReadUntil('\0');
  CHECK(chunk.view == "abc");
  CHECK(chunk.more_to_read == false);
}

TEST(StringSrcSingleChunkNoDelimFound) {
  StringSrc src("abc");
  auto chunk = src.ReadUntil('x');
  CHECK(chunk.view == "abc");
  CHECK(chunk.more_to_read == false);
}

TEST(StringSrcMultipleChunks) {
  StringSrc src("abcdefg");
  auto chunk = src.ReadUntil('d');
  CHECK(chunk.view == "abc");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('d');
  CHECK(chunk.view == "efg");
  CHECK(chunk.more_to_read == false);
}
TEST(StringSrcSmallChunks) {
  StringSrc src("aaa");
  auto chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('a');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcFail) {
  CHECK(FileSrc::Make("not_a_file.txt").has_value() == false);
}

TEST(FileSrcReadEmptyFile) {
  REQUIRE_ASSIGN(auto src, FileSrc::Make("frontend/testdata/empty_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcReadOneLine) {
  REQUIRE_ASSIGN(auto src,
                 FileSrc::Make("frontend/testdata/one_line_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcReadMultipleLines) {
  REQUIRE_ASSIGN(auto src,
                 FileSrc::Make("frontend/testdata/multi_line_file.txt"));

  auto chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "hello");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "world!");
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadUntil('\n');
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == false);
}

}  // namespace
}  // namespace frontend
