#include "frontend/source.h"

#include "test/test.h"

namespace frontend {
namespace {

TEST(StringSrcSingleChunk) {
  StringSrc src("abc");
  auto chunk = src.ReadChunk();
  CHECK(chunk.view == "abc");
  CHECK(chunk.view.size() == 3u);
  CHECK(chunk.more_to_read == false);
}

TEST(StringSrcMultipleChunks) {
  StringSrc src(std::string(512, 'a') + std::string(512, 'b') +
                std::string(512, 'c') + std::string(512, 'd') +
                std::string(512, 'e'));
  auto chunk = src.ReadChunk();
  CHECK(chunk.view[0] == 'a');
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadChunk();
  CHECK(chunk.view[0] == 'c');
  CHECK(chunk.more_to_read == true);

  chunk = src.ReadChunk();
  CHECK(chunk.view[0] == 'e');
  CHECK(chunk.view.size() == 512u);
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcFail) {
  CHECK(MakeFileSrc("not_a_file.txt").has_value() == false);
}

TEST(FileSrcReadEmptyFile) {
  auto maybe_file_src = MakeFileSrc("frontend/testdata/empty_file.txt");
  REQUIRE(maybe_file_src.has_value() == true);

  auto chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "");
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcReadOneLine) {
  auto maybe_file_src = MakeFileSrc("frontend/testdata/one_line_file.txt");
  REQUIRE(maybe_file_src.has_value() == true);

  auto chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "hello\n");
  CHECK(chunk.more_to_read == false);
}

TEST(FileSrcReadMultipleLines) {
  auto maybe_file_src = MakeFileSrc<5>("frontend/testdata/multi_line_file.txt");
  REQUIRE(maybe_file_src.has_value() == true);

  auto chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "hell");
  CHECK(chunk.more_to_read == true);

  chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "o\nwo");
  CHECK(chunk.more_to_read == true);

  chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "rld!");
  CHECK(chunk.more_to_read == true);

  chunk = maybe_file_src->ReadChunk();
  CHECK(chunk.view == "\n");
  CHECK(chunk.more_to_read == false);
}

}  // namespace
}  // namespace frontend
