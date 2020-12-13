#include "frontend/source/buffer.h"

#include "gtest/gtest.h"

namespace frontend {
namespace {

TEST(SourceBuffer, NumChunks) {
  SourceBuffer buffer("\n");
  EXPECT_EQ(buffer.num_chunks(), 1);

  buffer.AppendChunk("abc\n");
  EXPECT_EQ(buffer.num_chunks(), 2);

  buffer.AppendChunk("\n");
  EXPECT_EQ(buffer.num_chunks(), 3);

  buffer.AppendChunk("\n");
  EXPECT_EQ(buffer.num_chunks(), 4);

  buffer.AppendChunk("abc\n");
  EXPECT_EQ(buffer.num_chunks(), 5);
}

TEST(SourceBuffer, OneChunkOneLine) {
  SourceBuffer buffer("abc\n");
  ASSERT_EQ(buffer.num_chunks(), 1);
  EXPECT_EQ(buffer.chunk(0), "abc\n");
  ASSERT_EQ(buffer.num_lines(), 1);
  EXPECT_EQ(buffer.line(1), "abc\n");
}

TEST(SourceBuffer, OneChunkMultipleLines) {
  SourceBuffer buffer("abc\ndef\n");
  ASSERT_EQ(buffer.num_chunks(), 1);
  EXPECT_EQ(buffer.chunk(0), "abc\ndef\n");
  ASSERT_EQ(buffer.num_lines(), 2);
  EXPECT_EQ(buffer.line(1), "abc\n");
  EXPECT_EQ(buffer.line(2), "def\n");
}

TEST(SourceBuffer, OneChunkEndingWithMultipleNewlines) {
  SourceBuffer buffer("abc\ndef\n\n");
  ASSERT_EQ(buffer.num_chunks(), 1);
  EXPECT_EQ(buffer.chunk(0), "abc\ndef\n\n");
  ASSERT_EQ(buffer.num_lines(), 3);
  EXPECT_EQ(buffer.line(1), "abc\n");
  EXPECT_EQ(buffer.line(2), "def\n");
  EXPECT_EQ(buffer.line(3), "\n");
}

TEST(SourceBuffer, MultipleChunks) {
  SourceBuffer buffer("abc\n111\n\n");
  buffer.AppendChunk("abc\n222\n\n");
  buffer.AppendChunk("abc\n333\n\n");
  ASSERT_EQ(buffer.num_chunks(), 3);
  EXPECT_EQ(buffer.chunk(0), "abc\n111\n\n");
  EXPECT_EQ(buffer.chunk(1), "abc\n222\n\n");
  EXPECT_EQ(buffer.chunk(2), "abc\n333\n\n");
  ASSERT_EQ(buffer.num_lines(), 9);
  EXPECT_EQ(buffer.line(1), "abc\n");
  EXPECT_EQ(buffer.line(2), "111\n");
  EXPECT_EQ(buffer.line(3), "\n");
  EXPECT_EQ(buffer.line(4), "abc\n");
  EXPECT_EQ(buffer.line(5), "222\n");
  EXPECT_EQ(buffer.line(6), "\n");
  EXPECT_EQ(buffer.line(7), "abc\n");
  EXPECT_EQ(buffer.line(8), "333\n");
  EXPECT_EQ(buffer.line(9), "\n");
}

}  // namespace
}  // namespace frontend
