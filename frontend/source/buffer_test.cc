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
  EXPECT_EQ(buffer.line(LineNum(1)), "abc\n");
}

TEST(SourceBuffer, OneChunkMultipleLines) {
  SourceBuffer buffer("abc\ndef\n");
  ASSERT_EQ(buffer.num_chunks(), 1);
  EXPECT_EQ(buffer.chunk(0), "abc\ndef\n");
  ASSERT_EQ(buffer.num_lines(), 2);
  EXPECT_EQ(buffer.line(LineNum(1)), "abc\n");
  EXPECT_EQ(buffer.line(LineNum(2)), "def\n");
}

TEST(SourceBuffer, OneChunkEndingWithMultipleNewlines) {
  SourceBuffer buffer("abc\ndef\n\n");
  ASSERT_EQ(buffer.num_chunks(), 1);
  EXPECT_EQ(buffer.chunk(0), "abc\ndef\n\n");
  ASSERT_EQ(buffer.num_lines(), 3);
  EXPECT_EQ(buffer.line(LineNum(1)), "abc\n");
  EXPECT_EQ(buffer.line(LineNum(2)), "def\n");
  EXPECT_EQ(buffer.line(LineNum(3)), "\n");
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
  EXPECT_EQ(buffer.line(LineNum(1)), "abc\n");
  EXPECT_EQ(buffer.line(LineNum(2)), "111\n");
  EXPECT_EQ(buffer.line(LineNum(3)), "\n");
  EXPECT_EQ(buffer.line(LineNum(4)), "abc\n");
  EXPECT_EQ(buffer.line(LineNum(5)), "222\n");
  EXPECT_EQ(buffer.line(LineNum(6)), "\n");
  EXPECT_EQ(buffer.line(LineNum(7)), "abc\n");
  EXPECT_EQ(buffer.line(LineNum(8)), "333\n");
  EXPECT_EQ(buffer.line(LineNum(9)), "\n");
}

TEST(SourceBuffer, LineNumberAndOffset) {
  SourceBuffer buffer("abc\ndef\n\nghi");
  SourceLoc loc(0, 0);
  EXPECT_EQ(buffer.line_number(loc), LineNum(1));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(0));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(1), Offset(0)));

  loc = SourceLoc(0, 1);
  EXPECT_EQ(buffer.line_number(loc), LineNum(1));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(1));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(1), Offset(1)));

  loc = SourceLoc(0, 3);
  EXPECT_EQ(buffer.line_number(loc), LineNum(1));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(3));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(1), Offset(3)));

  loc = SourceLoc(0, 4);
  EXPECT_EQ(buffer.line_number(loc), LineNum(2));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(0));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(2), Offset(0)));

  loc = SourceLoc(0, 7);
  EXPECT_EQ(buffer.line_number(loc), LineNum(2));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(3));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(2), Offset(3)));

  loc = SourceLoc(0, 8);
  EXPECT_EQ(buffer.line_number(loc), LineNum(3));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(0));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(3), Offset(0)));

  loc = SourceLoc(0, 9);
  EXPECT_EQ(buffer.line_number(loc), LineNum(4));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(0));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(4), Offset(0)));

  loc = SourceLoc(0, 10);
  EXPECT_EQ(buffer.line_number(loc), LineNum(4));
  EXPECT_EQ(buffer.offset_in_line(loc), Offset(1));
  EXPECT_EQ(buffer.line_and_offset(loc), std::pair(LineNum(4), Offset(1)));
}

TEST(SourceBuffer, Range) {
  SourceBuffer buffer("abc\ndef\n\nghi");
  SourceRange range(SourceLoc(0, 1), SourceLoc(0, 5));
  EXPECT_EQ(buffer[range], "bc\nd");
}

TEST(SourceBuffer, ConsumeChunkWhile) {
  SourceBuffer buffer("abc\ndef\n\nghi");
  {
    SourceLoc loc(0, 0);
    EXPECT_EQ(buffer.ConsumeChunkWhile(loc, [](char c) { return c != 'e'; }),
              std::pair(SourceRange(SourceLoc(0, 0), SourceLoc(0, 5)),
                        std::string_view("abc\nd")));
    EXPECT_EQ(loc, SourceLoc(0, 5));
  }

  {
    SourceLoc loc(0, 1);
    EXPECT_EQ(buffer.ConsumeChunkWhile(loc, [](char) { return false; }),
              std::pair(SourceRange(SourceLoc(0, 1), SourceLoc(0, 1)), std::string_view("")));
    EXPECT_EQ(loc, SourceLoc(0, 1));
  }
}

TEST(SourceBuffer, Indexing) {
  SourceBuffer buffer("abc\ndef\n\nghi");
  EXPECT_EQ(buffer[SourceLoc(0, 1)], 'b');
  EXPECT_EQ(buffer[SourceLoc(0, 5)], 'e');
  EXPECT_EQ(buffer[SourceRange(SourceLoc(0, 1), SourceLoc(0, 5))], "bc\nd");
  EXPECT_EQ(buffer[SourceRange(SourceLoc(0, 1), SourceLoc(0, 1))], "");
}

TEST(SourceLoc, Offset) {
  EXPECT_EQ(SourceLoc(0, 5) - SourceLoc(0, 3), Offset(2));
  EXPECT_EQ(SourceLoc(2, 3) - SourceLoc(2, 3), Offset(0));
  EXPECT_EQ(SourceLoc(2, 3) - SourceLoc(2, 5), Offset(-2));
}

}  // namespace
}  // namespace frontend
