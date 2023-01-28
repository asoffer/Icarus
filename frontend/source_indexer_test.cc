#include "frontend/source_indexer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace frontend {
namespace {

using ::testing::Pair;

TEST(SourceIndexer, Insert) {
  SourceIndexer indexer;
  std::string_view content1 = indexer.insert(data_types::ModuleId(0), R"(abc
  def
  ghi)");

  EXPECT_EQ(content1, R"(abc
  def
  ghi)");

  std::string_view content2 = indexer.insert(data_types::ModuleId(0), "");
  EXPECT_EQ(content2, content1);

  std::string_view content3 = indexer.insert(data_types::ModuleId(1), "abc");
  EXPECT_EQ(content3, "abc");
}

TEST(SourceIndexer, EntryFor) {
  SourceIndexer indexer;
  std::string_view content = indexer.insert(data_types::ModuleId(0), R"(abc
  def
  ghi)");

  std::string_view x = content;
  x.remove_prefix(1);
  EXPECT_EQ(&indexer.EntryFor(content), &indexer.EntryFor(x));
  std::string_view y = content;
  y.remove_suffix(1);
  EXPECT_EQ(&indexer.EntryFor(content), &indexer.EntryFor(y));
  std::string_view z = content;
  z.remove_prefix(1);
  z.remove_suffix(1);
  EXPECT_EQ(&indexer.EntryFor(content), &indexer.EntryFor(z));
}

TEST(SourceIndexer, Lines) {
  SourceIndexer indexer;
  std::string_view content = indexer.insert(data_types::ModuleId(0), R"(abc
  def
  ghi)");

  auto& entry = indexer.EntryFor(content);
  EXPECT_EQ(entry.line(1), "abc");
  EXPECT_EQ(entry.line(2), "  def");
  EXPECT_EQ(entry.line(3), "  ghi");
}

TEST(SourceIndexer, LineContaining) {
  SourceIndexer indexer;
  std::string_view content = indexer.insert(data_types::ModuleId(0), R"(abc
  def
  ghi)");

  auto& entry = indexer.EntryFor(content);
  EXPECT_EQ(entry.line_containing(content.data()), 1);
  EXPECT_EQ(entry.line_containing(content.data() + 3), 1);
  EXPECT_EQ(entry.line_containing(content.data() + 4), 2);
}

TEST(SourceIndexer, LinesContaining) {
  SourceIndexer indexer;
  std::string_view content = indexer.insert(data_types::ModuleId(0), R"(abc
  def
  ghi)");

  auto& entry = indexer.EntryFor(content);
  EXPECT_THAT(entry.lines_containing(std::string_view(content.data(), 1)),
              Pair(1, 2));
  EXPECT_THAT(entry.lines_containing(std::string_view(content.data(), 5)),
              Pair(1, 3));
  EXPECT_THAT(entry.lines_containing(std::string_view(content.data() + 1, 4)),
              Pair(1, 3));
  EXPECT_THAT(entry.lines_containing(std::string_view(content.data() + 3, 1)),
              Pair(1, 2));
  EXPECT_THAT(entry.lines_containing(std::string_view(content.data() + 4, 1)),
              Pair(2, 3));
}

}  // namespace
}  // namespace frontend
