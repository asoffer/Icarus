#include "serialization/read_only_data.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace serialization {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

TEST(ReadOnlyDataAggregator, Empty) {
  ReadOnlyDataAggregator r;
  ReadOnlyData output;
  r.write_to(output);
  EXPECT_THAT(output.strings(), IsEmpty());
}

TEST(ReadOnlyDataAggregator, OneEntry) {
  ReadOnlyDataAggregator r;
  ReadOnlyData input, output;
  *input.add_strings() = "abc";
  r.merge(ModuleIndex(7), input);
  r.write_to(output);
  EXPECT_THAT(output.strings(), ElementsAre("abc"));
  EXPECT_THAT(r.read(ModuleIndex(7), 0), Pair(0, "abc"));
}

TEST(ReadOnlyDataAggregator, MultipleEntries) {
  ReadOnlyDataAggregator r;
  ReadOnlyData input, output;
  *input.add_strings() = "abc";
  *input.add_strings() = "def";
  r.merge(ModuleIndex(7), input);
  r.write_to(output);
  EXPECT_THAT(output.strings(), ElementsAre("abc", "def"));
  EXPECT_THAT(r.read(ModuleIndex(7), 0), Pair(0, "abc"));
  EXPECT_THAT(r.read(ModuleIndex(7), 1), Pair(1, "def"));
}

TEST(ReadOnlyDataAggregator, MultipleModules) {
  ReadOnlyDataAggregator r;
  ReadOnlyData input1, input2, input3, output;
  *input1.add_strings() = "abc";
  *input1.add_strings() = "def";
  *input2.add_strings() = "ghi";
  *input2.add_strings() = "def";
  *input3.add_strings() = "ghi";

  r.merge(ModuleIndex(1), input1);
  r.merge(ModuleIndex(2), input2);
  r.merge(ModuleIndex(3), input3);

  r.write_to(output);
  EXPECT_THAT(output.strings(), ElementsAre("abc", "def", "ghi"));
  EXPECT_THAT(r.read(ModuleIndex(1), 0), Pair(0, "abc"));
  EXPECT_THAT(r.read(ModuleIndex(1), 1), Pair(1, "def"));
  EXPECT_THAT(r.read(ModuleIndex(2), 0), Pair(2, "ghi"));
  EXPECT_THAT(r.read(ModuleIndex(2), 1), Pair(1, "def"));
  EXPECT_THAT(r.read(ModuleIndex(3), 0), Pair(2, "ghi"));
}

}  // namespace serialization
