#include "data_types/integer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace data_types {

using ::testing::UnorderedElementsAre;

IntegerTable RoundTrip(IntegerTable const& table) {
  IntegerTable result;
  serialization::IntegerTable proto;
  Serialize(table, proto);
  Deserialize(proto, result);
  return result;
}

TEST(IntegerTable, Empty) {
  IntegerTable table;
  EXPECT_THAT(RoundTrip(table), UnorderedElementsAre());
}

TEST(IntegerTable, OneElement) {
  {
    IntegerTable table;
    table.insert(0);
    EXPECT_THAT(RoundTrip(table), UnorderedElementsAre(0));
  }
  {
    IntegerTable table;
    table.insert(1);
    EXPECT_THAT(RoundTrip(table), UnorderedElementsAre(1));
  }
  {
    IntegerTable table;
    table.insert(-1);
    EXPECT_THAT(RoundTrip(table), UnorderedElementsAre(-1));
  }
}

TEST(IntegerTable, MultipleElements) {
  IntegerTable table;
  table.insert(0);
  table.insert(-1);
  table.insert(17);
  table.insert(std::numeric_limits<intptr_t>::max());
  EXPECT_THAT(
      RoundTrip(table),
      UnorderedElementsAre(-1, 0, 17, std::numeric_limits<intptr_t>::max()));
}

}  // namespace data_types
