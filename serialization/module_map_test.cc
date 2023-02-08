#include "serialization/module_map.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace serialization {

using ::testing::Pair;

TEST(ModuleMap, OneEntry) {
  ModuleMap m;
  m.insert(ModuleIndex(7), ModuleIndex(5), UniqueModuleId("abc"));
  EXPECT_THAT(m.read(ModuleIndex(7), ModuleIndex(5)),
              Pair(ModuleIndex(0), UniqueModuleId("abc")));
  EXPECT_EQ(m.get(UniqueModuleId("abc")), ModuleIndex(0));
  EXPECT_EQ(m.get(UniqueModuleId("ghi")), ModuleIndex::Invalid());
}

TEST(ModuleMap, MultipleEntries) {
  ModuleMap m;
  m.insert(ModuleIndex(7), ModuleIndex(5), UniqueModuleId("abc"));
  m.insert(ModuleIndex(2), ModuleIndex(3), UniqueModuleId("def"));
  m.insert(ModuleIndex(2), ModuleIndex(4), UniqueModuleId("abc"));

  EXPECT_THAT(m.read(ModuleIndex(7), ModuleIndex(5)),
              Pair(ModuleIndex(0), UniqueModuleId("abc")));
  EXPECT_THAT(m.read(ModuleIndex(2), ModuleIndex(3)),
              Pair(ModuleIndex(1), UniqueModuleId("def")));
  EXPECT_THAT(m.read(ModuleIndex(2), ModuleIndex(4)),
              Pair(ModuleIndex(0), UniqueModuleId("abc")));
  EXPECT_EQ(m.get(UniqueModuleId("abc")), ModuleIndex(0));
  EXPECT_EQ(m.get(UniqueModuleId("def")), ModuleIndex(1));
  EXPECT_EQ(m.get(UniqueModuleId("ghi")), ModuleIndex::Invalid());
}

}  // namespace serialization
