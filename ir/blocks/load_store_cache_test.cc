#include "ir/blocks/load_store_cache.h"

#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(LoadStoreCache, Caches) {
  LoadStoreCache cache;
  cache.slot<int>(Reg{1}) = Results{3};
  cache.slot<int>(Reg{2}) = Results{Reg::Arg(2)};
  EXPECT_EQ(cache.slot<int>(Reg{1}).get<int>(0), RegOr<int>(3));
  EXPECT_EQ(cache.slot<int>(Reg{2}).get<int>(0), RegOr<int>(Reg::Arg(2)));
}

TEST(LoadStoreCache, ClearAll) {
  LoadStoreCache cache;
  cache.slot<int>(Reg{1}) = Results{3};
  cache.slot<double>(Reg{2}) = Results{3.0};
  cache.clear();
  EXPECT_TRUE(cache.slot<int>(Reg{1}).empty());
  EXPECT_TRUE(cache.slot<double>(Reg{2}).empty());
}

TEST(LoadStoreCache, ClearOne) {
  LoadStoreCache cache;
  cache.slot<int>(Reg{1})    = Results{3};
  cache.slot<double>(Reg{1}) = Results{3.0};
  cache.clear<int>();
  EXPECT_TRUE(cache.slot<int>(Reg{1}).empty());
  EXPECT_EQ(cache.slot<double>(Reg{1}).get<double>(0), RegOr<double>(3.0));
}


}  // namespace
}  // namespace ir
