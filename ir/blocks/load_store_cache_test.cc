#include "ir/blocks/load_store_cache.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

#include "gtest/gtest.h"

namespace {

TEST(LoadStoreCache, Caches) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg{1}) = ir::Value(3);
  cache.slot<int>(ir::Reg{2}) = ir::Value(ir::Reg::Arg(2));
  EXPECT_EQ(cache.slot<int>(ir::Reg{1}).get<ir::RegOr<int>>(),
            ir::RegOr<int>(3));
  EXPECT_EQ(cache.slot<int>(ir::Reg{2}).get<ir::RegOr<int>>(),
            ir::RegOr<int>(ir::Reg::Arg(2)));
}

TEST(LoadStoreCache, ClearAll) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg{1})    = ir::Value(3);
  cache.slot<double>(ir::Reg{2}) = ir::Value(3.0);
  cache.clear();
  EXPECT_TRUE(cache.slot<int>(ir::Reg{1}).empty());
  EXPECT_TRUE(cache.slot<double>(ir::Reg{2}).empty());
}

TEST(LoadStoreCache, ClearOne) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg{1})    = ir::Value(3);
  cache.slot<double>(ir::Reg{1}) = ir::Value(3.0);
  cache.clear<int>();
  EXPECT_TRUE(cache.slot<int>(ir::Reg{1}).empty());
  EXPECT_EQ(cache.slot<double>(ir::Reg{1}).get<ir::RegOr<double>>(),
            ir::RegOr<double>(3.0));
}

}  // namespace
