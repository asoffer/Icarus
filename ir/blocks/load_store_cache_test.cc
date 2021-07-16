#include "ir/blocks/load_store_cache.h"

#include "gtest/gtest.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace {

TEST(LoadStoreCache, Caches) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg(1)).first = 3;
  cache.slot<int>(ir::Reg(2)).first = ir::Reg::Arg(2);
  EXPECT_EQ(cache.slot<int>(ir::Reg(1)).first.get(), ir::RegOr<int>(3));
  EXPECT_EQ(cache.slot<int>(ir::Reg(2)).first.get(),
            ir::RegOr<int>(ir::Reg::Arg(2)));
}

TEST(LoadStoreCache, ClearAll) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg(1)).first    = 3;
  cache.slot<double>(ir::Reg(2)).first = 3.0;
  cache.clear();
  EXPECT_TRUE(cache.slot<int>(ir::Reg(1)).second);
  EXPECT_TRUE(cache.slot<double>(ir::Reg(2)).second);
}

TEST(LoadStoreCache, ClearOne) {
  ir::LoadStoreCache cache;
  cache.slot<int>(ir::Reg(1)).first    = 3;
  cache.slot<double>(ir::Reg(1)).first = 3.0;
  cache.clear<int>();
  EXPECT_TRUE(cache.slot<int>(ir::Reg(1)).second);
  EXPECT_EQ(cache.slot<double>(ir::Reg(1)).first.get(), ir::RegOr<double>(3.0));
}

}  // namespace
