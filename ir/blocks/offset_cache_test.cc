#include "ir/blocks/offset_cache.h"

#include <optional>

#include "gtest/gtest.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace {

TEST(OffsetCache, Caches) {
  ir::OffsetCache cache;
  EXPECT_EQ(cache.get(ir::Reg{1}, 3, ir::OffsetCache::Kind::Into),
            std::nullopt);
  cache.set(ir::Reg{1}, 3, ir::OffsetCache::Kind::Into, ir::Reg{5});
  EXPECT_EQ(cache.get(ir::Reg{1}, 3, ir::OffsetCache::Kind::Into), ir::Reg{5});
  EXPECT_EQ(cache.get(ir::Reg{1}, 3, ir::OffsetCache::Kind::Passed),
            std::nullopt);
}

}  // namespace
