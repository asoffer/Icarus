#include "type_contour.h"

#include "gtest/gtest.h"

namespace {

TEST(TypeContour, DefaultConstructor) {
  core::TypeContour contour;
  EXPECT_EQ(contour.bytes(), core::Bytes{0});
  EXPECT_EQ(contour.alignment(), core::Alignment{1});
}

TEST(TypeContour, Get) {
  {
    auto contour = core::TypeContour::Get<char>();
    EXPECT_EQ(contour.bytes(), core::Bytes::Get<char>());
    EXPECT_EQ(contour.alignment(), core::Alignment::Get<char>());
  }
  {
    auto contour = core::TypeContour::Get<int>();
    EXPECT_EQ(contour.bytes(), core::Bytes::Get<int>());
    EXPECT_EQ(contour.alignment(), core::Alignment::Get<int>());
  }
  {
    auto contour = core::TypeContour::Get<bool>();
    EXPECT_EQ(contour.bytes(), core::Bytes::Get<bool>());
    EXPECT_EQ(contour.alignment(), core::Alignment::Get<bool>());
  }
  {
    auto contour = core::TypeContour::Get<void *>();
    EXPECT_EQ(contour.bytes(), core::Bytes::Get<void *>());
    EXPECT_EQ(contour.alignment(), core::Alignment::Get<void *>());
  }
  {
    auto contour = core::TypeContour::Get<void (*)()>();
    EXPECT_EQ(contour.bytes(), core::Bytes::Get<void (*)()>());
    EXPECT_EQ(contour.alignment(), core::Alignment::Get<void (*)()>());
  }
}

TEST(TypeContour, Comparison) {
  EXPECT_EQ(core::TypeContour::Get<int64_t>(),
            core::TypeContour::Get<int64_t>());
  EXPECT_EQ(core::TypeContour::Get<int64_t>(),
            core::TypeContour::Get<uint64_t>());
  EXPECT_NE(core::TypeContour::Get<int64_t>(),
            core::TypeContour::Get<int32_t>());
}

}  // namespace
