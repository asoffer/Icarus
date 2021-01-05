#include "ir/value/addr.h"

#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(Addr, Heap) {
  int64_t n[10];
  auto a1 = Addr::Heap(&n[0]);
  auto a2 = Addr::Heap(&n[9]);

  EXPECT_EQ(a1 + core::Bytes::Get<int64_t>() * 9, a2);
  EXPECT_EQ(core::Bytes::Get<int64_t>() * 9 + a1, a2);
}

TEST(Addr, Stack) {
  auto a1 = Addr::Stack(16);
  auto a2 = Addr::Stack(32);

  EXPECT_NE(a1, a2);
  EXPECT_EQ(a1 + core::Bytes(16), a2);
  EXPECT_EQ(core::Bytes(16) + a1, a2);
  a1 += core::Bytes(16);
  EXPECT_EQ(a1, a2);
}

TEST(Addr, ReadOnly) {
  auto a1 = Addr::ReadOnly(16);
  auto a2 = Addr::ReadOnly(32);

  EXPECT_NE(a1, a2);
  EXPECT_EQ(a1 + core::Bytes(16), a2);
  EXPECT_EQ(core::Bytes(16) + a1, a2);
  a1 += core::Bytes(16);
  EXPECT_EQ(a1, a2);

  // Stack and read-only are distinct.
  auto s = Addr::Stack(32);
  EXPECT_NE(s, a2);
}

TEST(Addr, ToString) {
  EXPECT_EQ(Addr::Stack(17).to_string(), "s.17");
  EXPECT_EQ(Addr::Heap(nullptr).to_string(), "h.0");
  EXPECT_EQ(Addr::ReadOnly(17).to_string(), "ro.17");
}

TEST(Addr, MoveForwardToAlignment) {
  EXPECT_EQ(ir::Addr::Stack(0).MoveForwardToAlignment(core::Alignment{1}),
            ir::Addr::Stack(0));
  EXPECT_EQ(ir::Addr::Stack(0).MoveForwardToAlignment(core::Alignment{2}),
            ir::Addr::Stack(0));
  EXPECT_EQ(ir::Addr::Stack(1).MoveForwardToAlignment(core::Alignment{2}),
            ir::Addr::Stack(2));
  EXPECT_EQ(ir::Addr::Stack(0).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::Stack(0));
  EXPECT_EQ(ir::Addr::Stack(1).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::Stack(8));
  EXPECT_EQ(ir::Addr::Stack(8).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::Stack(8));

  EXPECT_EQ(ir::Addr::ReadOnly(0).MoveForwardToAlignment(core::Alignment{1}),
            ir::Addr::ReadOnly(0));
  EXPECT_EQ(ir::Addr::ReadOnly(0).MoveForwardToAlignment(core::Alignment{2}),
            ir::Addr::ReadOnly(0));
  EXPECT_EQ(ir::Addr::ReadOnly(1).MoveForwardToAlignment(core::Alignment{2}),
            ir::Addr::ReadOnly(2));
  EXPECT_EQ(ir::Addr::ReadOnly(0).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::ReadOnly(0));
  EXPECT_EQ(ir::Addr::ReadOnly(1).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::ReadOnly(8));
  EXPECT_EQ(ir::Addr::ReadOnly(8).MoveForwardToAlignment(core::Alignment{8}),
            ir::Addr::ReadOnly(8));
}

}  // namespace
}  // namespace ir
