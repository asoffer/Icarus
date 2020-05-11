#include "type/pointer.h"

#include "gtest/gtest.h"
#include "type/fake.h"

namespace {

TEST(Pointer, Construction) {
  type::Type const *t1 = type::Fake<1, 2>();
  type::Type const *t2 = type::Fake<2, 2>();
  EXPECT_EQ(type::Ptr(t1), type::Ptr(t1));
  EXPECT_EQ(type::Ptr(t2), type::Ptr(t2));
  EXPECT_NE(type::Ptr(t1), type::Ptr(t2));

  EXPECT_EQ(type::Ptr(t1)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::Ptr(t1)->alignment(core::Host),
            core::Host.pointer().alignment());
  EXPECT_EQ(type::Ptr(t2)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::Ptr(t2)->alignment(core::Host),
            core::Host.pointer().alignment());
}

TEST(BufferPointer, Construction) {
  type::Type const *t1 = type::Fake<1, 2>();
  type::Type const *t2 = type::Fake<2, 2>();
  EXPECT_EQ(type::BufPtr(t1), type::BufPtr(t1));
  EXPECT_EQ(type::BufPtr(t2), type::BufPtr(t2));
  EXPECT_NE(type::BufPtr(t1), type::BufPtr(t2));

  EXPECT_EQ(type::BufPtr(t1)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::BufPtr(t1)->alignment(core::Host),
            core::Host.pointer().alignment());
  EXPECT_EQ(type::BufPtr(t2)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::BufPtr(t2)->alignment(core::Host),
            core::Host.pointer().alignment());
}

TEST(Pointer, ToString) {
  type::Type const *t = type::Fake<1, 2>();
  EXPECT_EQ(type::Ptr(t)->to_string(), "*(Fake(1, 2))");
  EXPECT_EQ(type::BufPtr(t)->to_string(), "[*](Fake(1, 2))");
  EXPECT_EQ(type::Ptr(type::Ptr(t))->to_string(), "*(*(Fake(1, 2)))");
  EXPECT_EQ(type::Ptr(type::BufPtr(t))->to_string(), "*([*](Fake(1, 2)))");
  EXPECT_EQ(type::BufPtr(type::Ptr(t))->to_string(), "[*](*(Fake(1, 2)))");
  EXPECT_EQ(type::BufPtr(type::BufPtr(t))->to_string(), "[*]([*](Fake(1, 2)))");
}

}  // namespace
