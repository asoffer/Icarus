#include "type/type.h"

#include <string>

#include "core/arch.h"
#include "gtest/gtest.h"

namespace {
struct FakeType : type::LegacyType {
  ~FakeType() override {}

  explicit FakeType(uint8_t n)
      : type::LegacyType(type::IndexOf<FakeType>(),
                         type::LegacyType::Flags{
                             .is_default_initializable = (n & 1) != 0,
                             .is_copyable              = (n & 2) != 0,
                             .is_movable               = (n & 4) != 0,
                             .has_destructor           = (n & 8) != 0,
                         }) {}

  void Accept(type::VisitorBase *visitor, void *ret,
              void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  type::Completeness completeness() const override {
    return type::Completeness::Complete;
  }

  void WriteTo(std::string *out) const override { out->append("[[FakeType]]"); }
  core::Bytes bytes(core::Arch const &arch) const override {
    return core::Bytes{17};
  }
  core::Alignment alignment(core::Arch const &arch) const override {
    return core::Alignment{2};
  }
};

TEST(LegacyType, Flags) {
  FakeType f1(0);
  EXPECT_FALSE(f1.IsDefaultInitializable());
  EXPECT_FALSE(f1.IsCopyable());
  EXPECT_FALSE(f1.IsMovable());
  EXPECT_FALSE(f1.HasDestructor());

  FakeType f2(6);
  EXPECT_FALSE(f2.IsDefaultInitializable());
  EXPECT_TRUE(f2.IsCopyable());
  EXPECT_TRUE(f2.IsMovable());
  EXPECT_FALSE(f2.HasDestructor());

  FakeType f3(31);
  EXPECT_TRUE(f3.IsDefaultInitializable());
  EXPECT_TRUE(f3.IsCopyable());
  EXPECT_TRUE(f3.IsMovable());
  EXPECT_TRUE(f3.HasDestructor());
}

TEST(LegacyType, WriteTo) {
  EXPECT_EQ(FakeType(0).to_string(), "[[FakeType]]");
}

TEST(LegacyType, Layout) {
  EXPECT_EQ(FakeType(0).bytes(core::Host), core::Bytes{17});
  EXPECT_EQ(FakeType(0).alignment(core::Host), core::Alignment{2});
}

}  // namespace
