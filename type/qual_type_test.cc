#include "type/qual_type.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"

namespace {

using ::testing::MockFunction;

// TODO test calls to quals() and check kRef

TEST(QualType, Regularity) {
  EXPECT_EQ(type::QualType::Error(), type::QualType::Error());

  EXPECT_EQ(type::QualType::Constant(type::I32),
            type::QualType::Constant(type::I32));
  EXPECT_NE(type::QualType::Constant(type::I32),
            type::QualType::Constant(type::I64));

  EXPECT_EQ(type::QualType::NonConstant(type::I32),
            type::QualType::NonConstant(type::I32));
  EXPECT_NE(type::QualType::NonConstant(type::I32),
            type::QualType::Constant(type::I32));
  EXPECT_NE(type::QualType::NonConstant(type::I32),
            type::QualType::NonConstant(type::I64));

  EXPECT_EQ(type::QualType::NonConstant(type::I32),
            type::QualType(type::I32, type::Qualifiers::Unqualified()));

  EXPECT_EQ(type::QualType::Constant(type::I32),
            type::QualType(type::I32, type::Qualifiers::Constant()));

  auto q = type::QualType::Constant(type::I64);

  EXPECT_EQ(q, type::QualType::Constant(type::I64));

  EXPECT_NE(q, type::QualType::Constant(type::Bool));
  q = type::QualType::Constant(type::Bool);
  EXPECT_EQ(q, type::QualType::Constant(type::Bool));
}

TEST(QualType, Access) {
  auto q = type::QualType::Constant(type::I32);
  EXPECT_TRUE(q.constant());
  EXPECT_EQ(q.type(), type::I32);

  auto q2 = type::QualType::NonConstant(type::Bool);
  EXPECT_FALSE(q2.constant());
  EXPECT_EQ(q2.type(), type::Bool);
}

TEST(QualType, Streaming) {
  {
    std::stringstream ss;
    auto q = type::QualType::Constant(type::I32);
    ss << q;
    EXPECT_EQ(ss.str(), "[const](i32)");
  }

  {
    std::stringstream ss;
    auto q = type::QualType::NonConstant(type::Bool);
    ss << q;
    EXPECT_EQ(ss.str(), "[](bool)");
  }
}

TEST(QualType, Ok) {
  EXPECT_FALSE(type::QualType::Error().ok());
  EXPECT_FALSE(type::QualType::Error());
  EXPECT_TRUE(type::QualType::Constant(type::Bool).ok());
  EXPECT_TRUE(type::QualType::Constant(type::Bool));
}

TEST(QualType, RemoveConstant) {
  {
    type::QualType qt(type::I32, type::Qualifiers::All());
    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Qualifiers::All() & ~type::Qualifiers::Constant());

    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Qualifiers::All() & ~type::Qualifiers::Constant());
  }

  {
    type::QualType qt(type::I32, type::Qualifiers::Constant());
    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Qualifiers::Unqualified());

    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Qualifiers::Unqualified());
  }
}

TEST(QualType, Error) {
  type::QualType qt(type::I32, type::Qualifiers::All());
  EXPECT_FALSE(qt.HasErrorMark());
  qt.MarkError();
  EXPECT_TRUE(qt.HasErrorMark());

  auto copy = qt;
  EXPECT_TRUE(qt.HasErrorMark());
}

}  // namespace
