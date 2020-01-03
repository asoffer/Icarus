#include "type/qual_type.h"

#include "gtest/gtest.h"
#include "type/primitive.h"

namespace {

TEST(QualType, Regularity) {
  EXPECT_EQ(type::QualType::Error(), type::QualType::Error());

  EXPECT_EQ(type::QualType::Constant(type::Int32),
            type::QualType::Constant(type::Int32));
  EXPECT_NE(type::QualType::Constant(type::Int32),
            type::QualType::Constant(type::Int64));

  EXPECT_EQ(type::QualType::NonConstant(type::Int32),
            type::QualType::NonConstant(type::Int32));
  EXPECT_NE(type::QualType::NonConstant(type::Int32),
            type::QualType::Constant(type::Int32));
  EXPECT_NE(type::QualType::NonConstant(type::Int32),
            type::QualType::NonConstant(type::Int64));

  EXPECT_EQ(type::QualType::NonConstant(type::Int32),
            type::QualType({type::Int32}, false));

  EXPECT_EQ(type::QualType::Constant(type::Int32),
            type::QualType({type::Int32}, true));

  EXPECT_EQ(type::QualType({type::Int32, type::Bool}, true),
            type::QualType({type::Int32, type::Bool}, true));
  EXPECT_NE(type::QualType({type::Int32, type::Bool}, false),
            type::QualType({type::Int32, type::Bool}, true));

  auto q = type::QualType::Constant(type::Int64);

  EXPECT_EQ(q, type::QualType::Constant(type::Int64));

  EXPECT_NE(q, type::QualType::Constant(type::Bool));
  q = type::QualType::Constant(type::Bool);
  EXPECT_EQ(q, type::QualType::Constant(type::Bool));
}

TEST(QualType, Access) {
  auto q = type::QualType::Constant(type::Int32);
  EXPECT_TRUE(q.constant());
  EXPECT_EQ(q.type(), type::Int32);

  auto q2 = type::QualType::NonConstant(type::Bool);
  EXPECT_FALSE(q2.constant());
  EXPECT_EQ(q2.type(), type::Bool);
}

TEST(QualType, Streaming) {
  {
    std::stringstream ss;
    auto q = type::QualType::Constant(type::Int32);
    ss << q;
    EXPECT_EQ(ss.str(), "const(int32)");
  }

  {
    std::stringstream ss;
    auto q = type::QualType::NonConstant(type::Bool);
    ss << q;
    EXPECT_EQ(ss.str(), "non-const(bool)");
  }


  {
    std::stringstream ss;
    auto q = type::QualType({type::Int64, type::Bool}, true);
    ss << q;
    EXPECT_EQ(ss.str(), "const(int64, bool)");
  }
}

TEST(QualType, Ok) {
  EXPECT_FALSE(type::QualType::Error().ok());
  EXPECT_FALSE(type::QualType::Error());
  EXPECT_TRUE(type::QualType::Constant(type::Bool).ok());
  EXPECT_TRUE(type::QualType::Constant(type::Bool));
}

TEST(QualType, ExpansionSize) {
  EXPECT_EQ(type::QualType::Constant(type::Int32).expansion_size(), 1);
  EXPECT_EQ(type::QualType({type::Int32}, true).expansion_size(), 1);

  EXPECT_EQ(type::QualType({}, true).expansion_size(), 0);
  EXPECT_EQ(type::QualType({type::Int32, type::Bool}, true).expansion_size(),
            2);
}

}  // namespace
