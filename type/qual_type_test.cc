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
            type::QualType({type::I32}, type::Quals::Unqualified()));

  EXPECT_EQ(type::QualType::Constant(type::I32),
            type::QualType({type::I32}, type::Quals::Const()));

  EXPECT_EQ(type::QualType({type::I32, type::Bool}, type::Quals::Const()),
            type::QualType({type::I32, type::Bool}, type::Quals::Const()));
  EXPECT_NE(
      type::QualType({type::I32, type::Bool}, type::Quals::Unqualified()),
      type::QualType({type::I32, type::Bool}, type::Quals::Const()));

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

  {
    std::stringstream ss;
    auto q = type::QualType({type::I64, type::Bool}, type::Quals::Const());
    ss << q;
    EXPECT_EQ(ss.str(), "[const](i64, bool)");
  }
}

TEST(QualType, Ok) {
  EXPECT_FALSE(type::QualType::Error().ok());
  EXPECT_FALSE(type::QualType::Error());
  EXPECT_TRUE(type::QualType::Constant(type::Bool).ok());
  EXPECT_TRUE(type::QualType::Constant(type::Bool));
}

TEST(QualType, ExpansionSize) {
  EXPECT_EQ(type::QualType::Constant(type::I32).expansion_size(), 1);
  EXPECT_EQ(
      type::QualType({type::I32}, type::Quals::Const()).expansion_size(), 1);

  EXPECT_EQ(type::QualType(absl::Span<type::Type const>{}, type::Quals::Const())
                .expansion_size(),
            0);
  EXPECT_EQ(type::QualType({type::I32, type::Bool}, type::Quals::Const())
                .expansion_size(),
            2);
}

TEST(QualType, ForEach) {
  {
    MockFunction<void(type::Type)> mock_fn;
    EXPECT_CALL(mock_fn, Call(type::Bool)).Times(1);
    type::QualType::Constant(type::Bool).ForEach(mock_fn.AsStdFunction());
  }

  {
    MockFunction<void(type::Type)> mock_fn;
    EXPECT_CALL(mock_fn, Call(type::Bool)).Times(1);
    EXPECT_CALL(mock_fn, Call(type::I64)).Times(1);
    type::QualType({type::Bool, type::I64}, type::Quals::Const())
        .ForEach(mock_fn.AsStdFunction());
  }
}

TEST(QualType, RemoveConstant) {
  {
    type::QualType qt(type::I32, type::Quals::All());
    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Quals::All() & ~type::Quals::Const());

    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Quals::All() & ~type::Quals::Const());
  }

  {
    type::QualType qt(type::I32, type::Quals::Const());
    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Quals::Unqualified());

    qt.remove_constant();
    EXPECT_NE(qt, type::QualType::Error());
    EXPECT_EQ(qt.quals(), type::Quals::Unqualified());
  }
}

TEST(QualType, EmptyVector) {
  type::QualType qt(std::vector<type::Type>{}, type::Quals::Unqualified());
  EXPECT_TRUE(qt.ok());
}

TEST(QualType, Error) {
  type::QualType qt(type::I32, type::Quals::All());
  EXPECT_FALSE(qt.HasErrorMark());
  qt.MarkError();
  EXPECT_TRUE(qt.HasErrorMark());

  auto copy = qt;
  EXPECT_TRUE(qt.HasErrorMark());
}

}  // namespace
