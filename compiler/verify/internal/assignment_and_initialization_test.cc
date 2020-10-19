#include "compiler/verify/internal/assignment_and_initialization.h"

#include "gtest/gtest.h"

#include "diagnostic/consumer/trivial.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace compiler::internal {
namespace {

TEST(ExpansionSize, Matches) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType(absl::Span<type::Type const>{},
                     type::Quals::Unqualified()),
      type::QualType(absl::Span<type::Type const>{}, type::Quals::Const())));
  EXPECT_TRUE(VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType(absl::Span<type::Type const>{},
                     type::Quals::Unqualified()),
      type::QualType(absl::Span<type::Type const>{}, type::Quals::Const())));

  EXPECT_TRUE(VerifyInitialization(diag, frontend::SourceRange{},
                                   type::QualType::NonConstant(type::Int64),
                                   type::QualType::Constant(type::Int64)));
  EXPECT_TRUE(VerifyAssignment(diag, frontend::SourceRange{},
                               type::QualType::NonConstant(type::Int64),
                               type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, type::Quals::Unqualified()),
      type::QualType({type::Int64, type::Int64}, type::Quals::Const())));

  EXPECT_TRUE(VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, type::Quals::Unqualified()),
      type::QualType({type::Int64, type::Int64}, type::Quals::Const())));

  EXPECT_EQ(diag.num_consumed(), 0);
  EXPECT_FALSE(VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, type::Quals::Unqualified()),
      type::QualType({type::Int64}, type::Quals::Const())));
  EXPECT_EQ(diag.num_consumed(), 1);

  EXPECT_FALSE(VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, type::Quals::Unqualified()),
      type::QualType({type::Int64}, type::Quals::Const())));
  EXPECT_EQ(diag.num_consumed(), 2);

  EXPECT_FALSE(VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64}, type::Quals::Unqualified()),
      type::QualType({type::Int64, type::Int64}, type::Quals::Const())));
  EXPECT_EQ(diag.num_consumed(), 3);

  EXPECT_FALSE(VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64}, type::Quals::Unqualified()),
      type::QualType({type::Int64, type::Int64}, type::Quals::Const())));
  EXPECT_EQ(diag.num_consumed(), 4);
}

TEST(Initialization, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(diag, frontend::SourceRange{},
                                   type::QualType::Constant(type::Int64),
                                   type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(VerifyInitialization(diag, frontend::SourceRange{},
                                   type::QualType::Constant(type::Float32),
                                   type::QualType::Constant(type::Int64)));
}

TEST(Assignment, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_EQ(diag.num_consumed(), 0);

  EXPECT_FALSE(VerifyAssignment(diag, frontend::SourceRange{},
                                type::QualType::Constant(type::Int64),
                                type::QualType::Constant(type::Int64)));

  EXPECT_EQ(diag.num_consumed(), 1);

  EXPECT_FALSE(VerifyAssignment(diag, frontend::SourceRange{},
                                type::QualType::Constant(type::Float32),
                                type::QualType::Constant(type::Int64)));

  EXPECT_EQ(diag.num_consumed(), 2);
}

// TODO add a test covering immovable assignment (should fail) and
// initialization (should pass)

TEST(Casts, AreAllowed) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));

  EXPECT_TRUE(VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));
}

}  // namespace
}  // namespace compiler::internal
