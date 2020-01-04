#include "compiler/verify_assignment_and_initialization.h"

#include "gtest/gtest.h"

#include "diagnostic/consumer/trivial.h"
#include "type/pointer.h"

namespace {

TEST(ExpansionSize, Matches) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(compiler::VerifyInitialization(diag, frontend::SourceRange{},
                                             type::QualType({}, false),
                                             type::QualType({}, true)));
  EXPECT_TRUE(compiler::VerifyAssignment(diag, frontend::SourceRange{},
                                         type::QualType({}, false),
                                         type::QualType({}, true)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{}, type::QualType::NonConstant(type::Int64),
      type::QualType::Constant(type::Int64)));
  EXPECT_TRUE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{}, type::QualType::NonConstant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64, type::Int64}, true)));

  EXPECT_TRUE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64, type::Int64}, true)));

  EXPECT_EQ(diag.num_consumed(), 0);
  EXPECT_FALSE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64}, true)));
  EXPECT_EQ(diag.num_consumed(), 1);

  EXPECT_FALSE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64}, true)));
  EXPECT_EQ(diag.num_consumed(), 2);

  EXPECT_FALSE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{}, type::QualType({type::Int64}, false),
      type::QualType({type::Int64, type::Int64}, true)));
  EXPECT_EQ(diag.num_consumed(), 3);

  EXPECT_FALSE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{}, type::QualType({type::Int64}, false),
      type::QualType({type::Int64, type::Int64}, true)));
  EXPECT_EQ(diag.num_consumed(), 4);
}

TEST(Initialization, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{}, type::QualType::Constant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{}, type::QualType::Constant(type::Float32),
      type::QualType::Constant(type::Int64)));
}

TEST(Assignment, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_EQ(diag.num_consumed(), 0);

  EXPECT_FALSE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{}, type::QualType::Constant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_EQ(diag.num_consumed(), 1);

  EXPECT_FALSE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{}, type::QualType::Constant(type::Float32),
      type::QualType::Constant(type::Int64)));

  EXPECT_EQ(diag.num_consumed(), 2);
}

// TODO add a test covering immovable assignment (should fail) and
// initialization (should pass)

TEST(Casts, AreAllowed) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(compiler::VerifyInitialization(
      diag, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));

  EXPECT_TRUE(compiler::VerifyAssignment(
      diag, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));
}

}  // namespace