#include "compiler/verify_assignment_and_initialization.h"

#include "gtest/gtest.h"

#include "type/pointer.h"

namespace {

TEST(ExpansionSize, Matches) {
  error::Log log;

  EXPECT_TRUE(compiler::VerifyInitialization(&log, frontend::SourceRange{},
                                             type::QualType({}, false),
                                             type::QualType({}, true)));
  EXPECT_TRUE(compiler::VerifyAssignment(&log, frontend::SourceRange{},
                                         type::QualType({}, false),
                                         type::QualType({}, true)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{}, type::QualType::NonConstant(type::Int64),
      type::QualType::Constant(type::Int64)));
  EXPECT_TRUE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{}, type::QualType::NonConstant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64, type::Int64}, true)));

  EXPECT_TRUE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64, type::Int64}, true)));

  EXPECT_FALSE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64}, true)));

  EXPECT_FALSE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{},
      type::QualType({type::Int64, type::Float64}, false),
      type::QualType({type::Int64}, true)));

  EXPECT_FALSE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{}, type::QualType({type::Int64}, false),
      type::QualType({type::Int64, type::Int64}, true)));

  EXPECT_FALSE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{}, type::QualType({type::Int64}, false),
      type::QualType({type::Int64, type::Int64}, true)));
}

TEST(Initialization, AllowsConstants) {
  error::Log log;
  EXPECT_TRUE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{}, type::QualType::Constant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_TRUE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{}, type::QualType::Constant(type::Float32),
      type::QualType::Constant(type::Int64)));
}

TEST(Assignment, AllowsConstants) {
  error::Log log;
  EXPECT_FALSE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{}, type::QualType::Constant(type::Int64),
      type::QualType::Constant(type::Int64)));

  EXPECT_FALSE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{}, type::QualType::Constant(type::Float32),
      type::QualType::Constant(type::Int64)));
}

// TODO add a test covering immovable assignment (should fail) and initialization (should pass)

TEST(Casts, AreAllowed) {
  error::Log log;
  EXPECT_TRUE(compiler::VerifyInitialization(
      &log, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));

  EXPECT_TRUE(compiler::VerifyAssignment(
      &log, frontend::SourceRange{},
      type::QualType::NonConstant(type::Ptr(type::Ptr(type::Int64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::Int64)))));
}

}  // namespace
