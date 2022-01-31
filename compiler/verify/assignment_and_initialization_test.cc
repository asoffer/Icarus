#include "compiler/verify/assignment_and_initialization.h"

#include "diagnostic/consumer/trivial.h"
#include "gtest/gtest.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace compiler::internal {
namespace {

TEST(ExpansionSize, Matches) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(diag, "",
                                   type::QualType::NonConstant(type::I64),
                                   type::QualType::Constant(type::I64)));
  EXPECT_TRUE(VerifyAssignment(diag, "", type::QualType::NonConstant(type::I64),
                               type::QualType::Constant(type::I64)));
}

TEST(Initialization, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(diag, "",
                                   type::QualType::Constant(type::I64),
                                   type::QualType::Constant(type::I64)));

  EXPECT_FALSE(VerifyInitialization(diag, "",
                                    type::QualType::Constant(type::F32),
                                    type::QualType::Constant(type::I64)));
}

TEST(Assignment, AllowsConstants) {
  diagnostic::TrivialConsumer diag;

  EXPECT_EQ(diag.num_consumed(), 0);

  EXPECT_FALSE(VerifyAssignment(diag, "", type::QualType::Constant(type::I64),
                                type::QualType::Constant(type::I64)));

  EXPECT_EQ(diag.num_consumed(), 1);

  EXPECT_FALSE(VerifyAssignment(diag, "", type::QualType::Constant(type::F32),
                                type::QualType::Constant(type::I64)));

  EXPECT_EQ(diag.num_consumed(), 2);
}

// TODO add a test covering immovable assignment (should fail) and
// initialization (should pass)

TEST(Casts, AreAllowed) {
  diagnostic::TrivialConsumer diag;

  EXPECT_TRUE(VerifyInitialization(
      diag, "", type::QualType::NonConstant(type::Ptr(type::Ptr(type::I64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::I64)))));

  EXPECT_TRUE(VerifyAssignment(
      diag, "", type::QualType::NonConstant(type::Ptr(type::Ptr(type::I64))),
      type::QualType::NonConstant(type::BufPtr(type::BufPtr(type::I64)))));
}

}  // namespace
}  // namespace compiler::internal
