#include "type/interface/interface.h"

#include "gtest/gtest.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type.h"

namespace interface {
namespace {

TEST(Interface, Equality) {
  EXPECT_EQ(Interface::Just(type::I64), Interface::Just(type::I64));
  EXPECT_NE(Interface::Just(type::I64), Interface::ConvertsTo(type::I64));
  EXPECT_NE(Interface::Just(type::I64), Interface::Just(type::U64));
}

TEST(Satisfiability, Just) {
  EXPECT_TRUE(Interface::Just(type::I64).SatisfiedBy(type::I64));
  EXPECT_FALSE(Interface::Just(type::I64).SatisfiedBy(type::U64));
}

TEST(Satisfiability, ConvertsTo) {
  EXPECT_TRUE(Interface::ConvertsTo(type::Ptr(type::I64))
                  .SatisfiedBy(type::Ptr(type::I64)));
  EXPECT_TRUE(Interface::ConvertsTo(type::Ptr(type::I64))
                  .SatisfiedBy(type::BufPtr(type::I64)));
  EXPECT_TRUE(
      Interface::ConvertsTo(type::Ptr(type::I64)).SatisfiedBy(type::I64));
}

TEST(Satisfiability, Callable) {
  EXPECT_TRUE(Interface::Callable(
                  core::Arguments<type::Type>({type::Ptr(type::I64)}, {}))
                  .SatisfiedBy(type::Func(
                      core::Parameters<type::QualType>({core::AnonymousParameter(
                          type::QualType::NonConstant(type::Ptr(type::I64)))}),
                      {})));
  EXPECT_TRUE(Interface::Callable(
                  core::Arguments<type::Type>({type::BufPtr(type::I64)}, {}))
                  .SatisfiedBy(type::Func(
                      core::Parameters<type::QualType>({core::AnonymousParameter(
                          type::QualType::NonConstant(type::Ptr(type::I64)))}),
                      {})));
  EXPECT_FALSE(
      Interface::Callable(
          core::Arguments<type::Type>({type::Ptr(type::I64)}, {}))
          .SatisfiedBy(type::Func(
              core::Parameters<type::QualType>({core::AnonymousParameter(
                  type::QualType::NonConstant(type::BufPtr(type::I64)))}),
              {})));
}

}  // namespace
}  // namespace interface
