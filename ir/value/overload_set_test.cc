#include "ir/value/overload_set.h"

#include <cstdio>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/foreign_fn.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace {
using testing::Optional;

void TestFn1() { std::puts("TestFn1"); }
void TestFn2() { std::puts("TestFn2"); }
void TestFn3() { std::puts("TestFn3"); }

TEST(OverloadSet, Construction) {
  ir::OverloadSet os(std::vector<ir::Fn>{
      ir::ForeignFn(TestFn1, type::Func({}, {})),
      ir::ForeignFn(TestFn2,
                    type::Func({core::AnonymousParam(
                                   type::QualType::NonConstant(type::Int64))},
                               {})),
      ir::ForeignFn(TestFn3,
                    type::Func({core::AnonymousParam(
                                   type::QualType::NonConstant(type::Bool))},
                               {}))});

  {
    core::Arguments<type::QualType> a;
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn1));
  }
  {
    core::Arguments<type::QualType> a{{type::QualType::Constant(type::Int64)},
                                      {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn2));
  }
  {
    core::Arguments<type::QualType> a{{type::QualType::Constant(type::Bool)},
                                      {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn3));
  }
}

TEST(OverloadSet, FailsToConstruct) {
  ir::OverloadSet os(std::vector<ir::Fn>{});

  // Check twice because `Lookup()` caches state.
  EXPECT_FALSE(os.Lookup(core::Arguments<type::QualType>()).has_value());
  EXPECT_FALSE(os.Lookup(core::Arguments<type::QualType>()).has_value());
}

}  // namespace
