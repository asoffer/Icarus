#include "ir/overload_set.h"

#include <cstdio>

#include "gtest/gtest.h"
#include "ir/value/foreign_fn.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace {

void TestFn1() { std::puts("TestFn1"); }
void TestFn2() { std::puts("TestFn2"); }
void TestFn3() { std::puts("TestFn3"); }

TEST(OverloadSet, Construction) {
  ir::OverloadSet os(absl::Span<ir::Fn const>{
      ir::ForeignFn(TestFn1, type::Func({}, {})),
      ir::ForeignFn(TestFn2,
                    type::Func({core::AnonymousParam(type::Int64)}, {})),
      ir::ForeignFn(TestFn3,
                    type::Func({core::AnonymousParam(type::Bool)}, {}))});

  {
    core::FnArgs<type::QualType> a;
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn1));
  }
  {
    core::FnArgs<type::QualType> a{{type::QualType::Constant(type::Int64)}, {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn2));
  }
  {
    core::FnArgs<type::QualType> a{{type::QualType::Constant(type::Bool)}, {}};
    ASSIGN_OR(FAIL(), auto fn, os.Lookup(a));
    EXPECT_EQ(fn.foreign().get(), static_cast<void (*)()>(TestFn3));
  }
}

TEST(OverloadSet, FailsToConstruct) {
  ir::OverloadSet os(absl::Span<ir::Fn const>{});

  // Check twice because `Lookup()` caches state.
  EXPECT_FALSE(os.Lookup(core::FnArgs<type::QualType>()).has_value());
  EXPECT_FALSE(os.Lookup(core::FnArgs<type::QualType>()).has_value());
}

}  // namespace
