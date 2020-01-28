#include "ir/overload_set.h"

#include <cstdio>

#include "gtest/gtest.h"
#include "ir/foreign_fn.h"
#include "type/primitive.h"

namespace {

void TestFn1() { std::puts("TestFn1"); }
void TestFn2() { std::puts("TestFn2"); }
void TestFn3() { std::puts("TestFn3"); }

TEST(OverloadSet, Construction) {
  ir::OverloadSet os(absl::Span<ir::AnyFunc const>{
      ir::ForeignFn(TestFn1, type::Func({}, {})),
      ir::ForeignFn(TestFn2,
                    type::Func({core::AnonymousParam(type::Int64)}, {})),
      ir::ForeignFn(TestFn3,
                    type::Func({core::AnonymousParam(type::Bool)}, {}))});

  {
    core::FnParams<type::Type const *> p;
    auto fn = os[p];
    ASSERT_TRUE(fn.has_value());
    EXPECT_EQ(fn->foreign().get(), static_cast<void (*)()>(TestFn1));
  }
  {
    core::FnParams<type::Type const *> p{core::AnonymousParam(type::Int64)};
    auto fn = os[p];
    ASSERT_TRUE(fn.has_value());
    EXPECT_EQ(fn->foreign().get(), static_cast<void (*)()>(TestFn2));
  }
  {
    core::FnParams<type::Type const *> p{core::AnonymousParam(type::Bool)};
    auto fn = os[p];
    ASSERT_TRUE(fn.has_value());
    EXPECT_EQ(fn->foreign().get(), static_cast<void (*)()>(TestFn3));
  }
}

TEST(OverloadSet, Callable) {
  ir::OverloadSet os(absl::Span<ir::AnyFunc const>{},
                     [](core::FnParams<type::Type const *> const &)
                         -> std::optional<ir::AnyFunc> {
                       return ir::ForeignFn(TestFn1, type::Func({}, {}));
                     });
  auto fn = os[core::FnParams<type::Type const *>()];
  ASSERT_TRUE(fn.has_value());
  ASSERT_FALSE(fn->is_fn());
  EXPECT_EQ(fn->foreign().get(), static_cast<void (*)()>(TestFn1));
}

TEST(OverloadSet, FailsToConstruct) {
  ir::OverloadSet os(absl::Span<ir::AnyFunc const>{});

  // Check twice because operator[] caches state.
  EXPECT_FALSE(os[core::FnParams<type::Type const *>()].has_value());
  EXPECT_FALSE(os[core::FnParams<type::Type const *>()].has_value());
}

}  // namespace