#include "ir/overload_set.h"

#include <cstdio>

#include "gtest/gtest.h"
#include "ir/foreign_fn.h"
#include "type/primitive.h"

namespace {

void TestFn1() { std::puts("TestFn1"); }
void TestFn2() { std::puts("TestFn2"); }
void TestFn3() { std::puts("TestFn3"); }
void TestFn4() { std::puts("TestFn4"); }

TEST(OverloadSet, Construction) {
  ir::OverloadSet os(
      absl::Span<ir::AnyFunc const>{
          ir::ForeignFn(TestFn1, type::Func({}, {})),
          ir::ForeignFn(TestFn2, type::Func({type::Int64}, {})),
          ir::ForeignFn(TestFn3, type::Func({type::Bool}, {})),
      },
      nullptr);

  {
    std::vector<type::Type const *> v;
    EXPECT_EQ(os[v].foreign().get(), TestFn1);
  }
  {
    std::vector<type::Type const *> v{type::Int64};
    EXPECT_EQ(os[v].foreign().get(), TestFn2);
  }
  {
    std::vector<type::Type const *> v{type::Bool};
    EXPECT_EQ(os[v].foreign().get(), TestFn3);
  }
}

TEST(OverloadSet, Callable) {
  ir::OverloadSet os(absl::Span<ir::AnyFunc const>{},
                     [](absl::Span<type::Type const *const>) -> ir::AnyFunc {
                       return ir::ForeignFn(TestFn4, type::Func({}, {}));
                     });
  auto fn = os[std::vector<type::Type const*>()];
  ASSERT_FALSE(fn.is_fn());
  EXPECT_EQ(fn.foreign().get(), TestFn4);
}

}  // namespace
