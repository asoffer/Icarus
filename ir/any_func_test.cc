#include "ir/any_func.h"

#include "gtest/gtest.h"
#include "ir/compiled_fn.h"
#include "ir/value/foreign_fn.h"
#include "type/function.h"
#include "type/type.h"

void TestFn() {}

namespace {

TEST(AnyFunc, Foreign) {
  ir::ForeignFn f(TestFn, type::Func({}, {}));
  ir::AnyFunc a(f);
  ASSERT_FALSE(a.is_fn());

  uintptr_t lhs, rhs;
  void (*fnptr)() = a.foreign().get();
  std::memcpy(&lhs, &fnptr, sizeof(uintptr_t));
  void (*test_fn)() = TestFn;
  std::memcpy(&rhs, &test_fn, sizeof(uintptr_t));

  ASSERT_EQ(lhs, rhs);
}

TEST(AnyFunc, CompiledFn) {
  ir::CompiledFn f(type::Func({}, {}), {});
  ir::AnyFunc a(&f);
  ASSERT_TRUE(a.is_fn());
  ASSERT_EQ(a.func(), &f);
}

}  // namespace
