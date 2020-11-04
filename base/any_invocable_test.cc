#include "base/any_invocable.h"

#include <type_traits>
#include "gtest/gtest.h"

namespace base {
namespace {

TEST(AnyInvocable, Traits) {
  static_assert(not std::is_copy_assignable_v<any_invocable<int()>>);
  static_assert(not std::is_copy_constructible_v<any_invocable<int()>>);
  static_assert(std::is_nothrow_move_constructible_v<any_invocable<int()>>);
  static_assert(std::is_nothrow_move_assignable_v<any_invocable<int()>>);
}

TEST(AnyInvocable, ComparisonToNullptr) {
  EXPECT_TRUE(any_invocable<int()>{});
  EXPECT_EQ(any_invocable<int()>{}, nullptr);
  EXPECT_EQ(nullptr, any_invocable<int()>{});
}

struct Uncopyable {
  explicit Uncopyable(int n) : n_(n) {}

  Uncopyable(Uncopyable const &) = delete;
  Uncopyable(Uncopyable &&)      = default;

  Uncopyable &operator=(Uncopyable const &) = delete;
  Uncopyable &operator=(Uncopyable &&) = default;

  int operator()() const { return n_; }

 private:
  int n_;
};

template <typename R, typename... Args>
R Invoke(any_invocable<R(Args...)> const &a, Args... args) {
  return a(std::move(args)...);
}

TEST(AnyInvocable, Invoke) {
  Uncopyable u(7);
  EXPECT_EQ(Invoke(any_invocable<int()>(std::move(u))), 7);
}

TEST(AnyInvocable, MoveAssignment) {
  any_invocable<int()> a;
  a = Uncopyable(10);
  EXPECT_EQ(Invoke(a), 10);
}

TEST(AnyInvocable, Lambda) {
  EXPECT_EQ(Invoke(any_invocable<int()>([u = Uncopyable(10)] { return u(); })),
            10);
}

}  // namespace
}  // namespace base
