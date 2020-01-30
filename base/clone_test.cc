#include "base/clone.h"
#include "test/catch.h"

#include <iostream>
#include <memory>

namespace {

struct Base : base::Clone<Base, void> {
  explicit Base(int* n) : base_copy_counter(n) {}
  virtual ~Base() {}
  Base(Base const& b) : base_copy_counter(b.base_copy_counter) {
    ++*base_copy_counter;
  }
  int* base_copy_counter;
};

struct Derived : base::Clone<Derived, Base> {
  Derived(int* b, int* d)
      : base::Clone<Derived, Base>(b), derived_copy_counter(d) {}
  ~Derived() override {}

  Derived(Derived const& d)
      : base::Clone<Derived, Base>(d),
        derived_copy_counter(d.derived_copy_counter) {
    ++*derived_copy_counter;
  }
  int* derived_copy_counter;
};

TEST_CASE("Copy count") {
  int base_count    = 0;
  int derived_count = 0;
  Derived d(&base_count, &derived_count);
  Derived d2(d);
  auto d3 = d;
  static_cast<void>(d2);
  static_cast<void>(d3);

  CHECK(base_count == 2);
  CHECK(derived_count == 2);
}

TEST_CASE("Clone") {
  int base_count    = 0;
  int derived_count = 0;

  auto d         = std::make_unique<Derived>(&base_count, &derived_count);
  Base* base_ptr = d.get();
  auto p         = base_ptr->clone();
  static_assert(std::is_same_v<decltype(p), std::unique_ptr<Base>>);

  auto* d_ptr = dynamic_cast<Derived*>(p.get());
  REQUIRE(d_ptr != nullptr);

  CHECK(base_count == 1);
  CHECK(derived_count == 1);
}

struct X : base::Clone<X, void> {};
struct Y : base::Clone<Y, X> {};

TEST_CASE("Default copy exists") {
  Y y1;
  Y y2 = y1;
  static_cast<void>(y2);
}

}  // namespace
