#include "base/extend.h"

#include "gtest/gtest.h"

namespace base {
namespace {

struct S1 {
  int x = 0;
};

struct S2 {
  int x = 0;
  S1 y  = {};
};

struct S3 {
  int x  = 0;
  S1 y   = {};
  bool z = false;
};

TEST(NumInitializers, Works) {
  EXPECT_EQ(
      (std::integral_constant<int,
                              internal_extend::NumInitializers<S1>()>::value),
      1);
  EXPECT_EQ(
      (std::integral_constant<int,
                              internal_extend::NumInitializers<S2>()>::value),
      2);
  EXPECT_EQ(
      (std::integral_constant<int,
                              internal_extend::NumInitializers<S3>()>::value),
      3);
}

struct Base {
  int v;
  int w;
};
struct Derived : Base {
  int x;
  int y;
  int z;
};

TEST(NumInitializers, WorksWithBaseClass) {
  EXPECT_EQ((std::integral_constant<
                int, internal_extend::NumInitializers<Derived>()>::value),
            4);
}

TEST(EnableExtensions, Mutable) {
  {
    S1 s1;
    auto [x] = EnableExtensions::field_refs<S1, 0, 1>(s1);
    EXPECT_EQ(&x, &s1.x);
  }
  {
    S2 s2;
    auto [x, y] = EnableExtensions::field_refs<S2, 0, 2>(s2);
    EXPECT_EQ(&x, &s2.x);
    EXPECT_EQ(&y, &s2.y);
  }
  {
    S3 s3;
    auto [x, y, z] = EnableExtensions::field_refs<S3, 0, 3>(s3);
    EXPECT_EQ(&x, &s3.x);
    EXPECT_EQ(&y, &s3.y);
    EXPECT_EQ(&z, &s3.z);
  }
}

TEST(EnableExtensions, Const) {
  {
    S1 const s1;
    auto [x] = EnableExtensions::field_refs<S1, 0, 1>(s1);
    EXPECT_EQ(&x, &s1.x);
  }
  {
    S2 const s2;
    auto [x, y] = EnableExtensions::field_refs<S2, 0, 2>(s2);
    EXPECT_EQ(&x, &s2.x);
    EXPECT_EQ(&y, &s2.y);
  }
  {
    S3 const s3;
    auto [x, y, z] = EnableExtensions::field_refs<S3, 0, 3>(s3);
    EXPECT_EQ(&x, &s3.x);
    EXPECT_EQ(&y, &s3.y);
    EXPECT_EQ(&z, &s3.z);
  }
}

TEST(Extend, FieldRefs) {
  struct S : Extend<S>::With<> {
    int x = 0;
    int y = 0;
  };

  S s;
  EXPECT_EQ(&s.x, &std::get<0>(s.field_refs()));
  EXPECT_EQ(&s.y, &std::get<1>(s.field_refs()));
}

TEST(Extend, ExplicitCountFieldRefs) {
  struct S : Extend<S, 2>::With<> {
    int x = 0;
    int y = 0;
  };
  S s;
  EXPECT_EQ(&s.x, &std::get<0>(s.field_refs()));
  EXPECT_EQ(&s.y, &std::get<1>(s.field_refs()));
}

struct E {};
struct D {
  static constexpr auto dependencies = nth::type_sequence<>;
};
struct C {
  static constexpr auto dependencies = nth::type_sequence<D>;
};
struct B {
  static constexpr auto dependencies = nth::type_sequence<D, E>;
};
struct A {
  static constexpr auto dependencies = nth::type_sequence<B, C>;
};

TEST(Extend, Dependencies) {
  constexpr auto deps = internal_extend::AllDependencies<A>;
  EXPECT_TRUE(deps.contains<nth::type<A>>());
  EXPECT_TRUE(deps.contains<nth::type<B>>());
  EXPECT_TRUE(deps.contains<nth::type<C>>());
  EXPECT_TRUE(deps.contains<nth::type<D>>());
  EXPECT_TRUE(deps.contains<nth::type<E>>());
  EXPECT_EQ(deps.size(), 5);
}

}  // namespace
}  // namespace base
