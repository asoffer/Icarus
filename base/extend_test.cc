#include "base/extend.h"

#include "gtest/gtest.h"

namespace base {
namespace {

struct S1 {
  int x = 0;
};

struct S2 {
  int x = 0;
  S1 y = {};
};

struct S3 {
  int x = 0;
  S1 y = {};
  bool z = false;
};

TEST(NumInitializers, Works) {
  EXPECT_EQ((std::integral_constant<int, internal::NumInitializers<S1>()>::value), 1);
  EXPECT_EQ((std::integral_constant<int, internal::NumInitializers<S2>()>::value), 2);
  EXPECT_EQ((std::integral_constant<int, internal::NumInitializers<S3>()>::value), 3);
}

TEST(GetFields, Mutable) {
  {
    S1 s1;
    auto [x] = internal::GetFields<S1, 0>(s1);
    EXPECT_EQ(&x, &s1.x);
  }
  {
    S2 s2;
    auto [x, y] = internal::GetFields<S2, 0>(s2);
    EXPECT_EQ(&x, &s2.x);
    EXPECT_EQ(&y, &s2.y);
  }
  {
    S3 s3;
    auto [x, y, z] = internal::GetFields<S3, 0>(s3);
    EXPECT_EQ(&x, &s3.x);
    EXPECT_EQ(&y, &s3.y);
    EXPECT_EQ(&z, &s3.z);
  }
}

TEST(GetFields, Const) {
  {
    S1 const s1;
    auto [x] = internal::GetFields<S1, 0>(s1);
    EXPECT_EQ(&x, &s1.x);
  }
  {
    S2 const s2;
    auto [x, y] = internal::GetFields<S2, 0>(s2);
    EXPECT_EQ(&x, &s2.x);
    EXPECT_EQ(&y, &s2.y);
  }
  {
    S3 const s3;
    auto [x, y, z] = internal::GetFields<S3, 0>(s3);
    EXPECT_EQ(&x, &s3.x);
    EXPECT_EQ(&y, &s3.y);
    EXPECT_EQ(&z, &s3.z);
  }
}

struct S : Extend<S>::With<> {
  int x = 0;
  int y = 0;
};

TEST(Extend, FieldRefs) {
  S s;
  EXPECT_EQ(&s.x, &std::get<0>(s.field_refs()));
  EXPECT_EQ(&s.y, &std::get<1>(s.field_refs()));
}

}  // namespace
}  // namespace base
