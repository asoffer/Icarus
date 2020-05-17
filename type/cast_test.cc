#include "type/cast.h"

#include "gtest/gtest.h"
#include "type/array.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace type {
namespace {

TEST(CanCast, FromNat8) {
  EXPECT_TRUE(CanCast(Nat8, Nat8));
  EXPECT_TRUE(CanCast(Nat8, Nat16));
  EXPECT_TRUE(CanCast(Nat8, Nat32));
  EXPECT_TRUE(CanCast(Nat8, Nat64));
  EXPECT_TRUE(CanCast(Nat8, Int8));
  EXPECT_TRUE(CanCast(Nat8, Int16));
  EXPECT_TRUE(CanCast(Nat8, Int32));
  EXPECT_TRUE(CanCast(Nat8, Int64));
  EXPECT_TRUE(CanCast(Nat8, Float32));
  EXPECT_TRUE(CanCast(Nat8, Float64));
}

TEST(CanCast, FromNat16) {
  EXPECT_TRUE(CanCast(Nat16, Nat8));
  EXPECT_TRUE(CanCast(Nat16, Nat16));
  EXPECT_TRUE(CanCast(Nat16, Nat32));
  EXPECT_TRUE(CanCast(Nat16, Nat64));
  EXPECT_TRUE(CanCast(Nat16, Int8));
  EXPECT_TRUE(CanCast(Nat16, Int16));
  EXPECT_TRUE(CanCast(Nat16, Int32));
  EXPECT_TRUE(CanCast(Nat16, Int64));
  EXPECT_TRUE(CanCast(Nat16, Float32));
  EXPECT_TRUE(CanCast(Nat16, Float64));
}

TEST(CanCast, FromNat32) {
  EXPECT_TRUE(CanCast(Nat32, Nat8));
  EXPECT_TRUE(CanCast(Nat32, Nat16));
  EXPECT_TRUE(CanCast(Nat32, Nat32));
  EXPECT_TRUE(CanCast(Nat32, Nat64));
  EXPECT_TRUE(CanCast(Nat32, Int8));
  EXPECT_TRUE(CanCast(Nat32, Int16));
  EXPECT_TRUE(CanCast(Nat32, Int32));
  EXPECT_TRUE(CanCast(Nat32, Int64));
  EXPECT_TRUE(CanCast(Nat32, Float32));
  EXPECT_TRUE(CanCast(Nat32, Float64));
}

TEST(CanCast, FromNat64) {
  EXPECT_TRUE(CanCast(Nat64, Nat8));
  EXPECT_TRUE(CanCast(Nat64, Nat16));
  EXPECT_TRUE(CanCast(Nat64, Nat32));
  EXPECT_TRUE(CanCast(Nat64, Nat64));
  EXPECT_TRUE(CanCast(Nat64, Int8));
  EXPECT_TRUE(CanCast(Nat64, Int16));
  EXPECT_TRUE(CanCast(Nat64, Int32));
  EXPECT_TRUE(CanCast(Nat64, Int64));
  EXPECT_TRUE(CanCast(Nat64, Float32));
  EXPECT_TRUE(CanCast(Nat64, Float64));
}

TEST(CanCast, FromInt8) {
  EXPECT_TRUE(CanCast(Int8, Nat8));
  EXPECT_TRUE(CanCast(Int8, Nat16));
  EXPECT_TRUE(CanCast(Int8, Nat32));
  EXPECT_TRUE(CanCast(Int8, Nat64));
  EXPECT_TRUE(CanCast(Int8, Int8));
  EXPECT_TRUE(CanCast(Int8, Int16));
  EXPECT_TRUE(CanCast(Int8, Int32));
  EXPECT_TRUE(CanCast(Int8, Int64));
  EXPECT_TRUE(CanCast(Int8, Float32));
  EXPECT_TRUE(CanCast(Int8, Float64));
}

TEST(CanCast, FromInt16) {
  EXPECT_TRUE(CanCast(Int16, Nat8));
  EXPECT_TRUE(CanCast(Int16, Nat16));
  EXPECT_TRUE(CanCast(Int16, Nat32));
  EXPECT_TRUE(CanCast(Int16, Nat64));
  EXPECT_TRUE(CanCast(Int16, Int8));
  EXPECT_TRUE(CanCast(Int16, Int16));
  EXPECT_TRUE(CanCast(Int16, Int32));
  EXPECT_TRUE(CanCast(Int16, Int64));
  EXPECT_TRUE(CanCast(Int16, Float32));
  EXPECT_TRUE(CanCast(Int16, Float64));
}

TEST(CanCast, FromInt32) {
  EXPECT_TRUE(CanCast(Int32, Nat8));
  EXPECT_TRUE(CanCast(Int32, Nat16));
  EXPECT_TRUE(CanCast(Int32, Nat32));
  EXPECT_TRUE(CanCast(Int32, Nat64));
  EXPECT_TRUE(CanCast(Int32, Int8));
  EXPECT_TRUE(CanCast(Int32, Int16));
  EXPECT_TRUE(CanCast(Int32, Int32));
  EXPECT_TRUE(CanCast(Int32, Int64));
  EXPECT_TRUE(CanCast(Int32, Float32));
  EXPECT_TRUE(CanCast(Int32, Float64));
}

TEST(CanCast, FromInt64) {
  EXPECT_TRUE(CanCast(Int64, Nat8));
  EXPECT_TRUE(CanCast(Int64, Nat16));
  EXPECT_TRUE(CanCast(Int64, Nat32));
  EXPECT_TRUE(CanCast(Int64, Nat64));
  EXPECT_TRUE(CanCast(Int64, Int8));
  EXPECT_TRUE(CanCast(Int64, Int16));
  EXPECT_TRUE(CanCast(Int64, Int32));
  EXPECT_TRUE(CanCast(Int64, Int64));
  EXPECT_TRUE(CanCast(Int64, Float32));
  EXPECT_TRUE(CanCast(Int64, Float64));
}

TEST(CanCast, FromFloat32) {
  EXPECT_FALSE(CanCast(Float32, Nat8));
  EXPECT_FALSE(CanCast(Float32, Nat16));
  EXPECT_FALSE(CanCast(Float32, Nat32));
  EXPECT_FALSE(CanCast(Float32, Nat64));
  EXPECT_FALSE(CanCast(Float32, Int8));
  EXPECT_FALSE(CanCast(Float32, Int16));
  EXPECT_FALSE(CanCast(Float32, Int32));
  EXPECT_FALSE(CanCast(Float32, Int64));
  EXPECT_TRUE(CanCast(Float32, Float32));
  EXPECT_TRUE(CanCast(Float32, Float64));
}

TEST(CanCast, FromFloat64) {
  EXPECT_FALSE(CanCast(Float64, Nat8));
  EXPECT_FALSE(CanCast(Float64, Nat16));
  EXPECT_FALSE(CanCast(Float64, Nat32));
  EXPECT_FALSE(CanCast(Float64, Nat64));
  EXPECT_FALSE(CanCast(Float64, Int8));
  EXPECT_FALSE(CanCast(Float64, Int16));
  EXPECT_FALSE(CanCast(Float64, Int32));
  EXPECT_FALSE(CanCast(Float64, Int64));
  EXPECT_TRUE(CanCast(Float64, Float32));
  EXPECT_TRUE(CanCast(Float64, Float64));
}

TEST(CanCast, NullPointer) {
  EXPECT_TRUE(CanCast(NullPtr, Ptr(Bool)));
  EXPECT_TRUE(CanCast(NullPtr, BufPtr(Bool)));
  EXPECT_FALSE(CanCast(NullPtr, Int64));
}

TEST(CanCast, BufPtrToPtr) {
  EXPECT_TRUE(CanCast(BufPtr(Int8), Ptr(Int8)));
  EXPECT_FALSE(CanCast(Ptr(Int8), BufPtr(Int8)));
  EXPECT_TRUE(CanCast(Ptr(BufPtr(Int8)), Ptr(Ptr(Int8))));
  EXPECT_TRUE(CanCast(BufPtr(Ptr(Int8)), Ptr(Ptr(Int8))));
  EXPECT_TRUE(CanCast(BufPtr(BufPtr(Int8)), Ptr(Ptr(Int8))));
}

TEST(CanCast, PointeeType) {
  EXPECT_FALSE(CanCast(Ptr(Int8), Ptr(Int16)));
  EXPECT_FALSE(CanCast(BufPtr(Int8), BufPtr(Int16)));
  EXPECT_FALSE(CanCast(BufPtr(Int8), Ptr(Int16)));

  EXPECT_FALSE(CanCast(Ptr(Tup({Int32, Int64})), Ptr(Tup({Int64, Int32}))));
}

TEST(CanCast, EmptyArray) {
  EXPECT_FALSE(CanCast(EmptyArray, Arr(5, Int64)));
  EXPECT_TRUE(CanCast(EmptyArray, Arr(0, Bool)));
  EXPECT_FALSE(CanCast(EmptyArray, Ptr(Bool)));
}

TEST(CanCast, ArrayDataType) {
  EXPECT_TRUE(CanCast(Arr(5, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCast(Arr(4, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCast(Arr(5, Ptr(Bool)), Arr(5, BufPtr(Bool))));

  EXPECT_FALSE(CanCast(Arr(5, Int32), Arr(5, Int64)));
}

TEST(CanCast, Variant) {
  EXPECT_TRUE(CanCast(Int32, Var({Bool, Int32})));

  // It would be ambiguous which integral type we should cast to so it's
  // disallowed.
  EXPECT_FALSE(CanCast(Int16, Var({Int32, Int64})));

  // Even though there is a best match here, we disallow the cast because it
  // could be ambiguous. We want to avoid picking the best and falling back
  // because it leads to weird action-at-a-distance where the type changes
  // and the cast still works but does something different.
  EXPECT_FALSE(CanCast(Int16, Var({Int16, Int64})));

  // TODO there's a weird situation here where you can't really create a
  // one-element variant, but if you could it's casting behavior would be
  // different from that element. I.e., you could cast int8 to int16, but
  // not int8 to Var(int16). You need to rectify this. I kind of don't want
  // adding a member to a variant to ever break well-behaved code.

  EXPECT_TRUE(CanCast(BufPtr(Bool), Var({Int32, Ptr(Bool)})));
  EXPECT_FALSE(CanCast(BufPtr(Bool), Var({BufPtr(Bool), Ptr(Bool)})));

  EXPECT_TRUE(CanCast(Var({Int16, Int64}), Var({Int16, Int64})));
  EXPECT_FALSE(CanCast(Var({Int16, Int64}), Var({Bool, Int16, Int64})));

  EXPECT_TRUE(CanCast(Var({Int32, BufPtr(Bool)}), Var({Int64, Ptr(Bool)})));
  EXPECT_FALSE(CanCast(Var({Int32, BufPtr(Bool)}),
                       Var({Int32, BufPtr(Bool), Ptr(Bool)})));
}

TEST(CanCast, Tuple) {
  EXPECT_TRUE(CanCast(Tup({Int32, Int64}), Tup({Int64, Int32})));
  EXPECT_FALSE(CanCast(Tup({Int32, Int64}), Tup({Bool, Int32})));
  EXPECT_FALSE(CanCast(Tup({Int32, Int64}), Tup({Int32, Int32, Int32})));
  EXPECT_TRUE(CanCast(Tup({Int32, BufPtr(Int32), NullPtr}),
                      Tup({Int64, Ptr(Int32), BufPtr(Bool)})));
}

TEST(CanCast, Function) {
  EXPECT_TRUE(CanCast(Func({}, {}), Func({}, {})));
  EXPECT_FALSE(CanCast(Func({}, {Bool}), Func({}, {Int64})));
  EXPECT_FALSE(CanCast(Func({core::AnonymousParam(Bool)}, {}),
                       Func({core::AnonymousParam(Int64)}, {})));

  EXPECT_TRUE(CanCast(Func({core::Param("name", Bool)}, {}),
                      Func({core::AnonymousParam(Bool)}, {})));

  EXPECT_FALSE(CanCast(Func({core::Param("name", Bool, core::MUST_NAME)}, {}),
                       Func({core::AnonymousParam(Bool)}, {})));

  EXPECT_FALSE(CanCast(Func({core::AnonymousParam(Bool)}, {}),
                       Func({core::Param("name", Bool, core::MUST_NAME)}, {})));

  EXPECT_FALSE(
      CanCast(Func({core::Param("name1", Bool, core::MUST_NAME)}, {}),
              Func({core::Param("name2", Bool, core::MUST_NAME)}, {})));

  EXPECT_FALSE(CanCast(Func({core::Param("name1", Bool)}, {}),
                       Func({core::Param("name2", Bool)}, {})));

  // TODO, actually these should be equal.
  EXPECT_TRUE(
      CanCast(Func({core::Param("name", Bool, core::MUST_NOT_NAME)}, {}),
              Func({core::AnonymousParam(Bool)}, {})));

  EXPECT_TRUE(CanCast(Func({core::Param("name", BufPtr(Bool))}, {}),
                      Func({core::AnonymousParam(Ptr(Bool))}, {})));
}

}  // namespace
}  // namespace type
