#include "type/cast.h"

#include "test/catch.h"
#include "type/array.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace type {
namespace {

TEST_CASE("Numeric casting") {
  SECTION("from nat8") {
    CHECK(CanCast(Nat8, Nat8));
    CHECK(CanCast(Nat8, Nat16));
    CHECK(CanCast(Nat8, Nat32));
    CHECK(CanCast(Nat8, Nat64));
    CHECK(CanCast(Nat8, Int8));
    CHECK(CanCast(Nat8, Int16));
    CHECK(CanCast(Nat8, Int32));
    CHECK(CanCast(Nat8, Int64));
    CHECK(CanCast(Nat8, Float32));
    CHECK(CanCast(Nat8, Float64));
  }

  SECTION("from nat16") {
    CHECK(CanCast(Nat16, Nat8));
    CHECK(CanCast(Nat16, Nat16));
    CHECK(CanCast(Nat16, Nat32));
    CHECK(CanCast(Nat16, Nat64));
    CHECK(CanCast(Nat16, Int8));
    CHECK(CanCast(Nat16, Int16));
    CHECK(CanCast(Nat16, Int32));
    CHECK(CanCast(Nat16, Int64));
    CHECK(CanCast(Nat16, Float32));
    CHECK(CanCast(Nat16, Float64));
  }

  SECTION("from nat32") {
    CHECK(CanCast(Nat32, Nat8));
    CHECK(CanCast(Nat32, Nat16));
    CHECK(CanCast(Nat32, Nat32));
    CHECK(CanCast(Nat32, Nat64));
    CHECK(CanCast(Nat32, Int8));
    CHECK(CanCast(Nat32, Int16));
    CHECK(CanCast(Nat32, Int32));
    CHECK(CanCast(Nat32, Int64));
    CHECK(CanCast(Nat32, Float32));
    CHECK(CanCast(Nat32, Float64));
  }

  SECTION("from nat64") {
    CHECK(CanCast(Nat64, Nat8));
    CHECK(CanCast(Nat64, Nat16));
    CHECK(CanCast(Nat64, Nat32));
    CHECK(CanCast(Nat64, Nat64));
    CHECK(CanCast(Nat64, Int8));
    CHECK(CanCast(Nat64, Int16));
    CHECK(CanCast(Nat64, Int32));
    CHECK(CanCast(Nat64, Int64));
    CHECK(CanCast(Nat64, Float32));
    CHECK(CanCast(Nat64, Float64));
  }

  SECTION("from int8") {
    CHECK(CanCast(Int8, Nat8));
    CHECK(CanCast(Int8, Nat16));
    CHECK(CanCast(Int8, Nat32));
    CHECK(CanCast(Int8, Nat64));
    CHECK(CanCast(Int8, Int8));
    CHECK(CanCast(Int8, Int16));
    CHECK(CanCast(Int8, Int32));
    CHECK(CanCast(Int8, Int64));
    CHECK(CanCast(Int8, Float32));
    CHECK(CanCast(Int8, Float64));
  }

  SECTION("from int16") {
    CHECK(CanCast(Int16, Nat8));
    CHECK(CanCast(Int16, Nat16));
    CHECK(CanCast(Int16, Nat32));
    CHECK(CanCast(Int16, Nat64));
    CHECK(CanCast(Int16, Int8));
    CHECK(CanCast(Int16, Int16));
    CHECK(CanCast(Int16, Int32));
    CHECK(CanCast(Int16, Int64));
    CHECK(CanCast(Int16, Float32));
    CHECK(CanCast(Int16, Float64));
  }

  SECTION("from int32") {
    CHECK(CanCast(Int32, Nat8));
    CHECK(CanCast(Int32, Nat16));
    CHECK(CanCast(Int32, Nat32));
    CHECK(CanCast(Int32, Nat64));
    CHECK(CanCast(Int32, Int8));
    CHECK(CanCast(Int32, Int16));
    CHECK(CanCast(Int32, Int32));
    CHECK(CanCast(Int32, Int64));
    CHECK(CanCast(Int32, Float32));
    CHECK(CanCast(Int32, Float64));
  }

  SECTION("from int64") {
    CHECK(CanCast(Int64, Nat8));
    CHECK(CanCast(Int64, Nat16));
    CHECK(CanCast(Int64, Nat32));
    CHECK(CanCast(Int64, Nat64));
    CHECK(CanCast(Int64, Int8));
    CHECK(CanCast(Int64, Int16));
    CHECK(CanCast(Int64, Int32));
    CHECK(CanCast(Int64, Int64));
    CHECK(CanCast(Int64, Float32));
    CHECK(CanCast(Int64, Float64));
  }

  SECTION("from float32") {
    CHECK_FALSE(CanCast(Float32, Nat8));
    CHECK_FALSE(CanCast(Float32, Nat16));
    CHECK_FALSE(CanCast(Float32, Nat32));
    CHECK_FALSE(CanCast(Float32, Nat64));
    CHECK_FALSE(CanCast(Float32, Int8));
    CHECK_FALSE(CanCast(Float32, Int16));
    CHECK_FALSE(CanCast(Float32, Int32));
    CHECK_FALSE(CanCast(Float32, Int64));
    CHECK(CanCast(Float32, Float32));
    CHECK(CanCast(Float32, Float64));
  }

  SECTION("from float64") {
    CHECK_FALSE(CanCast(Float64, Nat8));
    CHECK_FALSE(CanCast(Float64, Nat16));
    CHECK_FALSE(CanCast(Float64, Nat32));
    CHECK_FALSE(CanCast(Float64, Nat64));
    CHECK_FALSE(CanCast(Float64, Int8));
    CHECK_FALSE(CanCast(Float64, Int16));
    CHECK_FALSE(CanCast(Float64, Int32));
    CHECK_FALSE(CanCast(Float64, Int64));
    CHECK(CanCast(Float64, Float32));
    CHECK(CanCast(Float64, Float64));
  }
}

TEST_CASE("Pointers") {
  SECTION("Null pointers") {
    CHECK(CanCast(NullPtr, Ptr(Bool)));
    CHECK(CanCast(NullPtr, BufPtr(Bool)));
    CHECK_FALSE(CanCast(NullPtr, Int64));
  }

  SECTION("Can replace bufptr with ptr at any level, but not the other way.") {
    CHECK(CanCast(BufPtr(Int8), Ptr(Int8)));
    CHECK_FALSE(CanCast(Ptr(Int8), BufPtr(Int8)));
    CHECK(CanCast(Ptr(BufPtr(Int8)), Ptr(Ptr(Int8))));
    CHECK(CanCast(BufPtr(Ptr(Int8)), Ptr(Ptr(Int8))));
    CHECK(CanCast(BufPtr(BufPtr(Int8)), Ptr(Ptr(Int8))));
  }

  SECTION("Changing the pointed-to type") {
    CHECK_FALSE(CanCast(Ptr(Int8), Ptr(Int16)));
    CHECK_FALSE(CanCast(BufPtr(Int8), BufPtr(Int16)));
    CHECK_FALSE(CanCast(BufPtr(Int8), Ptr(Int16)));

    CHECK_FALSE(CanCast(Ptr(Tup({Int32, Int64})), Ptr(Tup({Int64, Int32}))));
  }
}

TEST_CASE("Arrays") {
  SECTION("Empty arrays") {
    CHECK_FALSE(CanCast(EmptyArray, Arr(5, Int64)));
    CHECK(CanCast(EmptyArray, Arr(0, Bool)));
    CHECK_FALSE(CanCast(EmptyArray, Ptr(Bool)));
  }

  SECTION("Data type casting") {
    CHECK(CanCast(Arr(5, BufPtr(Bool)), Arr(5, Ptr(Bool))));
    CHECK_FALSE(CanCast(Arr(4, BufPtr(Bool)), Arr(5, Ptr(Bool))));
    CHECK_FALSE(CanCast(Arr(5, Ptr(Bool)), Arr(5, BufPtr(Bool))));

    CHECK_FALSE(CanCast(Arr(5, Int32), Arr(5, Int64)));
  }
}

TEST_CASE("Variants") {
  SECTION("Base type into variant") {
    CHECK(CanCast(Int32, Var({Bool, Int32})));

    // It would be ambiguous which integral type we should cast to so it's
    // disallowed.
    CHECK_FALSE(CanCast(Int16, Var({Int32, Int64})));

    // Even though there is a best match here, we disallow the cast because it
    // could be ambiguous. We want to avoid picking the best and falling back
    // because it leads to weird action-at-a-distance where the type changes and
    // the cast still works but does something different.
    CHECK_FALSE(CanCast(Int16, Var({Int16, Int64})));

    // TODO there's a weird situation here where you can't really create a
    // one-element variant, but if you could it's casting behavior would be
    // different from that element. I.e., you could cast int8 to int16, but not
    // int8 to Var(int16). You need to rectify this. I kind of don't want adding
    // a member to a variant to ever break well-behaved code.

    CHECK(CanCast(BufPtr(Bool), Var({Int32, Ptr(Bool)})));
    CHECK_FALSE(CanCast(BufPtr(Bool), Var({BufPtr(Bool), Ptr(Bool)})));
  }

  SECTION("Variant to variant") {
    CHECK(CanCast(Var({Int16, Int64}), Var({Int16, Int64})));
    CHECK(CanCast(Var({Int16, Int64}), Var({Bool, Int16, Int64})));

    CHECK(CanCast(Var({Int32, BufPtr(Bool)}), Var({Int64, Ptr(Bool)})));
    CHECK_FALSE(CanCast(Var({Int32, BufPtr(Bool)}),
                        Var({Int32, BufPtr(Bool), Ptr(Bool)})));
  }
}

TEST_CASE("Tuples") {
  CHECK(CanCast(Tup({Int32, Int64}), Tup({Int64, Int32})));
  CHECK_FALSE(CanCast(Tup({Int32, Int64}), Tup({Bool, Int32})));
  CHECK_FALSE(CanCast(Tup({Int32, Int64}), Tup({Int32, Int32, Int32})));
  CHECK(CanCast(Tup({Int32, BufPtr(Int32), NullPtr}),
                Tup({Int64, Ptr(Int32), BufPtr(Bool)})));
}

}  // namespace
}  // namespace type
