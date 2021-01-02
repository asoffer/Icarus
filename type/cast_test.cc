#include "type/cast.h"

#include "gtest/gtest.h"
#include "type/array.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/tuple.h"

namespace type {
namespace {

TEST(CanCast, FromU8) {
  EXPECT_TRUE(CanCast(U8, U8));
  EXPECT_TRUE(CanCast(U8, U16));
  EXPECT_TRUE(CanCast(U8, U32));
  EXPECT_TRUE(CanCast(U8, U64));
  EXPECT_TRUE(CanCast(U8, I8));
  EXPECT_TRUE(CanCast(U8, I16));
  EXPECT_TRUE(CanCast(U8, I32));
  EXPECT_TRUE(CanCast(U8, I64));
  EXPECT_TRUE(CanCast(U8, F32));
  EXPECT_TRUE(CanCast(U8, F64));
}

TEST(CanCast, FromU16) {
  EXPECT_TRUE(CanCast(U16, U8));
  EXPECT_TRUE(CanCast(U16, U16));
  EXPECT_TRUE(CanCast(U16, U32));
  EXPECT_TRUE(CanCast(U16, U64));
  EXPECT_TRUE(CanCast(U16, I8));
  EXPECT_TRUE(CanCast(U16, I16));
  EXPECT_TRUE(CanCast(U16, I32));
  EXPECT_TRUE(CanCast(U16, I64));
  EXPECT_TRUE(CanCast(U16, F32));
  EXPECT_TRUE(CanCast(U16, F64));
}

TEST(CanCast, FromU32) {
  EXPECT_TRUE(CanCast(U32, U8));
  EXPECT_TRUE(CanCast(U32, U16));
  EXPECT_TRUE(CanCast(U32, U32));
  EXPECT_TRUE(CanCast(U32, U64));
  EXPECT_TRUE(CanCast(U32, I8));
  EXPECT_TRUE(CanCast(U32, I16));
  EXPECT_TRUE(CanCast(U32, I32));
  EXPECT_TRUE(CanCast(U32, I64));
  EXPECT_TRUE(CanCast(U32, F32));
  EXPECT_TRUE(CanCast(U32, F64));
}

TEST(CanCast, FromU64) {
  EXPECT_TRUE(CanCast(U64, U8));
  EXPECT_TRUE(CanCast(U64, U16));
  EXPECT_TRUE(CanCast(U64, U32));
  EXPECT_TRUE(CanCast(U64, U64));
  EXPECT_TRUE(CanCast(U64, I8));
  EXPECT_TRUE(CanCast(U64, I16));
  EXPECT_TRUE(CanCast(U64, I32));
  EXPECT_TRUE(CanCast(U64, I64));
  EXPECT_TRUE(CanCast(U64, F32));
  EXPECT_TRUE(CanCast(U64, F64));
}

TEST(CanCast, FromI8) {
  EXPECT_TRUE(CanCast(I8, U8));
  EXPECT_TRUE(CanCast(I8, U16));
  EXPECT_TRUE(CanCast(I8, U32));
  EXPECT_TRUE(CanCast(I8, U64));
  EXPECT_TRUE(CanCast(I8, I8));
  EXPECT_TRUE(CanCast(I8, I16));
  EXPECT_TRUE(CanCast(I8, I32));
  EXPECT_TRUE(CanCast(I8, I64));
  EXPECT_TRUE(CanCast(I8, F32));
  EXPECT_TRUE(CanCast(I8, F64));
}

TEST(CanCast, FromI16) {
  EXPECT_TRUE(CanCast(I16, U8));
  EXPECT_TRUE(CanCast(I16, U16));
  EXPECT_TRUE(CanCast(I16, U32));
  EXPECT_TRUE(CanCast(I16, U64));
  EXPECT_TRUE(CanCast(I16, I8));
  EXPECT_TRUE(CanCast(I16, I16));
  EXPECT_TRUE(CanCast(I16, I32));
  EXPECT_TRUE(CanCast(I16, I64));
  EXPECT_TRUE(CanCast(I16, F32));
  EXPECT_TRUE(CanCast(I16, F64));
}

TEST(CanCast, FromI32) {
  EXPECT_TRUE(CanCast(I32, U8));
  EXPECT_TRUE(CanCast(I32, U16));
  EXPECT_TRUE(CanCast(I32, U32));
  EXPECT_TRUE(CanCast(I32, U64));
  EXPECT_TRUE(CanCast(I32, I8));
  EXPECT_TRUE(CanCast(I32, I16));
  EXPECT_TRUE(CanCast(I32, I32));
  EXPECT_TRUE(CanCast(I32, I64));
  EXPECT_TRUE(CanCast(I32, F32));
  EXPECT_TRUE(CanCast(I32, F64));
}

TEST(CanCast, FromI64) {
  EXPECT_TRUE(CanCast(I64, U8));
  EXPECT_TRUE(CanCast(I64, U16));
  EXPECT_TRUE(CanCast(I64, U32));
  EXPECT_TRUE(CanCast(I64, U64));
  EXPECT_TRUE(CanCast(I64, I8));
  EXPECT_TRUE(CanCast(I64, I16));
  EXPECT_TRUE(CanCast(I64, I32));
  EXPECT_TRUE(CanCast(I64, I64));
  EXPECT_TRUE(CanCast(I64, F32));
  EXPECT_TRUE(CanCast(I64, F64));
}

TEST(CanCast, FromF32) {
  EXPECT_FALSE(CanCast(F32, U8));
  EXPECT_FALSE(CanCast(F32, U16));
  EXPECT_FALSE(CanCast(F32, U32));
  EXPECT_FALSE(CanCast(F32, U64));
  EXPECT_FALSE(CanCast(F32, I8));
  EXPECT_FALSE(CanCast(F32, I16));
  EXPECT_FALSE(CanCast(F32, I32));
  EXPECT_FALSE(CanCast(F32, I64));
  EXPECT_TRUE(CanCast(F32, F32));
  EXPECT_TRUE(CanCast(F32, F64));
}

TEST(CanCast, FromF64) {
  EXPECT_FALSE(CanCast(F64, U8));
  EXPECT_FALSE(CanCast(F64, U16));
  EXPECT_FALSE(CanCast(F64, U32));
  EXPECT_FALSE(CanCast(F64, U64));
  EXPECT_FALSE(CanCast(F64, I8));
  EXPECT_FALSE(CanCast(F64, I16));
  EXPECT_FALSE(CanCast(F64, I32));
  EXPECT_FALSE(CanCast(F64, I64));
  EXPECT_TRUE(CanCast(F64, F32));
  EXPECT_TRUE(CanCast(F64, F64));
}

TEST(CanCast, NullPointer) {
  EXPECT_TRUE(CanCast(NullPtr, Ptr(Bool)));
  EXPECT_TRUE(CanCast(NullPtr, BufPtr(Bool)));
  EXPECT_FALSE(CanCast(NullPtr, I64));
}

TEST(CanCast, BufPtrToPtr) {
  EXPECT_TRUE(CanCast(BufPtr(I8), Ptr(I8)));
  EXPECT_FALSE(CanCast(Ptr(I8), BufPtr(I8)));
  EXPECT_TRUE(CanCast(Ptr(BufPtr(I8)), Ptr(Ptr(I8))));
  EXPECT_TRUE(CanCast(BufPtr(Ptr(I8)), Ptr(Ptr(I8))));
  EXPECT_TRUE(CanCast(BufPtr(BufPtr(I8)), Ptr(Ptr(I8))));
}

TEST(CanCast, PointeeType) {
  EXPECT_FALSE(CanCast(Ptr(I8), Ptr(I16)));
  EXPECT_FALSE(CanCast(BufPtr(I8), BufPtr(I16)));
  EXPECT_FALSE(CanCast(BufPtr(I8), Ptr(I16)));

  EXPECT_FALSE(CanCast(Ptr(Tup({I32, I64})), Ptr(Tup({I64, I32}))));
}

TEST(CanCast, EmptyArray) {
  EXPECT_FALSE(CanCast(EmptyArray, Arr(5, I64)));
  EXPECT_TRUE(CanCast(EmptyArray, Arr(0, Bool)));
  EXPECT_FALSE(CanCast(EmptyArray, Ptr(Bool)));
}

TEST(CanCast, ArrayDataType) {
  EXPECT_TRUE(CanCast(Arr(5, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCast(Arr(4, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCast(Arr(5, Ptr(Bool)), Arr(5, BufPtr(Bool))));

  EXPECT_FALSE(CanCast(Arr(5, I32), Arr(5, I64)));
}

TEST(CanCast, ArrayToSlice) {
  EXPECT_TRUE(CanCast(EmptyArray, Slc(Bool)));
  EXPECT_TRUE(CanCast(EmptyArray, Slc(I64)));

  EXPECT_TRUE(CanCast(Arr(5, I64), Slc(I64)));
  EXPECT_TRUE(CanCast(Arr(3, I64), Slc(I64)));
  EXPECT_TRUE(CanCast(Arr(3, Bool), Slc(Bool)));
  EXPECT_FALSE(CanCast(Arr(3, Bool), Slc(I64)));

  EXPECT_TRUE(CanCast(Arr(3, BufPtr(Bool)), Slc(Ptr(Bool))));
}

TEST(CanCast, Tuple) {
  EXPECT_TRUE(CanCast(Tup({I32, I64}), Tup({I64, I32})));
  EXPECT_FALSE(CanCast(Tup({I32, I64}), Tup({Bool, I32})));
  EXPECT_FALSE(CanCast(Tup({I32, I64}), Tup({I32, I32, I32})));
  EXPECT_TRUE(CanCast(Tup({I32, BufPtr(I32), NullPtr}),
                      Tup({I64, Ptr(I32), BufPtr(Bool)})));
}

TEST(CanCast, Function) {
  EXPECT_TRUE(CanCast(Func({}, {}), Func({}, {})));
  EXPECT_FALSE(CanCast(Func({}, {Bool}), Func({}, {I64})));
  EXPECT_FALSE(
      CanCast(Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
              Func({core::AnonymousParam(QualType::NonConstant(I64))}, {})));

  EXPECT_TRUE(
      CanCast(Func({core::Param("name", QualType::NonConstant(Bool))}, {}),
              Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCast(
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCast(
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(CanCast(
      Func({core::Param("name1", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::Param("name2", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(
      CanCast(Func({core::Param("name1", QualType::NonConstant(Bool))}, {}),
              Func({core::Param("name2", QualType::NonConstant(Bool))}, {})));

  // TODO, actually these should be equal.
  EXPECT_TRUE(
      CanCast(Func({core::Param("name", QualType::NonConstant(Bool),
                                core::MUST_NOT_NAME)},
                   {}),
              Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_TRUE(CanCast(
      Func({core::Param("name", QualType::NonConstant(BufPtr(Bool)))}, {}),
      Func({core::AnonymousParam(QualType::NonConstant(Ptr(Bool)))}, {})));
}

}  // namespace
}  // namespace type
