#include "type/cast.h"

#include "gtest/gtest.h"
#include "type/array.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace type {
namespace {

TEST(CanCast, InPlace) {
  EXPECT_TRUE(CanCastInPlace(U8, U8));
  EXPECT_FALSE(CanCastInPlace(U8, I8));
  EXPECT_TRUE(CanCastInPlace(Ptr(U8), Ptr(U8)));
  EXPECT_TRUE(CanCastInPlace(BufPtr(U8), Ptr(U8)));
  EXPECT_FALSE(CanCastInPlace(Ptr(U8), BufPtr(U8)));
  EXPECT_TRUE(CanCastInPlace(BufPtr(BufPtr(U8)), Ptr(Ptr(U8))));
  EXPECT_FALSE(CanCastInPlace(Arr(3, BufPtr(U8)), Slc(Ptr(U8))));

  EXPECT_FALSE(CanCastInPlace(NullPtr, BufPtr(I64)));
  EXPECT_FALSE(CanCastInPlace(NullPtr, Ptr(I64)));

  EXPECT_FALSE(CanCastInPlace(Arr(3, BufPtr(U8)), Arr(3, Ptr(U8))));
  EXPECT_TRUE(CanCastInPlace(Slc(BufPtr(U8)), Slc(Ptr(U8))));
}

TEST(CanCast, Implicitly) {
  EXPECT_TRUE(CanCastImplicitly(U8, U8));
  EXPECT_TRUE(CanCastImplicitly(Ptr(U8), Ptr(U8)));
  EXPECT_TRUE(CanCastImplicitly(BufPtr(U8), Ptr(U8)));
  EXPECT_TRUE(CanCastImplicitly(BufPtr(U8), Ptr(U8)));

  EXPECT_TRUE(CanCastImplicitly(NullPtr, BufPtr(I64)));
  EXPECT_TRUE(CanCastImplicitly(NullPtr, Ptr(I64)));

  EXPECT_FALSE(CanCastImplicitly(U8, Char));
  EXPECT_FALSE(CanCastImplicitly(Char, U8));

  EXPECT_TRUE(CanCastImplicitly(Arr(3, U64), Slc(U64)));

  EXPECT_TRUE(CanCastImplicitly(Arr(3, U64), Ptr(Arr(3, U64))));
  EXPECT_FALSE(CanCastImplicitly(Arr(3, U64), BufPtr(Arr(3, U64))));
}

TEST(CanCastExplicitly, Char) {
  EXPECT_TRUE(CanCastExplicitly(U8, Char));
  EXPECT_TRUE(CanCastExplicitly(Char, U8));

  EXPECT_FALSE(CanCastExplicitly(U64, Char));
  EXPECT_FALSE(CanCastExplicitly(I64, Char));

  EXPECT_TRUE(CanCastExplicitly(Char, U64));
  EXPECT_TRUE(CanCastExplicitly(Char, I64));
}

TEST(CanCastExplicitly, Integral) {
  EXPECT_TRUE(CanCastExplicitly(U8, U8));
  EXPECT_TRUE(CanCastExplicitly(U8, U16));
  EXPECT_TRUE(CanCastExplicitly(U8, U32));
  EXPECT_TRUE(CanCastExplicitly(U8, U64));
  EXPECT_TRUE(CanCastExplicitly(U8, I8));
  EXPECT_TRUE(CanCastExplicitly(U8, I16));
  EXPECT_TRUE(CanCastExplicitly(U8, I32));
  EXPECT_TRUE(CanCastExplicitly(U8, I64));
  EXPECT_TRUE(CanCastExplicitly(U8, F32));
  EXPECT_TRUE(CanCastExplicitly(U8, F64));

  EXPECT_TRUE(CanCastExplicitly(U16, U8));
  EXPECT_TRUE(CanCastExplicitly(U16, U16));
  EXPECT_TRUE(CanCastExplicitly(U16, U32));
  EXPECT_TRUE(CanCastExplicitly(U16, U64));
  EXPECT_TRUE(CanCastExplicitly(U16, I8));
  EXPECT_TRUE(CanCastExplicitly(U16, I16));
  EXPECT_TRUE(CanCastExplicitly(U16, I32));
  EXPECT_TRUE(CanCastExplicitly(U16, I64));
  EXPECT_TRUE(CanCastExplicitly(U16, F32));
  EXPECT_TRUE(CanCastExplicitly(U16, F64));

  EXPECT_TRUE(CanCastExplicitly(U32, U8));
  EXPECT_TRUE(CanCastExplicitly(U32, U16));
  EXPECT_TRUE(CanCastExplicitly(U32, U32));
  EXPECT_TRUE(CanCastExplicitly(U32, U64));
  EXPECT_TRUE(CanCastExplicitly(U32, I8));
  EXPECT_TRUE(CanCastExplicitly(U32, I16));
  EXPECT_TRUE(CanCastExplicitly(U32, I32));
  EXPECT_TRUE(CanCastExplicitly(U32, I64));
  EXPECT_TRUE(CanCastExplicitly(U32, F32));
  EXPECT_TRUE(CanCastExplicitly(U32, F64));

  EXPECT_TRUE(CanCastExplicitly(U64, U8));
  EXPECT_TRUE(CanCastExplicitly(U64, U16));
  EXPECT_TRUE(CanCastExplicitly(U64, U32));
  EXPECT_TRUE(CanCastExplicitly(U64, U64));
  EXPECT_TRUE(CanCastExplicitly(U64, I8));
  EXPECT_TRUE(CanCastExplicitly(U64, I16));
  EXPECT_TRUE(CanCastExplicitly(U64, I32));
  EXPECT_TRUE(CanCastExplicitly(U64, I64));
  EXPECT_TRUE(CanCastExplicitly(U64, F32));
  EXPECT_TRUE(CanCastExplicitly(U64, F64));

  EXPECT_TRUE(CanCastExplicitly(I8, U8));
  EXPECT_TRUE(CanCastExplicitly(I8, U16));
  EXPECT_TRUE(CanCastExplicitly(I8, U32));
  EXPECT_TRUE(CanCastExplicitly(I8, U64));
  EXPECT_TRUE(CanCastExplicitly(I8, I8));
  EXPECT_TRUE(CanCastExplicitly(I8, I16));
  EXPECT_TRUE(CanCastExplicitly(I8, I32));
  EXPECT_TRUE(CanCastExplicitly(I8, I64));
  EXPECT_TRUE(CanCastExplicitly(I8, F32));
  EXPECT_TRUE(CanCastExplicitly(I8, F64));

  EXPECT_TRUE(CanCastExplicitly(I16, U8));
  EXPECT_TRUE(CanCastExplicitly(I16, U16));
  EXPECT_TRUE(CanCastExplicitly(I16, U32));
  EXPECT_TRUE(CanCastExplicitly(I16, U64));
  EXPECT_TRUE(CanCastExplicitly(I16, I8));
  EXPECT_TRUE(CanCastExplicitly(I16, I16));
  EXPECT_TRUE(CanCastExplicitly(I16, I32));
  EXPECT_TRUE(CanCastExplicitly(I16, I64));
  EXPECT_TRUE(CanCastExplicitly(I16, F32));
  EXPECT_TRUE(CanCastExplicitly(I16, F64));

  EXPECT_TRUE(CanCastExplicitly(I32, U8));
  EXPECT_TRUE(CanCastExplicitly(I32, U16));
  EXPECT_TRUE(CanCastExplicitly(I32, U32));
  EXPECT_TRUE(CanCastExplicitly(I32, U64));
  EXPECT_TRUE(CanCastExplicitly(I32, I8));
  EXPECT_TRUE(CanCastExplicitly(I32, I16));
  EXPECT_TRUE(CanCastExplicitly(I32, I32));
  EXPECT_TRUE(CanCastExplicitly(I32, I64));
  EXPECT_TRUE(CanCastExplicitly(I32, F32));
  EXPECT_TRUE(CanCastExplicitly(I32, F64));

  EXPECT_TRUE(CanCastExplicitly(I64, U8));
  EXPECT_TRUE(CanCastExplicitly(I64, U16));
  EXPECT_TRUE(CanCastExplicitly(I64, U32));
  EXPECT_TRUE(CanCastExplicitly(I64, U64));
  EXPECT_TRUE(CanCastExplicitly(I64, I8));
  EXPECT_TRUE(CanCastExplicitly(I64, I16));
  EXPECT_TRUE(CanCastExplicitly(I64, I32));
  EXPECT_TRUE(CanCastExplicitly(I64, I64));
  EXPECT_TRUE(CanCastExplicitly(I64, F32));
  EXPECT_TRUE(CanCastExplicitly(I64, F64));
}

TEST(CanCastExplicitly, Arithmetic) {
  EXPECT_FALSE(CanCastExplicitly(F32, U8));
  EXPECT_FALSE(CanCastExplicitly(F32, U16));
  EXPECT_FALSE(CanCastExplicitly(F32, U32));
  EXPECT_FALSE(CanCastExplicitly(F32, U64));
  EXPECT_FALSE(CanCastExplicitly(F32, I8));
  EXPECT_FALSE(CanCastExplicitly(F32, I16));
  EXPECT_FALSE(CanCastExplicitly(F32, I32));
  EXPECT_FALSE(CanCastExplicitly(F32, I64));
  EXPECT_TRUE(CanCastExplicitly(F32, F32));
  EXPECT_TRUE(CanCastExplicitly(F32, F64));

  EXPECT_FALSE(CanCastExplicitly(F64, U8));
  EXPECT_FALSE(CanCastExplicitly(F64, U16));
  EXPECT_FALSE(CanCastExplicitly(F64, U32));
  EXPECT_FALSE(CanCastExplicitly(F64, U64));
  EXPECT_FALSE(CanCastExplicitly(F64, I8));
  EXPECT_FALSE(CanCastExplicitly(F64, I16));
  EXPECT_FALSE(CanCastExplicitly(F64, I32));
  EXPECT_FALSE(CanCastExplicitly(F64, I64));
  EXPECT_TRUE(CanCastExplicitly(F64, F32));
  EXPECT_TRUE(CanCastExplicitly(F64, F64));
}

TEST(CanCastExplicitly, Pointers) {
  EXPECT_TRUE(CanCastExplicitly(BufPtr(Memory), Ptr(Bool)));
  EXPECT_TRUE(CanCastExplicitly(BufPtr(Memory), BufPtr(Bool)));
  EXPECT_FALSE(CanCastExplicitly(BufPtr(Memory), NullPtr));

  EXPECT_TRUE(CanCastExplicitly(Ptr(Bool), Ptr(Memory)));
  EXPECT_TRUE(CanCastExplicitly(BufPtr(Bool), BufPtr(Memory)));
  EXPECT_TRUE(CanCastExplicitly(NullPtr, BufPtr(Memory)));

  EXPECT_TRUE(CanCastExplicitly(Ptr(Memory), Ptr(Bool)));
  EXPECT_FALSE(CanCastExplicitly(Ptr(Memory), BufPtr(Bool)));
  EXPECT_FALSE(CanCastExplicitly(Ptr(Memory), NullPtr));

  EXPECT_TRUE(CanCastExplicitly(Ptr(Bool), Ptr(Memory)));
  EXPECT_TRUE(CanCastExplicitly(BufPtr(Bool), Ptr(Memory)));
  EXPECT_TRUE(CanCastExplicitly(NullPtr, Ptr(Memory)));

  EXPECT_TRUE(CanCastExplicitly(NullPtr, Ptr(Bool)));
  EXPECT_TRUE(CanCastExplicitly(NullPtr, BufPtr(Bool)));
  EXPECT_FALSE(CanCastExplicitly(NullPtr, I64));

  EXPECT_TRUE(CanCastExplicitly(BufPtr(I8), Ptr(I8)));
  EXPECT_FALSE(CanCastExplicitly(Ptr(I8), BufPtr(I8)));
  EXPECT_TRUE(CanCastExplicitly(Ptr(BufPtr(I8)), Ptr(Ptr(I8))));
  EXPECT_TRUE(CanCastExplicitly(BufPtr(Ptr(I8)), Ptr(Ptr(I8))));
  EXPECT_TRUE(CanCastExplicitly(BufPtr(BufPtr(I8)), Ptr(Ptr(I8))));

  EXPECT_FALSE(CanCastExplicitly(Ptr(I8), Ptr(I16)));
  EXPECT_FALSE(CanCastExplicitly(BufPtr(I8), BufPtr(I16)));
  EXPECT_FALSE(CanCastExplicitly(BufPtr(I8), Ptr(I16)));

  EXPECT_TRUE(CanCastImplicitly(Arr(3, U64), Ptr(Arr(3, U64))));
  EXPECT_FALSE(CanCastImplicitly(Arr(3, U64), BufPtr(Arr(3, U64))));
}

TEST(CanCastExplicitly, Arrays) {
  EXPECT_FALSE(CanCastExplicitly(EmptyArray, Arr(5, I64)));
  EXPECT_TRUE(CanCastExplicitly(EmptyArray, Arr(0, Bool)));
  EXPECT_FALSE(CanCastExplicitly(EmptyArray, Ptr(Bool)));

  EXPECT_TRUE(CanCastExplicitly(Arr(5, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCastExplicitly(Arr(4, BufPtr(Bool)), Arr(5, Ptr(Bool))));
  EXPECT_FALSE(CanCastExplicitly(Arr(5, Ptr(Bool)), Arr(5, BufPtr(Bool))));

  EXPECT_FALSE(CanCastExplicitly(Arr(5, I32), Arr(5, I64)));

  EXPECT_TRUE(CanCastExplicitly(EmptyArray, Slc(Bool)));
  EXPECT_TRUE(CanCastExplicitly(EmptyArray, Slc(I64)));

  EXPECT_TRUE(CanCastExplicitly(Arr(5, I64), Slc(I64)));
  EXPECT_TRUE(CanCastExplicitly(Arr(3, I64), Slc(I64)));
  EXPECT_TRUE(CanCastExplicitly(Arr(3, Bool), Slc(Bool)));
  EXPECT_FALSE(CanCastExplicitly(Arr(3, Bool), Slc(I64)));

  EXPECT_TRUE(CanCastExplicitly(Arr(3, BufPtr(Bool)), Slc(Ptr(Bool))));
}

TEST(CanCastInPlace, Function) {
  EXPECT_TRUE(CanCastInPlace(Func({}, {}), Func({}, {})));
  EXPECT_FALSE(CanCastInPlace(Func({}, {Bool}), Func({}, {I64})));
  EXPECT_FALSE(CanCastInPlace(
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
      Func({core::AnonymousParam(QualType::NonConstant(I64))}, {})));

  EXPECT_TRUE(CanCastInPlace(
      Func({core::Param("name", QualType::NonConstant(Bool))}, {}),
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCastInPlace(
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCastInPlace(
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(CanCastInPlace(
      Func({core::Param("name1", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::Param("name2", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(CanCastInPlace(
      Func({core::Param("name1", QualType::NonConstant(Bool))}, {}),
      Func({core::Param("name2", QualType::NonConstant(Bool))}, {})));

  // TODO, actually these should be equal.
  EXPECT_TRUE(CanCastInPlace(
      Func({core::Param("name", QualType::NonConstant(Bool),
                        core::MUST_NOT_NAME)},
           {}),
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_TRUE(CanCastInPlace(
      Func({core::Param("name", QualType::NonConstant(BufPtr(Bool)))}, {}),
      Func({core::AnonymousParam(QualType::NonConstant(Ptr(Bool)))}, {})));
}

TEST(CanCastExplicitly, Function) {
  EXPECT_TRUE(CanCastExplicitly(Func({}, {}), Func({}, {})));
  EXPECT_FALSE(CanCastExplicitly(Func({}, {Bool}), Func({}, {I64})));
  EXPECT_FALSE(
      CanCastExplicitly(Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
              Func({core::AnonymousParam(QualType::NonConstant(I64))}, {})));

  EXPECT_TRUE(
      CanCastExplicitly(Func({core::Param("name", QualType::NonConstant(Bool))}, {}),
              Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCastExplicitly(
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_FALSE(CanCastExplicitly(
      Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {}),
      Func({core::Param("name", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(CanCastExplicitly(
      Func({core::Param("name1", QualType::NonConstant(Bool), core::MUST_NAME)},
           {}),
      Func({core::Param("name2", QualType::NonConstant(Bool), core::MUST_NAME)},
           {})));

  EXPECT_FALSE(
      CanCastExplicitly(Func({core::Param("name1", QualType::NonConstant(Bool))}, {}),
              Func({core::Param("name2", QualType::NonConstant(Bool))}, {})));

  // TODO, actually these should be equal.
  EXPECT_TRUE(
      CanCastExplicitly(Func({core::Param("name", QualType::NonConstant(Bool),
                                core::MUST_NOT_NAME)},
                   {}),
              Func({core::AnonymousParam(QualType::NonConstant(Bool))}, {})));

  EXPECT_TRUE(CanCastExplicitly(
      Func({core::Param("name", QualType::NonConstant(BufPtr(Bool)))}, {}),
      Func({core::AnonymousParam(QualType::NonConstant(Ptr(Bool)))}, {})));
}

TEST(Meet, Integral) {
  EXPECT_EQ(Meet(Integer, Integer), Integer);
  EXPECT_EQ(Meet(Integer, I8), I8);
  EXPECT_EQ(Meet(Integer, U16), U16);
  EXPECT_EQ(Meet(U16, Integer), U16);
  EXPECT_EQ(Meet(I8, Integer), I8);
}

}  // namespace
}  // namespace type
