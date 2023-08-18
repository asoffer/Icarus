#include "semantic_analysis/type_verification/casting.h"

#include "gtest/gtest.h"

namespace semantic_analysis {
namespace {

TEST(CanCast, Integers) {
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(16)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(32)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), I(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), I(50)), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), I(88)), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(16)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(32)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(100)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), U(100)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), U(50)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), U(88)), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(16)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(32)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), I(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), I(50)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), I(88)), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(16)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(32)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), U(100)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), U(50)), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), U(88)), CastKind::None);

  EXPECT_EQ(CanCast(Constant(Integer), Integer), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(Integer), Integer), CastKind::InPlace);

  EXPECT_EQ(CanCast(Constant(Integer), U(17)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), U(17)), CastKind::None);
  EXPECT_EQ(CanCast(Constant(Integer), I(17)), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), I(17)), CastKind::None);

  EXPECT_EQ(CanCast(Constant(U(17)), Integer), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(17)), Integer), CastKind::None);
  EXPECT_EQ(CanCast(Constant(I(17)), Integer), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(17)), Integer), CastKind::None);
}

TEST(CanCast, FloatingPoint) {
  TypeSystem type_system;
  EXPECT_EQ(CanCast(QualifiedType(F32), F32), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(F32), F64), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(F64), F32), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(F64), F64), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(F32), F32), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(F32), F64), CastKind::Implicit);
  EXPECT_EQ(CanCast(Constant(F64), F32), CastKind::None);
  EXPECT_EQ(CanCast(Constant(F64), F64), CastKind::InPlace);
}

TEST(CanCast, Pointers) {
  TypeSystem type_system;

  core::Type bi = BufferPointerType(type_system, I(32));
  core::Type pi = core::PointerType(type_system, I(32));

  core::Type bbi = BufferPointerType(type_system, bi);
  core::Type bpi = BufferPointerType(type_system, pi);
  core::Type pbi = core::PointerType(type_system, bi);
  core::Type ppi = core::PointerType(type_system, pi);

  core::Type ppf = core::PointerType(type_system, F32);

  EXPECT_EQ(CanCast(QualifiedType(bi), bi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bi), pi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(pi), bi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pi), pi), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(bi), bi), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(bi), pi), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(pi), bi), CastKind::None);
  EXPECT_EQ(CanCast(Constant(pi), pi), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(bbi), bbi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), bpi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), pbi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), ppi), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(bpi), bbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), bpi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bpi), pbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), ppi), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(pbi), bbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), bpi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), pbi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(pbi), ppi), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(ppi), bbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), bpi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), pbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), ppi), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(bbi), ppf), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), ppf), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), ppf), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), ppf), CastKind::None);
}

TEST(CanCast, Slices) {
  TypeSystem type_system;

  core::Type bi = BufferPointerType(type_system, I(32));
  core::Type pi = core::PointerType(type_system, I(32));

  core::Type sbi  = SliceType(type_system, bi);
  core::Type spi  = SliceType(type_system, pi);
  core::Type sf32 = SliceType(type_system, F32);
  core::Type sf64 = SliceType(type_system, F64);

  EXPECT_EQ(CanCast(QualifiedType(sbi), sbi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(sbi), spi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(sbi), I(32)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sbi), bi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sbi), pi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sbi), sf64), CastKind::None);

  EXPECT_EQ(CanCast(Constant(sbi), sbi), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(sbi), spi), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(sbi), I(32)), CastKind::None);
  EXPECT_EQ(CanCast(Constant(sbi), bi), CastKind::None);
  EXPECT_EQ(CanCast(Constant(sbi), pi), CastKind::None);
  EXPECT_EQ(CanCast(Constant(sbi), sf64), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(spi), sbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(spi), spi), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(spi), I(32)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(spi), bi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(spi), pi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(spi), sf64), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(sf64), sbi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sf64), spi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sf64), I(32)), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sf64), bi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sf64), pi), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(sf64), sf64), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(sf64), sf32), CastKind::None);
}

}  // namespace
}  // namespace semantic_analysis
