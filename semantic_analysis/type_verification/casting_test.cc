#include "semantic_analysis/type_verification/casting.h"

#include "gtest/gtest.h"

namespace semantic_analysis {
namespace {

TEST(CanCast, Integers) {
  TypeSystem type_system;
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(16), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(32), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), I(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), I(50), type_system),
            CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), I(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(16), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(32), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(100), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), U(100), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), U(50), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), U(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(16), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(32), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), I(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), I(50), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), I(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(16), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(32), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), U(100), type_system),
            CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), U(50), type_system),
            CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), U(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(Constant(Integer), Integer, type_system),
            CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(Integer), Integer, type_system),
            CastKind::InPlace);

  EXPECT_EQ(CanCast(Constant(Integer), U(17), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), U(17), type_system),
            CastKind::None);
  EXPECT_EQ(CanCast(Constant(Integer), I(17), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), I(17), type_system),
            CastKind::None);

  EXPECT_EQ(CanCast(Constant(U(17)), Integer, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(17)), Integer, type_system),
            CastKind::None);
  EXPECT_EQ(CanCast(Constant(I(17)), Integer, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(17)), Integer, type_system),
            CastKind::None);
}

TEST(CanCast, FloatingPoint) {
  TypeSystem type_system;
  EXPECT_EQ(CanCast(QualifiedType(F32), F32, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(F32), F64, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(F64), F32, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(F64), F64, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(F32), F32, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(F32), F64, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(Constant(F64), F32, type_system), CastKind::None);
  EXPECT_EQ(CanCast(Constant(F64), F64, type_system), CastKind::InPlace);
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

  EXPECT_EQ(CanCast(QualifiedType(bi), bi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bi), pi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(pi), bi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pi), pi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(bi), bi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(bi), pi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(Constant(pi), bi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(Constant(pi), pi, type_system), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(bbi), bbi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), bpi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), pbi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bbi), ppi, type_system), CastKind::InPlace);

  EXPECT_EQ(CanCast(QualifiedType(bpi), bbi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), bpi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(bpi), pbi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), ppi, type_system), CastKind::InPlace); 

  EXPECT_EQ(CanCast(QualifiedType(pbi), bbi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), bpi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), pbi, type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(pbi), ppi, type_system), CastKind::InPlace); 

  EXPECT_EQ(CanCast(QualifiedType(ppi), bbi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), bpi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), pbi, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), ppi, type_system), CastKind::InPlace); 

  EXPECT_EQ(CanCast(QualifiedType(bbi), ppf, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(bpi), ppf, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(pbi), ppf, type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(ppi), ppf, type_system), CastKind::None);
}

}  // namespace
}  // namespace semantic_analysis
