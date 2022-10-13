#include "semantic_analysis/type_verification/casting.h"

#include "gtest/gtest.h"

namespace semantic_analysis {
namespace {

TEST(CanCast, Integers) {
  TypeSystem type_system;
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(16), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(32), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), I(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), I(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), I(50), type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), I(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(16), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(32), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(8)), U(100), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(88)), U(100), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(50)), U(50), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(I(100)), U(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(16), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(32), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), I(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), I(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), I(50), type_system), CastKind::None);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), I(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(16), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(32), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(8)), U(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(88)), U(100), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(50)), U(50), type_system), CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(U(100)), U(88), type_system), CastKind::None);

  EXPECT_EQ(CanCast(Constant(Integer), Integer, type_system),
            CastKind::InPlace);
  EXPECT_EQ(CanCast(QualifiedType(Integer), Integer, type_system),
            CastKind::InPlace);

  EXPECT_EQ(CanCast(Constant(Integer), U(17), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), U(17), type_system),
            CastKind::None);
  EXPECT_EQ(CanCast(Constant(Integer), I(17), type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(Integer), I(17), type_system), CastKind::None);

  EXPECT_EQ(CanCast(Constant(U(17)), Integer, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(U(17)), Integer, type_system),
            CastKind::None);
  EXPECT_EQ(CanCast(Constant(I(17)), Integer, type_system), CastKind::Implicit);
  EXPECT_EQ(CanCast(QualifiedType(I(17)), Integer, type_system),
            CastKind::None);
}

}  // namespace
}  // namespace semantic_analysis

