#include "semantic_analysis/type_system.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace semantic_analysis {

TEST(Qualifiers, Comparisons) {
  EXPECT_TRUE(Qualifiers::Constant() <= Qualifiers::Constant());
  EXPECT_TRUE(Qualifiers::Constant() >= Qualifiers::Constant());
  EXPECT_TRUE(Qualifiers::Constant() == Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Constant() < Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Constant() > Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Constant() != Qualifiers::Constant());

  EXPECT_FALSE(Qualifiers::Reference() <= Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Reference() >= Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Reference() == Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Reference() < Qualifiers::Constant());
  EXPECT_FALSE(Qualifiers::Reference() > Qualifiers::Constant());
  EXPECT_TRUE(Qualifiers::Reference() != Qualifiers::Constant());

  // `Buffer` subsumes `reference`.
  EXPECT_TRUE(Qualifiers::Reference() <= Qualifiers::Buffer());
  EXPECT_FALSE(Qualifiers::Reference() >= Qualifiers::Buffer());
  EXPECT_FALSE(Qualifiers::Reference() == Qualifiers::Buffer());
  EXPECT_TRUE(Qualifiers::Reference() < Qualifiers::Buffer());
  EXPECT_FALSE(Qualifiers::Reference() > Qualifiers::Buffer());
  EXPECT_TRUE(Qualifiers::Reference() != Qualifiers::Buffer());
}

TEST(Qualifiers, Union) {
  auto buf_ref = (Qualifiers::Reference() | Qualifiers::Buffer());
  EXPECT_TRUE(buf_ref <= Qualifiers::Buffer());
  EXPECT_TRUE(buf_ref >= Qualifiers::Buffer());
  EXPECT_TRUE(buf_ref == Qualifiers::Buffer());
  EXPECT_FALSE(buf_ref < Qualifiers::Buffer());
  EXPECT_FALSE(buf_ref > Qualifiers::Buffer());
  EXPECT_FALSE(buf_ref != Qualifiers::Buffer());

  auto ref_const = (Qualifiers::Reference() | Qualifiers::Constant());
  EXPECT_TRUE(ref_const > Qualifiers::Reference());
  EXPECT_TRUE(ref_const > Qualifiers::Constant());
  EXPECT_FALSE(ref_const < Qualifiers::Constant());
  EXPECT_FALSE(ref_const < Qualifiers::Reference());
  EXPECT_FALSE(ref_const < Qualifiers::Buffer());
}

TEST(Qualifiers, Intersection) {
  auto buf_ref   = (Qualifiers::Reference() | Qualifiers::Buffer());
  auto ref_const = (Qualifiers::Reference() | Qualifiers::Constant());
  EXPECT_EQ((buf_ref & ref_const), Qualifiers::Reference());
  EXPECT_EQ((Qualifiers::Reference() & Qualifiers::Buffer()),
            Qualifiers::Reference());
  EXPECT_EQ((Qualifiers::Reference() & Qualifiers::Constant()), Qualifiers());
}

TEST(QualifiedType, Constant) {
  EXPECT_EQ(Constant(Bool).type(), Bool);
  EXPECT_EQ(Constant(Bool).qualifiers(), Qualifiers::Constant());
  EXPECT_EQ(Constant(Constant(Bool)).type(), Bool);
  EXPECT_EQ(Constant(Constant(Bool)).qualifiers(), Qualifiers::Constant());
}

TEST(Predicates, IsIntegral) {
  TypeSystem ts;
  EXPECT_TRUE(IsIntegral(I(7)));
  EXPECT_TRUE(IsIntegral(U(7)));
  EXPECT_TRUE(IsIntegral(I(32)));
  EXPECT_TRUE(IsIntegral(U(32)));
  EXPECT_TRUE(IsIntegral(Integer));
  EXPECT_FALSE(IsIntegral(Bool));
  EXPECT_FALSE(IsIntegral(F32));
  EXPECT_FALSE(IsIntegral(F64));
  EXPECT_FALSE(IsIntegral(core::PointerType(ts, I(32))));

  EXPECT_TRUE(IsNumeric(I(7)));
  EXPECT_TRUE(IsNumeric(U(7)));
  EXPECT_TRUE(IsNumeric(I(32)));
  EXPECT_TRUE(IsNumeric(U(32)));
  EXPECT_TRUE(IsNumeric(Integer));
  EXPECT_FALSE(IsNumeric(Bool));
  EXPECT_TRUE(IsNumeric(F32));
  EXPECT_TRUE(IsNumeric(F64));
  EXPECT_FALSE(IsNumeric(core::PointerType(ts, I(32))));
}

}  // namespace semantic_analysis
