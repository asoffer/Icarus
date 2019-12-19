#include "type/qual_type.h"

#include "test/catch.h"
#include "type/primitive.h"

namespace {

TEST_CASE("Regularity") {
  CHECK(type::QualType::Error() == type::QualType::Error());

  CHECK(type::QualType::Constant(type::Int32) ==
        type::QualType::Constant(type::Int32));
  CHECK(type::QualType::Constant(type::Int32) !=
        type::QualType::Constant(type::Int64));

  CHECK(type::QualType::NonConstant(type::Int32) ==
        type::QualType::NonConstant(type::Int32));
  CHECK(type::QualType::NonConstant(type::Int32) !=
        type::QualType::Constant(type::Int32));
  CHECK(type::QualType::NonConstant(type::Int32) !=
        type::QualType::NonConstant(type::Int64));

  auto q = type::QualType::Constant(type::Int64);

  CHECK(q == type::QualType::Constant(type::Int64));

  CHECK(q != type::QualType::Constant(type::Bool));
  q = type::QualType::Constant(type::Bool);
  CHECK(q == type::QualType::Constant(type::Bool));
}

TEST_CASE("Access") {
  auto q = type::QualType::Constant(type::Int32);
  CHECK(q.constant());
  CHECK(q.type() == type::Int32);

  auto q2 = type::QualType::NonConstant(type::Bool);
  CHECK_FALSE(q2.constant());
  CHECK(q2.type() == type::Bool);
}

TEST_CASE("Streaming") {
  std::stringstream ss;

  SECTION("constant") {
    auto q = type::QualType::Constant(type::Int32);
    ss << q;
    CHECK(ss.str() == "const[int32]");
  }

  SECTION("non-constant") {
    auto q = type::QualType::NonConstant(type::Bool);
    ss << q;
    CHECK(ss.str() == "non-const[bool]");
  }
}

TEST_CASE("Ok") {
  CHECK_FALSE(type::QualType::Error().ok());
  CHECK_FALSE(type::QualType::Error());
  CHECK(type::QualType::Constant(type::Bool).ok());
  CHECK(type::QualType::Constant(type::Bool));
}

}  // namespace
