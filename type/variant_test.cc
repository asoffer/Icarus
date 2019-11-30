#include "type/variant.h"

#include "test/catch.h"
#include "type/primitive.h"
#include "type/tuple.h"

namespace type {
namespace {

TEST_CASE("Var") {
  SECTION("empty") {
    CHECK(Var({}) == Void());
    // Second time because of caching.
    CHECK(Var({}) == Void());
    Type const* v = Var({});
    CHECK(Var({}) == v);
  }

  SECTION("one") {
    CHECK(Var({Int64}) != Void());
    CHECK(Var({Int64}) == Var({Int64}));
    CHECK(Var({Int64}) == Int64);
    // Second time because of caching.
    CHECK(Var({Int64}) == Int64);
    CHECK(Var({Int64, Int64}) == Int64);
  }

  SECTION("many") {
    CHECK(Var({Int64, Bool}) != Void());
    CHECK(Var({Int64, Bool}) == Var({Bool, Int64}));
    // Second time because of caching.
    CHECK(Var({Int64, Bool}) == Var({Bool, Int64}));
    CHECK(Var({Bool, Int64, Bool}) == Var({Bool, Int64}));
  }
  SECTION("flatten") {
    CHECK(Var({Var({Int64, Bool}), Bool}) == Var({Bool, Int64}));
    CHECK(Var({Var({Int64, Bool}), Var({Float32, Bool})}) ==
          Var({Bool, Float32, Int64}));
  }
}

TEST_CASE("MultiVar") {
  CHECK(MultiVar({std::vector{Int64}, std::vector{Int64, Bool}}) == nullptr);

  CHECK(MultiVar({std::vector{Int64, Bool}, std::vector{Int64, Bool}}) ==
        type::Tup({Int64, Bool}));

  CHECK(MultiVar({std::vector{Int64, Bool}, std::vector{Int64, Int64}}) ==
        Tup({Int64, Var({Bool, Int64})}));
}

TEST_CASE("contains") {
  Variant const& var = Var({Int32, Int64})->as<Variant>();
  CHECK_FALSE(var.contains(Bool));
  CHECK(var.contains(Int32));
  CHECK_FALSE(var.contains(&var));
}

}  // namespace
}  // namespace type
