#include "Catch2/single_include/catch2/catch.hpp"
#include "base/macros.h"

#define REQUIRE_ASSIGN(var, expr)                                              \
  INTERNAL_TEST_REQUIRE_ASSIGN_(var, expr, CAT(expr__, __LINE__, __))
#define INTERNAL_TEST_REQUIRE_ASSIGN_(var, expr, tmp)                          \
  INTERNAL_TEST_REQUIRE_ASSIGN__(var, expr, tmp)

// TODO the error message from here will be pretty bad.
#define INTERNAL_TEST_REQUIRE_ASSIGN__(var, expr, temp)                        \
  auto&& temp = (expr);                                                        \
  REQUIRE(temp);                                                               \
  var = *std::move(temp)

