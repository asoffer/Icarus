#include <type_traits>

#include "Catch2/single_include/catch2/catch.hpp"
#include "base/macros.h"

#define REQUIRE_ASSIGN(var, expr)                                              \
  ICARUS_TEST_REQUIRE_ASSIGN_IMPL1(var, expr, ICARUS_CAT(expr__, __LINE__, __))

#define ICARUS_TEST_REQUIRE_ASSIGN_IMPL1(var, expr, tmp)                       \
  ICARUS_TEST_REQUIRE_ASSIGN_IMPL2(var, expr, tmp)

// TODO the error message from here will be pretty bad.
#define ICARUS_TEST_REQUIRE_ASSIGN_IMPL2(var, expr, temp)                      \
  auto&& temp = (expr);                                                        \
  REQUIRE(temp);                                                               \
  var = *std::move(temp)
