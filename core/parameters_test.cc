#include "core/parameters.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {
using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::SizeIs;

TEST(ParameterFlags, Equality) {
  auto d = ParameterFlags::HasDefault();
  auto n = ParameterFlags::MustNotName();
  auto v = ParameterFlags::Variadic();
  EXPECT_TRUE(d == d);
  EXPECT_FALSE(d != d);
  EXPECT_TRUE(n == n);
  EXPECT_FALSE(n != n);
  EXPECT_TRUE(v == v);
  EXPECT_FALSE(v != v);
  EXPECT_FALSE(d == n);
  EXPECT_FALSE(d == v);
  EXPECT_FALSE(v == n);
  EXPECT_TRUE(d != n);
  EXPECT_TRUE(d != v);
  EXPECT_TRUE(v != n);
}


TEST(ParameterFlags, MeetAndJoin) {
  auto d = ParameterFlags::HasDefault();
  auto n = ParameterFlags::MustNotName();
  auto v = ParameterFlags::Variadic();
  EXPECT_EQ((d | d), d);
  EXPECT_EQ((d | n), (n | d));
  EXPECT_EQ(((d | n) | v), (n | (v | d)));
  EXPECT_EQ((d & d), d);
  EXPECT_EQ((d & n), ParameterFlags());
  EXPECT_EQ(((d | n) & (n | v)), n);
}

TEST(ParameterFlags, Comparison) {
  auto d = ParameterFlags::HasDefault();
  auto n = ParameterFlags::MustNotName();
  auto v = ParameterFlags::Variadic();
  EXPECT_LT(d, (d | n));
  EXPECT_LE(d, (d | n));
  EXPECT_LE(d, d);
  EXPECT_FALSE((d | v) < (d | n));
  EXPECT_FALSE((d | v) > (d | n));
  EXPECT_FALSE((d | v) <= (d | n));
  EXPECT_FALSE((d | v) >= (d | n));
  EXPECT_NE((d | v), (d | n));
}

TEST(Parameters, Construction) {
  {
    Parameters<double> params;
    EXPECT_THAT(params, IsEmpty());
    EXPECT_THAT(params, SizeIs(0));
  }

  {
    Parameters<double> params(0);
    EXPECT_THAT(params, IsEmpty());
    EXPECT_THAT(params, SizeIs(0));
  }

  {
    Parameters<double> params(3);
    EXPECT_THAT(params, SizeIs(3));
    EXPECT_THAT(params, ElementsAre(Parameter<double>{.value = 0.0},
                                    Parameter<double>{.value = 0.0},
                                    Parameter<double>{.value = 0.0}));
  }

  {
    Parameters<double> params(3, Parameter<double>{.value = 1.0});
    EXPECT_THAT(params, SizeIs(3));
    EXPECT_THAT(params, ElementsAre(Parameter<double>{.value = 1.0},
                                    Parameter<double>{.value = 1.0},
                                    Parameter<double>{.value = 1.0}));
  }

  {
    std::vector v{Parameter<double>{.name = "a", .value = 1.0},
                  Parameter<double>{.name = "b", .value = 2.0},
                  Parameter<double>{.name = "c", .value = 3.0}};
    Parameters<double> params(std::move(v));
    EXPECT_THAT(params, SizeIs(3));
    EXPECT_THAT(params,
                ElementsAre(Parameter<double>{.name = "a", .value = 1.0},
                            Parameter<double>{.name = "b", .value = 2.0},
                            Parameter<double>{.name = "c", .value = 3.0}));
  }

  {
    Parameters params{Parameter<double>{.name = "a", .value = 1.0},
                      Parameter<double>{.name = "b", .value = 2.0},
                      Parameter<double>{.name = "c", .value = 3.0}};
    EXPECT_THAT(params, SizeIs(3));
    EXPECT_THAT(params,
                ElementsAre(Parameter<double>{.name = "a", .value = 1.0},
                            Parameter<double>{.name = "b", .value = 2.0},
                            Parameter<double>{.name = "c", .value = 3.0}));
  }

  {
    std::vector v{Parameter<double>{.name = "a", .value = 1.0},
                  Parameter<double>{.name = "b", .value = 2.0},
                  Parameter<double>{.name = "c", .value = 3.0}};
    Parameters<double> params(v.begin(), v.end());
    EXPECT_THAT(params, SizeIs(3));
    EXPECT_THAT(params,
                ElementsAre(Parameter<double>{.name = "a", .value = 1.0},
                            Parameter<double>{.name = "b", .value = 2.0},
                            Parameter<double>{.name = "c", .value = 3.0}));
  }
}

TEST(Parameters, Clear) {
  Parameters params{Parameter<double>{.name = "a", .value = 1.0},
                    Parameter<double>{.name = "b", .value = 2.0},
                    Parameter<double>{.name = "c", .value = 3.0}};
  EXPECT_THAT(params, SizeIs(3));
  params.clear();
  EXPECT_THAT(params, SizeIs(0));
  EXPECT_THAT(params, IsEmpty());
}

TEST(Parameters, TryGet) {
  Parameters params{Parameter<double>{.name = "a", .value = 1.0},
                    Parameter<double>{.name = "b", .value = 2.0},
                    Parameter<double>{.name = "c", .value = 3.0}};

  Parameters const const_params{Parameter<double>{.name = "a", .value = 1.0},
                                Parameter<double>{.name = "b", .value = 2.0},
                                Parameter<double>{.name = "c", .value = 3.0}};
  EXPECT_THAT(params.try_get("a"),
              Pair(Pointee(Parameter<double>{.name = "a", .value = 1.0}), 0));
  EXPECT_THAT(params.try_get("b"),
              Pair(Pointee(Parameter<double>{.name = "b", .value = 2.0}), 1));
  EXPECT_THAT(params.try_get("x"), Pair(nullptr, _));

  EXPECT_THAT(const_params.try_get("a"),
              Pair(Pointee(Parameter<double>{.name = "a", .value = 1.0}), 0));
  EXPECT_THAT(const_params.try_get("b"),
              Pair(Pointee(Parameter<double>{.name = "b", .value = 2.0}), 1));
  EXPECT_THAT(const_params.try_get("x"), Pair(nullptr, _));
}

TEST(Parameters, Access) {
  Parameters params{Parameter<double>{.name = "a", .value = 1.0},
                    Parameter<double>{.name = "b", .value = 2.0},
                    Parameter<double>{.name = "c", .value = 3.0}};

  Parameters const const_params{Parameter<double>{.name = "a", .value = 1.0},
                                Parameter<double>{.name = "b", .value = 2.0},
                                Parameter<double>{.name = "c", .value = 3.0}};
  EXPECT_EQ(const_params["a"], (Parameter<double>{.name = "a", .value = 1.0}));
  EXPECT_EQ(const_params["b"], (Parameter<double>{.name = "b", .value = 2.0}));

  auto& pa = params["a"];
  EXPECT_EQ(pa, (Parameter<double>{.name = "a", .value = 1.0}));
  EXPECT_EQ(params["b"], (Parameter<double>{.name = "b", .value = 2.0}));

  params["a"].value = 4.0;
  EXPECT_EQ(params["a"], (Parameter<double>{.name = "a", .value = 4.0}));
  EXPECT_EQ(params["b"], (Parameter<double>{.name = "b", .value = 2.0}));

  EXPECT_EQ(pa, (Parameter<double>{.name = "a", .value = 4.0}));
  EXPECT_EQ(params["a"], (Parameter<double>{.name = "a", .value = 4.0}));
  EXPECT_EQ(params["b"], (Parameter<double>{.name = "b", .value = 2.0}));

  EXPECT_EQ(&params[0], &pa);
  EXPECT_EQ(params[0], params["a"]);
  EXPECT_EQ(params[1], params["b"]);
  EXPECT_EQ(params[2], params["c"]);
}

TEST(Parameters, Append) {
  Parameters<double> params;
  EXPECT_EQ(params.size(), 0u);
  params.append("pi", 3.14, ParameterFlags::MustNotName());
  params.append("", 1234);

  EXPECT_THAT(
      params,
      ElementsAre(Parameter<double>{.name  = "pi",
                                    .value = 3.14,
                                    .flags = ParameterFlags::MustNotName()},
                  Parameter<double>{.value = 1234}));
}

}  // namespace
}  // namespace core
