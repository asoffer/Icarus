#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {
using param_type = core::Param<type::QualType>;

using ::testing::ElementsAre;

decltype(auto) GetParams(int, internal::ExprData const &data) {
  return data.params();
};

TEST(ParamsCoverArgs, EmptyArguments) {
  auto args = core::FnArgs<type::QualType>(/* pos = */ {}, /* named = */ {});

  {  // Empty overloads
    absl::flat_hash_map<int, internal::ExprData> table;
    // We need to have an entry in the table... but the default will be empty
    // parameters which should have the proper coverage.
    table[0];
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One overload
    core::Params params{
        param_type("param0", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads
    core::Params params{
        param_type("param0", type::QualType::NonConstant(type::Bool)),
        param_type("param1", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }
}

TEST(ParamsCoverArgs, OnePositionalArgument) {
  auto args = core::FnArgs<type::QualType>(
      /* pos = */ {type::QualType::NonConstant(
          type::Var({type::Int64, type::Bool}))},
      /* named = */ {});

  {  // No overloads
    absl::flat_hash_map<int, internal::ExprData> table;
    // We need to have an entry in the table... but the default will be empty
    // parameters which should have the proper coverage.
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One overload
    core::Params params{
        param_type("param0", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    core::Params params{param_type(
        "param0", type::QualType::NonConstant(
                      type::Var({type::Int64, type::Type_, type::Bool})))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads

    core::Params params0{
        param_type("param0", type::QualType::NonConstant(type::Type_))};
    core::Params params1{param_type(
        "param1",
        type::QualType::NonConstant(type::Var({type::Float64, type::Int64})))};

    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params0)},
        {1, internal::ExprData(nullptr, params1)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads, matches
    core::Params params0{param_type(
        "param0",
        type::QualType::NonConstant(type::Var({type::Type_, type::Bool})))};
    core::Params params1{param_type(
        "param1", type::QualType::NonConstant(
                      type::Var({type::Ptr(type::Bool), type::Int64})))};

    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params0)},
        {1, internal::ExprData(nullptr, params1)}};
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }
}

TEST(ParamsCoverArgs, OneNamedArgument) {
  auto const *t = type::Var({type::Int64, type::Bool});
  auto args     = core::FnArgs<type::QualType>(
      /* pos = */ {},
      /* named = */ {{"x", type::QualType::NonConstant(t)}});

  {  // Empty parameters
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0];
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, type mismatch
    core::Params params{
        param_type("x", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, name mismatch
    core::Params params{
        param_type("y", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    core::Params params{param_type(
        "x", type::QualType::NonConstant(
                 type::Var({type::Int64, type::Type_, type::Bool})))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatch args
    core::Params params0{
        param_type("x", type::QualType::NonConstant(type::Type_))};
    core::Params params1{param_type("x", type::QualType::NonConstant(type::Var(
                                             {type::Int64, type::Float64})))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatched name
    core::Params params0{param_type("x", type::QualType::NonConstant(type::Var(
                                             {type::Type_, type::Bool})))};
    core::Params params1{
        param_type("y", type::QualType::NonConstant(
                            type::Var({type::Ptr(type::Bool), type::Int64})))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, matches
    core::Params params0{param_type("x", type::QualType::NonConstant(type::Var(
                                             {type::Type_, type::Bool})))};
    core::Params params1{
        param_type("x", type::QualType::NonConstant(
                            type::Var({type::Ptr(type::Bool), type::Int64})))};

    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }
}

}  // namespace
}  // namespace compiler
