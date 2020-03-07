#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {
using param_type = core::Param<type::Type const *>;

using ::testing::ElementsAre;

type::Type const *ExtractType(type::Typed<ast::Declaration const *> decl) {
  return decl.type();
}

template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->AppendNode(std::move(node), mod->consumer);
  mod->compiler.CompleteDeferredBodies();
  return ptr;
}

TEST(ExtractParams, FunctionLiteral) {
  {  // Empty
    test::TestModule mod;
    EXPECT_THAT(ExtractParams(&mod.compiler,
                              Make<ast::FunctionLiteral>(&mod, "() -> () {}"))
                    .Transform(ExtractType),
                ElementsAre());
  }

  {  // One argument
    test::TestModule mod;
    EXPECT_THAT(ExtractParams(&mod.compiler, Make<ast::FunctionLiteral>(
                                                 &mod, "(b: bool) -> () {}"))
                    .Transform(ExtractType),
                ElementsAre(param_type("b", type::Bool)));
  }

  {  // Multiple arguments
    test::TestModule mod;
    EXPECT_THAT(
        ExtractParams(&mod.compiler, Make<ast::FunctionLiteral>(
                                         &mod, "(b: bool, n: int32) -> () {}"))
            .Transform(ExtractType),
        ElementsAre(param_type("b", type::Bool), param_type("n", type::Int32)));
  }
}

TEST(ExtractParams, ConstantDeclaration) {
  {  // Empty
    test::TestModule mod;
    EXPECT_THAT(ExtractParams(&mod.compiler,
                              Make<ast::Declaration>(&mod, "f ::= () -> () {}"))
                    .Transform(ExtractType),
                ElementsAre());
  }

  {  // One argument
    test::TestModule mod;
    EXPECT_THAT(
        ExtractParams(&mod.compiler,
                      Make<ast::Declaration>(&mod, "f ::= (b: bool) -> () {}"))
            .Transform(ExtractType),
        ElementsAre(param_type("b", type::Bool)));
  }

  {  // Multiple arguments
    test::TestModule mod;
    EXPECT_THAT(
        ExtractParams(
            &mod.compiler,
            Make<ast::Declaration>(&mod, "f ::= (b: bool, n: int32) -> () {}"))
            .Transform(ExtractType),
        ElementsAre(param_type("b", type::Bool), param_type("n", type::Int32)));
  }
}

TEST(ExtractParams, NonConstantDeclaration) {
  {  // Empty
    test::TestModule mod;
    EXPECT_THAT(ExtractParams(&mod.compiler,
                              Make<ast::Declaration>(&mod, "f := () -> () {}"))
                    .Transform(ExtractType),
                ElementsAre());
  }

  {  // One argument
    test::TestModule mod;
    EXPECT_THAT(
        ExtractParams(&mod.compiler,
                      Make<ast::Declaration>(&mod, "f := (b: bool) -> () {}"))
            .Transform(ExtractType),
        ElementsAre(param_type("b", type::Bool)));
  }

  {  // Multiple arguments
    test::TestModule mod;
    EXPECT_THAT(
        ExtractParams(
            &mod.compiler,
            Make<ast::Declaration>(&mod, "f := (b: bool, n: int32) -> () {}"))
            .Transform(ExtractType),
        ElementsAre(param_type("b", type::Bool), param_type("n", type::Int32)));
  }
}

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
    core::Params params{core::Param<type::Type const *>("param0", type::Bool)};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads
    core::Params params{core::Param<type::Type const *>("param0", type::Bool),
                        core::Param<type::Type const *>("param1", type::Bool)};
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
    core::Params params{core::Param<type::Type const *>("param0", type::Bool)};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    core::Params params{core::Param<type::Type const *>(
        "param0", type::Var({type::Int64, type::Type_, type::Bool}))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params)}};
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads

    core::Params params0{
        core::Param<type::Type const *>("param0", type::Type_)};
    core::Params params1{core::Param<type::Type const *>(
        "param1", type::Var({type::Float64, type::Int64}))};

    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, params0)},
        {1, internal::ExprData(nullptr, params1)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads, matches
    core::Params params0{core::Param<type::Type const *>(
        "param0", type::Var({type::Type_, type::Bool}))};
    core::Params params1{core::Param<type::Type const *>(
        "param1", type::Var({type::Ptr(type::Bool), type::Int64}))};

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
    core::Params params{core::Param<type::Type const *>("x", type::Bool)};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, name mismatch
    core::Params params{core::Param<type::Type const *>("y", type::Bool)};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    core::Params params{core::Param<type::Type const *>(
        "x", type::Var({type::Int64, type::Type_, type::Bool}))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params))}};
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatch args
    core::Params params0{core::Param<type::Type const *>("x", type::Type_)};
    core::Params params1{core::Param<type::Type const *>(
        "x", type::Var({type::Int64, type::Float64}))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatched name
    core::Params params0{core::Param<type::Type const *>(
        "x", type::Var({type::Type_, type::Bool}))};
    core::Params params1{core::Param<type::Type const *>(
        "y", type::Var({type::Ptr(type::Bool), type::Int64}))};
    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, matches
    core::Params params0{core::Param<type::Type const *>(
        "x", type::Var({type::Type_, type::Bool}))};
    core::Params params1{core::Param<type::Type const *>(
        "x", type::Var({type::Ptr(type::Bool), type::Int64}))};

    absl::flat_hash_map<int, internal::ExprData> table{
        {0, internal::ExprData(nullptr, std::move(params0))},
        {1, internal::ExprData(nullptr, std::move(params1))}};

    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }
}

}  // namespace
}  // namespace compiler
