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
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    table[0].mutable_params().append(
        "param1", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
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
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0",
        type::Typed<ast::Declaration const *>(
            nullptr, type::Var({type::Int64, type::Type_, type::Bool})));
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Type_));

    table[1].mutable_params().append(
        "param1", type::Typed<ast::Declaration const *>(
                      nullptr, type::Var({type::Float64, type::Int64})));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads, matches
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "param0", type::Typed<ast::Declaration const *>(
                      nullptr, type::Var({type::Type_, type::Bool})));
    table[1].mutable_params().append(
        "param1",
        type::Typed<ast::Declaration const *>(
            nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
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
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, name mismatch
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "y", type::Typed<ast::Declaration const *>(nullptr, t));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One parameter, matches
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Int64, type::Type_, type::Bool})));
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatch args
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(nullptr, type::Type_));

    table[1].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Float64, type::Int64})));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, mismatched name
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Type_, type::Bool})));
    table[1].mutable_params().append(
        "y", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overload coverage, matches
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Type_, type::Bool})));
    table[1].mutable_params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }
}

}  // namespace
}  // namespace compiler
