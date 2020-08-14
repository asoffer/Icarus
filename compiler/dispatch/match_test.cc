#include "compiler/dispatch/match.h"

#include "ast/ast.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {

template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->AppendNode(std::move(node), mod->consumer);
  return ptr;
}

TEST(Match, NoParams) {
  test::TestModule mod;
  auto params =
      mod.compiler.type_of(Make<ast::FunctionLiteral>(&mod, "() -> () {}"))
          ->as<type::Function>()
          .params();

  {  // No args
    auto args = core::FnArgs<type::QualType>({}, {});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 0);
  }

  {  // Positional args
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Int32)}, {});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Named args
    auto args = core::FnArgs<type::QualType>(
        {}, {{"abc", type::QualType::Constant(type::Int32)}});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }
}

TEST(Match, OneParam) {
  test::TestModule mod;
  auto params =
      mod.compiler
          .type_of(Make<ast::FunctionLiteral>(&mod, "(n: int32) -> () {}"))
          ->as<type::Function>()
          .params();

  {  // Call without args
    auto args = core::FnArgs<type::QualType>({}, {});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call positionally with correct type
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Int32)}, {});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 1);
    EXPECT_EQ(matched_params[0].name, "n");
    EXPECT_EQ(matched_params[0].value,
              type::QualType::NonConstant(type::Int32));
  }

  {  // Call positionally with incorrect type
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Bool)}, {});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call named with correct type
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)}});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 1);
    EXPECT_EQ(matched_params[0].name, "n");
    EXPECT_EQ(matched_params[0].value,
              type::QualType::NonConstant(type::Int32));
  }

  {  // Call named with incorrect type
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Bool)}});
    auto matched_params = MatchArgsToParams(params, args);
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call named with incorrect name
    auto args = core::FnArgs<type::QualType>(
        {}, {{"N", type::QualType::Constant(type::Int32)}});
    auto matched_params = MatchArgsToParams(params, args);
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }
}

TEST(Match, TwoParamsOneDefaulted) {
  test::TestModule mod;
  auto params = mod.compiler
                    .type_of(Make<ast::FunctionLiteral>(
                        &mod, "(n: int32, b := true) -> () {}"))
                    ->as<type::Function>()
                    .params();

  {  // Call without args
    auto args = core::FnArgs<type::QualType>({}, {});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call positionally with correct type
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Int32)}, {});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 2);
    EXPECT_EQ(matched_params[0].name, "n");
    EXPECT_EQ(matched_params[0].value,
              type::QualType::NonConstant(type::Int32));
    EXPECT_EQ(matched_params[1].name, "b");
    EXPECT_EQ(matched_params[1].value, type::QualType::NonConstant(type::Bool));
  }

  {  // Call positionally with incorrect type
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Bool)}, {});
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call named with correct type
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)}});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 2);
    EXPECT_EQ(matched_params[0].name, "n");
    EXPECT_EQ(matched_params[0].value,
              type::QualType::NonConstant(type::Int32));
    EXPECT_EQ(matched_params[1].name, "b");
    EXPECT_EQ(matched_params[1].value, type::QualType::NonConstant(type::Bool));
  }

  {  // Call named with explicit second argument
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)},
             {"b", type::QualType::Constant(type::Bool)}});
    ASSIGN_OR(FAIL(),  //
              auto matched_params, MatchArgsToParams(params, args));
    EXPECT_EQ(matched_params.size(), 2);
    EXPECT_EQ(matched_params[0].name, "n");
    EXPECT_EQ(matched_params[0].value,
              type::QualType::NonConstant(type::Int32));
    EXPECT_EQ(matched_params[1].name, "b");
    EXPECT_EQ(matched_params[1].value, type::QualType::NonConstant(type::Bool));
  }

  {  // Call named with incorrect type
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Bool)}});
    auto matched_params = MatchArgsToParams(params, args);
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }

  {  // Call named with incorrect name
    auto args = core::FnArgs<type::QualType>(
        {}, {{"N", type::QualType::Constant(type::Int32)}});
    auto matched_params = MatchArgsToParams(params, args);
    EXPECT_FALSE(MatchArgsToParams(params, args));
  }
}

}  // namespace
}  // namespace compiler
