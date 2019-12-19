#include "compiler/dispatch/match.h"

#include "ast/ast.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "test/catch.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"
#include "type/variant.h"

namespace compiler {
namespace {

template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->Process(std::move(node));
  return ptr;
}

TEST_CASE("() -> () {}") {
  test::TestModule mod;
  auto params = ExtractParams(&mod.compiler,
                              Make<ast::FunctionLiteral>(&mod, "() -> () {}"));

  SECTION("No args") {
    auto args = core::FnArgs<type::QualType>({}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 0);
  }

  SECTION("Positional args") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Int32)}, {});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Named args") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"abc", type::QualType::Constant(type::Int32)}});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }
}

TEST_CASE("(n: int32) -> () {}") {
  test::TestModule mod;
  auto params = ExtractParams(
      &mod.compiler, Make<ast::FunctionLiteral>(&mod, "(n: int32) -> () {}"));

  SECTION("Call without args") {
    auto args = core::FnArgs<type::QualType>({}, {});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call positionally with correct type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Int32)}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "n");
    CHECK(matched_params.at(0).value.type() == type::Int32);
  }

  SECTION("Call positionally with incorrect type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Bool)}, {});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call named with correct type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "n");
    CHECK(matched_params.at(0).value.type() == type::Int32);
  }

  SECTION("Call named with incorrect type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Bool)}});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call named with incorrect name") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"N", type::QualType::Constant(type::Int32)}});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }
}

TEST_CASE("(n: int32, b := true) -> () {}") {
  test::TestModule mod;
  auto params = ExtractParams(
      &mod.compiler,
      Make<ast::FunctionLiteral>(&mod, "(n: int32, b := true) -> () {}"));

  SECTION("Call without args") {
    auto args = core::FnArgs<type::QualType>({}, {});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call positionally with correct type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Int32)}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 2);
    CHECK(matched_params.at(0).name == "n");
    CHECK(matched_params.at(0).value.type() == type::Int32);
    CHECK(matched_params.at(1).name == "b");
    CHECK(matched_params.at(1).value.type() == type::Bool);
  }

  SECTION("Call positionally with incorrect type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Bool)}, {});
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call named with correct type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 2);
    CHECK(matched_params.at(0).name == "n");
    CHECK(matched_params.at(0).value.type() == type::Int32);
    CHECK(matched_params.at(1).name == "b");
    CHECK(matched_params.at(1).value.type() == type::Bool);
  }

  SECTION("Call named with explicit second argument") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Int32)},
             {"b", type::QualType::Constant(type::Bool)}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 2);
    CHECK(matched_params.at(0).name == "n");
    CHECK(matched_params.at(0).value.type() == type::Int32);
    CHECK(matched_params.at(1).name == "b");
    CHECK(matched_params.at(1).value.type() == type::Bool);
  }

  SECTION("Call named with incorrect type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"n", type::QualType::Constant(type::Bool)}});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call named with incorrect name") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"N", type::QualType::Constant(type::Int32)}});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }
}

TEST_CASE("(x: int32 | bool) -> () {}") {
  test::TestModule mod;
  auto params = ExtractParams(
      &mod.compiler,
      Make<ast::FunctionLiteral>(&mod, "(x: int32 | bool) -> () {}"));

  SECTION("Call without args") {
    auto args           = core::FnArgs<type::QualType>({}, {});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call positionally with matching type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Int32)}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Int32);
  }

  SECTION("Call positionally with matching type") {
    auto args =
        core::FnArgs<type::QualType>({type::QualType::Constant(type::Bool)}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Bool);
  }

  SECTION("Call positionally with overlapping type") {
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Var({type::Bool, type::Float32}))}, {});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Bool);
  }

  SECTION("Call positionally with non-overlapping type") {
    auto args = core::FnArgs<type::QualType>(
        {type::QualType::Constant(type::Var({type::Int16, type::Float32}))}, {});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }

  SECTION("Call named with matching type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"x", type::QualType::Constant(type::Int32)}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Int32);
  }

  SECTION("Call named with matching type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"x", type::QualType::Constant(type::Bool)}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Bool);
  }

  SECTION("Call named with overlapping type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"x",
              type::QualType::Constant(type::Var({type::Bool, type::Float32}))}});
    REQUIRE_OK_AND_ASSIGN(auto matched_params, MatchArgsToParams(params, args));
    CHECK(matched_params.size() == 1);
    CHECK(matched_params.at(0).name == "x");
    CHECK(matched_params.at(0).value.type() == type::Bool);
  }

  SECTION("Call named with non-overlapping type") {
    auto args = core::FnArgs<type::QualType>(
        {}, {{"x", type::QualType::Constant(
                       type::Var({type::Int16, type::Float32}))}});
    auto matched_params = MatchArgsToParams(params, args);
    CHECK_FALSE(MatchArgsToParams(params, args));
  }
}

}  // namespace
}  // namespace compiler
