#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "test/catch.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {

template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->AppendNode(std::move(node));
  return ptr;
}

TEST_CASE("ExtractParams - FunctionLiteral") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, Make<ast::FunctionLiteral>(&mod, "() -> () {}"));
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params = ExtractParams(
        &mod.compiler, Make<ast::FunctionLiteral>(&mod, "(b: bool) -> () {}"));
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        Make<ast::FunctionLiteral>(&mod, "(b: bool, n: int32) -> () {}"));
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "n");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}

TEST_CASE("ExtractParams - Constant declaration") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, Make<ast::Declaration>(&mod, "f ::= () -> () {}"));
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params =
        ExtractParams(&mod.compiler,
                      Make<ast::Declaration>(&mod, "f ::= (b: bool) -> () {}"));
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        Make<ast::Declaration>(&mod, "f ::= (b: bool, n: int32) -> () {}"));
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "n");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}

TEST_CASE("ExtractParams - Non-constant declaration") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, Make<ast::Declaration>(&mod, "f := () -> () {}"));
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params = ExtractParams(
        &mod.compiler, Make<ast::Declaration>(&mod, "f := (b: bool) -> () {}"));
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        Make<ast::Declaration>(&mod, "f := (b: bool, n: int32) -> () {}"));
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}

decltype(auto) GetParams(int, internal::ExprData const &data) {
  return data.params();
};

TEST_CASE("ParamsCoverArgs - empty arguments") {
  auto args = core::FnArgs<type::QualType>(/* pos = */ {}, /* named = */ {});

  SECTION("empty parameters") {
    absl::flat_hash_map<int, internal::ExprData> table;
    // We need to have an entry in the table... but the default will be empty
    // parameters which should have the proper coverage.
    table[0];
    CHECK(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("one parameter") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple parameters") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    table[0].params().append(
        "param1", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }
}

TEST_CASE("ParamsCoverArgs - one positional argument") {
  auto args = core::FnArgs<type::QualType>(
      /* pos = */ {type::QualType::NonConstant(
          type::Var({type::Int64, type::Bool}))},
      /* named = */ {});

  SECTION("empty parameters") {
    absl::flat_hash_map<int, internal::ExprData> table;
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("single overload coverage - negative") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("single overload coverage - positive") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0",
        type::Typed<ast::Declaration const *>(
            nullptr, type::Var({type::Int64, type::Type_, type::Bool})));
    CHECK(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple overload coverage - negative") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0", type::Typed<ast::Declaration const *>(nullptr, type::Type_));

    table[1].params().append(
        "param1", type::Typed<ast::Declaration const *>(
                      nullptr, type::Var({type::Float64, type::Int64})));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple overload coverage - positive") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "param0", type::Typed<ast::Declaration const *>(
                      nullptr, type::Var({type::Type_, type::Bool})));
    table[1].params().append(
        "param1",
        type::Typed<ast::Declaration const *>(
            nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
    CHECK(ParamsCoverArgs(args, table, GetParams));
  }
}

TEST_CASE("ParamsCoverArgs - one named argument") {
  auto const *t = type::Var({type::Int64, type::Bool});
  auto args     = core::FnArgs<type::QualType>(
      /* pos = */ {},
      /* named = */ {{"x", type::QualType::NonConstant(t)}});

  SECTION("empty parameters") {
    absl::flat_hash_map<int, internal::ExprData> table;
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("single overload coverage - mismatched type") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "x", type::Typed<ast::Declaration const *>(nullptr, type::Bool));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("single overload coverage - mismatched name") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append("y",
                             type::Typed<ast::Declaration const *>(nullptr, t));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("single overload coverage - positive") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Int64, type::Type_, type::Bool})));
    CHECK(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple overload coverage - mismatched type") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "x", type::Typed<ast::Declaration const *>(nullptr, type::Type_));

    table[1].params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Float64, type::Int64})));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple overload coverage - mismatched name") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Type_, type::Bool})));
    table[1].params().append(
        "y", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
    CHECK_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  SECTION("multiple overload coverage - positive") {
    absl::flat_hash_map<int, internal::ExprData> table;
    table[0].params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Type_, type::Bool})));
    table[1].params().append(
        "x", type::Typed<ast::Declaration const *>(
                 nullptr, type::Var({type::Ptr(type::Bool), type::Int64})));
    CHECK(ParamsCoverArgs(args, table, GetParams));
  }
}
}  // namespace
}  // namespace compiler
