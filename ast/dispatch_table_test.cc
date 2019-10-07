#include "ast/dispatch_table.h"

#include <string_view>

#include "ast/ast.h"
#include "ast/overload_set.h"
#include "test/catch.h"
#include "test/util.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/variant.h"

namespace ast {
namespace {
core::FnArgs<std::pair<Expression const*, compiler::VerifyResult>>
ResultsForArgs(compiler::Compiler* visitor,
               core::FnArgs<Expression const*> const& args) {
  return args.Transform([visitor](Expression const* expr) {
    return std::pair<Expression const*, compiler::VerifyResult>(
        expr, *ASSERT_NOT_NULL(visitor->prior_verification_attempt(expr)));
  });
}

TEST_CASE("() -> ()") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("() -> () {}", std::move(pos), std::move(named),
                          &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with positional arg") {
    auto [os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto [os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with named arg") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto [os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(b: bool) -> ()") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(b: bool) -> () {}", std::move(pos),
                          std::move(named), &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto [os, call_expr] = call_with({}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"n", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto [os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto [os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "false"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(b := true) -> ()") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(b := true) -> () {}", std::move(pos),
                          std::move(named), &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto [os, call_expr] = call_with({}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"n", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto [os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto [os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "false"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(n: int64, b: bool) -> ()") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(n: int64, b: bool) -> () {}", std::move(pos),
                          std::move(named), &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional args - incorrect order") {
    auto [os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional args - correct order") {
    auto [os, call_expr] = call_with({"3", "true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, correct names ") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with multiple named args - incorrect types, correct names") {
    auto [os, call_expr] = call_with({}, {{"b", "true"}, {"n", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, incorrect names") {
    auto [os, call_expr] = call_with({}, {{"B", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named args - incorrect types, incorrect names") {
    auto [os, call_expr] = call_with({}, {{"B", "true"}, {"n", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto [os, call_expr] = call_with({"3"}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Void()));
  }

  SECTION("with both positional and named args - named has incorrect type") {
    auto [os, call_expr] = call_with({"3"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION(
      "with both positional and named args - positional has incorrect type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with both positional and named args - both have  incorrect type") {
    auto [os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(T :: type) -> *T") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(T :: type) -> *T {}", std::move(pos),
                          std::move(named), &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one positional arg - (bool)") {
    auto [os, call_expr] = call_with({"bool"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - (int64)") {
    auto [os, call_expr] = call_with({"int64"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - (T = bool)") {
    auto [os, call_expr] = call_with({}, {{"T", "bool"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - (t = bool)") {
    auto [os, call_expr] = call_with({}, {{"t", "bool"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - (T = true)") {
    auto [os, call_expr] = call_with({}, {{"T", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - (t = true)") {
    auto [os, call_expr] = call_with({}, {{"t", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(T ::= bool) -> *T") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(T ::= bool) -> *T {}", std::move(pos),
                          std::move(named), &compiler);
  };

  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - correct type") {
    auto [os, call_expr] = call_with({"bool"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"int64"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - correct type, correct name") {
    auto [os, call_expr] = call_with({}, {{"T", "bool"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"t", "bool"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto [os, call_expr] = call_with({}, {{"T", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"t", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("(val: T, T ::= bool) -> *T") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  auto call_with = [&](std::vector<std::string>&& pos,
                       absl::flat_hash_map<std::string, std::string>&& named) {
    return test::MakeCall("(val: T, T ::= bool) -> *T", std::move(pos),
                          std::move(named), &compiler);
  };
  SECTION("without args") {
    auto [os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto [os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    auto [os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto [os, call_expr] = call_with({}, {{"val", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"x", "true"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto [os, call_expr] = call_with({}, {{"val", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto [os, call_expr] = call_with({}, {{"x", "3"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple positional arguments - (bool)") {
    auto [os, call_expr] = call_with({"true", "bool"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple positional arguments - (int64)") {
    auto [os, call_expr] = call_with({"3", "int64"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with multiple positional arguments - type mismatch") {
    auto [os, call_expr] = call_with({"3", "bool"}, {});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("with multiple named arguments - (bool)") {
    auto [os, call_expr] = call_with({}, {{"val", "true"}, {"T", "bool"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple named arguments - (int64)") {
    auto [os, call_expr] = call_with({}, {{"val", "3"}, {"T", "int64"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - correct types") {
    auto [os, call_expr] = call_with({"3"}, {{"T", "int64"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - incorrect types") {
    auto [os, call_expr] = call_with({"true"}, {{"T", "int64"}});
    CHECK(VerifyDispatch(&compiler, call_expr, os,
                         ResultsForArgs(&compiler, call_expr->args())) ==
          compiler::VerifyResult::Error());
  }
}

TEST_CASE("overload set") {
  module::Module mod;
  compiler::Compiler compiler(&mod);

  std::vector<std::unique_ptr<ast::Node>> nodes;
  nodes.push_back(test::ParseAs<Declaration>("f ::= (val: bool) -> bool {}"));
  nodes.push_back(test::ParseAs<Declaration>("f ::= (val: int64) -> int64 {}"));

  auto* bool_overload = ASSERT_NOT_NULL(nodes[0]->if_as<Expression>());
  auto* int_overload  = ASSERT_NOT_NULL(nodes[1]->if_as<Expression>());

  mod.AppendStatements(std::move(nodes));
  mod.process([&compiler](base::PtrSpan<Node const> nodes) {
    for (Node const* node : nodes) { node->VerifyType(&compiler); }
  });

  OverloadSet os;
  os.emplace(bool_overload, compiler::VerifyResult::Constant(type::Generic));
  os.emplace(int_overload, compiler::VerifyResult::Constant(type::Generic));

  SECTION("without args") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({}, {}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("without one positional arg - bool") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({"true"}, {}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Constant(type::Bool));
  }

  SECTION("without one positional arg - int64") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({"3"}, {}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Constant(type::Int64));
  }

  SECTION("without one positional arg - no matching type") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({"3.0"}, {}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("without one named arg - bool") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({}, {{"val", "true"}}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Constant(type::Bool));
  }

  SECTION("without one named arg - int64") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({}, {{"val", "3"}}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Constant(type::Int64));
  }

  SECTION("without one named arg - no matching type") {
    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({}, {{"val", "3.0"}}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("without variant - matching one") {
    mod.Append(test::ParseAs<Declaration>("v: bool | float32 = true "));
    mod.process([&compiler](base::PtrSpan<Node const> nodes) {
      for (Node const* node : nodes) { node->VerifyType(&compiler); }
    });

    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({"v"}, {}));
    CHECK(VerifyDispatch(&compiler, &call_expr, os,
                         ResultsForArgs(&compiler, call_expr.args())) ==
          compiler::VerifyResult::Error());
  }

  SECTION("without variant - matching both") {
    mod.Append(test::ParseAs<Declaration>("v: bool | int64 = true"));
    mod.process([&compiler](base::PtrSpan<Node const> nodes) {
      for (Node const* node : nodes) { node->VerifyType(&compiler); }
    });

    Call call_expr(frontend::SourceRange{}, test::ParseAs<Expression>("f"),
                   test::MakeFnArgs({"v"}, {}));
    CHECK(
        VerifyDispatch(&compiler, &call_expr, os,
                       ResultsForArgs(&compiler, call_expr.args())) ==
        compiler::VerifyResult::Constant(type::Var({type::Bool, type::Int64})));
  }
}

}  // namespace
}  // namespace ast
