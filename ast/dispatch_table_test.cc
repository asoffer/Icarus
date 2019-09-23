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
core::FnArgs<std::pair<Expression const*, visitor::VerifyResult>>
ResultsForArgs(visitor::TraditionalCompilation* visitor,
               core::FnArgs<Expression const*> const& args) {
  return args.Transform([visitor](Expression const* expr) {
    return std::pair<Expression const*, visitor::VerifyResult>(
        expr, *ASSERT_NOT_NULL(visitor->prior_verification_attempt(expr)));
  });
}

core::OrderedFnArgs<Expression> MakeFnArgs(
    visitor::TraditionalCompilation* visitor, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args) {
  std::vector<std::pair<std::string, std::unique_ptr<Expression>>> vec;
  for (auto pos_arg : pos_args) {
    vec.emplace_back(
        "", test::MakeVerified<Expression>(visitor, std::move(pos_arg)));
  }
  for (auto & [ name, arg ] : named_args) {
    vec.emplace_back(std::move(name),
                     test::MakeVerified<Expression>(visitor, std::move(arg)));
  }
  return core::OrderedFnArgs<Expression>(std::move(vec));
}

std::pair<OverloadSet, std::unique_ptr<Call>> MakeCall(
    visitor::TraditionalCompilation* visitor, std::string fn,
    std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args) {
  auto call_expr = std::make_unique<Call>(
      frontend::SourceRange{}, test::MakeVerified<Expression>(visitor, std::move(fn)),
      MakeFnArgs(visitor, std::move(pos_args), std::move(named_args)));
  OverloadSet os;
  os.emplace(call_expr->callee(),
             visitor::VerifyResult::Constant(type::Func({}, {})));

  return std::pair(std::move(os), std::move(call_expr));
}

// TODO you're not really handling const-ness correctly in any of these tests.
TEST_CASE("() -> ()") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "() -> () {}", std::move(pos), std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with positional arg") {
    auto[os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto[os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with named arg") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto[os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(b: bool) -> ()") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(b: bool) -> () {}", std::move(pos), std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto[os, call_expr] = call_with({}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"n", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto[os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto[os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "false"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(b := true) -> ()") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(b := true) -> () {}", std::move(pos),
                    std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto[os, call_expr] = call_with({}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"n", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    auto[os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto[os, call_expr] = call_with({"true"}, {{"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "false"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(n: int64, b: bool) -> ()") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(n: int64, b: bool) -> () {}", std::move(pos),
                    std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional args - incorrect order") {
    auto[os, call_expr] = call_with({"true", "3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional args - correct order") {
    auto[os, call_expr] = call_with({"3", "true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, correct names ") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with multiple named args - incorrect types, correct names") {
    auto[os, call_expr] = call_with({}, {{"b", "true"}, {"n", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, incorrect names") {
    auto[os, call_expr] = call_with({}, {{"B", "true"}, {"n", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named args - incorrect types, incorrect names") {
    auto[os, call_expr] = call_with({}, {{"B", "true"}, {"n", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    auto[os, call_expr] = call_with({"3"}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Void()));
  }

  SECTION("with both positional and named args - named has incorrect type") {
    auto[os, call_expr] = call_with({"3"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION(
      "with both positional and named args - positional has incorrect type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with both positional and named args - both have  incorrect type") {
    auto[os, call_expr] = call_with({"true"}, {{"b", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(T :: type) -> *T") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(T :: type) -> *T {}", std::move(pos),
                    std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one positional arg - (bool)") {
    auto[os, call_expr] = call_with({"bool"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - (int64)") {
    auto[os, call_expr] = call_with({"int64"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - (T = bool)") {
    auto[os, call_expr] = call_with({}, {{"T", "bool"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - (t = bool)") {
    auto[os, call_expr] = call_with({}, {{"t", "bool"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - (T = true)") {
    auto[os, call_expr] = call_with({}, {{"T", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - (t = true)") {
    auto[os, call_expr] = call_with({}, {{"t", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(T ::= bool) -> *T") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(T ::= bool) -> *T {}", std::move(pos),
                    std::move(named));
  };

  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - correct type") {
    auto[os, call_expr] = call_with({"bool"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"int64"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - correct type, correct name") {
    auto[os, call_expr] = call_with({}, {{"T", "bool"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"t", "bool"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto[os, call_expr] = call_with({}, {{"T", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"t", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("(val: T, T ::= bool) -> *T") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  Call call_expr(
      frontend::SourceRange{},
      test::MakeVerified<Expression>(&v, "(val: T, T ::= bool) -> *T {}"),
      core::OrderedFnArgs<Expression>{});
  REQUIRE(call_expr.callee() != nullptr);

  auto call_with = [&v](std::vector<std::string>&& pos,
                        absl::flat_hash_map<std::string, std::string>&& named) {
    return MakeCall(&v, "(val: T, T ::= bool) -> *T {}", std::move(pos),
                    std::move(named));
  };
  SECTION("without args") {
    auto[os, call_expr] = call_with({}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    auto[os, call_expr] = call_with({"true"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    auto[os, call_expr] = call_with({"3"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    auto[os, call_expr] = call_with({}, {{"val", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"x", "true"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    auto[os, call_expr] = call_with({}, {{"val", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    auto[os, call_expr] = call_with({}, {{"x", "3"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple positional arguments - (bool)") {
    auto[os, call_expr] = call_with({"true", "bool"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple positional arguments - (int64)") {
    auto[os, call_expr] = call_with({"3", "int64"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with multiple positional arguments - type mismatch") {
    auto[os, call_expr] = call_with({"3", "bool"}, {});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("with multiple named arguments - (bool)") {
    auto[os, call_expr] = call_with({}, {{"val", "true"}, {"T", "bool"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple named arguments - (int64)") {
    auto[os, call_expr] = call_with({}, {{"val", "3"}, {"T", "int64"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - correct types") {
    auto[os, call_expr] = call_with({"3"}, {{"T", "int64"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - incorrect types") {
    auto[os, call_expr] = call_with({"true"}, {{"T", "int64"}});
    CHECK(VerifyDispatch(&v, call_expr.get(), os,
                         ResultsForArgs(&v, call_expr->args())) ==
          visitor::VerifyResult::Error());
  }
}

TEST_CASE("overload set") {
  Module mod;
  visitor::TraditionalCompilation v(&mod);

  auto bool_overload =
      test::MakeVerified<Expression>(&v, "f ::= (val: bool) -> bool {}");
  auto int_overload =
      test::MakeVerified<Expression>(&v, "f ::= (val: int64) -> int64 {}");

  OverloadSet os;
  os.emplace(bool_overload.get(),
             visitor::VerifyResult::Constant(type::Generic));
  os.emplace(int_overload.get(),
             visitor::VerifyResult::Constant(type::Generic));

  SECTION("without args") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {}, {}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("without one positional arg - bool") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {"true"}, {}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Constant(type::Bool));
  }

  SECTION("without one positional arg - int64") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {"3"}, {}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Constant(type::Int64));
  }

  SECTION("without one positional arg - no matching type") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {"3.0"}, {}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("without one named arg - bool") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {}, {{"val", "true"}}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Constant(type::Bool));
  }

  SECTION("without one named arg - int64") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {}, {{"val", "3"}}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Constant(type::Int64));
  }

  SECTION("without one named arg - no matching type") {
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {}, {{"val", "3.0"}}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("without variant - matching one") {
    auto variant =
        test::MakeVerified<Expression>(&v, "v: bool | float32 = true");
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {"v"}, {}));
    CHECK(VerifyDispatch(&v, &call_expr, os,
                         ResultsForArgs(&v, call_expr.args())) ==
          visitor::VerifyResult::Error());
  }

  SECTION("without variant - matching both") {
    auto variant = test::MakeVerified<Expression>(&v, "v: bool | int64 = true");
    Call call_expr(frontend::SourceRange{}, test::MakeUnverified<Expression>(&v, "f"),
                   MakeFnArgs(&v, {"v"}, {}));
    CHECK(
        VerifyDispatch(&v, &call_expr, os,
                       ResultsForArgs(&v, call_expr.args())) ==
        visitor::VerifyResult::Constant(type::Var({type::Bool, type::Int64})));
  }
}

}  // namespace
}  // namespace ast
