#include "ast/dispatch_table.h"

#include <string_view>

#include "ast/call.h"
#include "misc/context.h"
#include "test/catch.h"
#include "test/util.h"

namespace ast {
namespace {

core::FnArgs<std::pair<Expression*, VerifyResult>> ResultsForArgs(
    core::FnArgs<std::unique_ptr<Expression>> const& args, Context* ctx) {
  return args.Transform([ctx](std::unique_ptr<Expression> const& expr) {
    return std::pair(
        expr.get(),
        *ASSERT_NOT_NULL(ctx->prior_verification_attempt(expr.get())));
  });
}

core::FnArgs<std::unique_ptr<Expression>> MakeCallExprs(
    std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args, Context* ctx) {
  core::FnArgs<std::string> arg_strs(std::move(pos_args),
                                     std::move(named_args));
  return arg_strs.Transform([&](std::string const& expr_str) {
    return test::MakeVerified<Expression>(expr_str, ctx);
  });
}

// TODO you're not really handling const-ness correctly in any of these tests.
TEST_CASE("() -> ()") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("() -> () {}", &ctx), {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(), VerifyResult::Constant(type::Func({}, {})));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with positional arg") {
    call_expr.args_ = MakeCallExprs({"true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    call_expr.args_ = MakeCallExprs({"true", "3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with named arg") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}, {"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(b: bool) -> ()") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(b: bool) -> () {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(),
             VerifyResult::Constant(type::Func({type::Bool}, {})));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"n", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    call_expr.args_ = MakeCallExprs({"true", "3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}, {"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "false"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(b := true) -> ()") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(b := true) -> () {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(),
             VerifyResult::Constant(type::Func({type::Bool}, {})));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg - incorrect type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"n", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional args") {
    call_expr.args_ = MakeCallExprs({"true", "3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}, {"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "false"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with named arg shadowing positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(n: int64, b: bool) -> ()") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(n: int64, b: bool) -> () {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(),
             VerifyResult::Constant(type::Func({type::Int64, type::Bool}, {})));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional args - incorrect order") {
    call_expr.args_ = MakeCallExprs({"true", "3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional args - correct order") {
    call_expr.args_ = MakeCallExprs({"3", "true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with one named arg") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, correct names ") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}, {"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with multiple named args - incorrect types, correct names") {
    call_expr.args_ = MakeCallExprs({}, {{"b", "true"}, {"n", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args - correct types, incorrect names") {
    call_expr.args_ = MakeCallExprs({}, {{"B", "true"}, {"n", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named args - incorrect types, incorrect names") {
    call_expr.args_ = MakeCallExprs({}, {{"B", "true"}, {"n", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args") {
    call_expr.args_ = MakeCallExprs({"3"}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Void()));
  }

  SECTION("with both positional and named args - named has incorrect type") {
    call_expr.args_ = MakeCallExprs({"3"}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args - positional has incorrect type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with both positional and named args - both have  incorrect type") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"b", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(T :: type) -> *T") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(T :: type) -> *T {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(), VerifyResult::Constant(type::Generic));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one positional arg - (bool)") {
    call_expr.args_ = MakeCallExprs({"bool"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - (int64)") {
    call_expr.args_ = MakeCallExprs({"int64"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - (T = bool)") {
    call_expr.args_ = MakeCallExprs({}, {{"T", "bool"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - (t = bool)") {
    call_expr.args_ = MakeCallExprs({}, {{"t", "bool"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - (T = true)") {
    call_expr.args_ = MakeCallExprs({}, {{"T", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - (t = true)") {
    call_expr.args_ = MakeCallExprs({}, {{"t", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(T ::= bool) -> *T") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(T ::= bool) -> *T {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(), VerifyResult::Constant(type::Generic));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"bool"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"int64"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with one named arg - correct type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"T", "bool"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"t", "bool"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"T", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"t", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

TEST_CASE("(val: T, T ::= bool) -> *T") {
  Module mod;
  Context ctx(&mod);

  Call call_expr(test::MakeVerified<Expression>("(val: T, T ::= bool) -> *T {}", &ctx),
                 {});
  REQUIRE(call_expr.fn_ != nullptr);

  OverloadSet os;
  os.emplace(call_expr.fn_.get(), VerifyResult::Constant(type::Generic));

  SECTION("without args") {
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one positional arg - correct type") {
    call_expr.args_ = MakeCallExprs({"true"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one positional arg - incorrect type") {
    call_expr.args_ = MakeCallExprs({"3"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - correct type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"val", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with one named arg - correct type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"x", "true"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, correct name") {
    call_expr.args_ = MakeCallExprs({}, {{"val", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with one named arg - incorrect type, incorrect name") {
    call_expr.args_ = MakeCallExprs({}, {{"x", "3"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple positional arguments - (bool)") {
    call_expr.args_ = MakeCallExprs({"true", "bool"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple positional arguments - (int64)") {
    call_expr.args_ = MakeCallExprs({"3", "int64"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with multiple positional arguments - type mismatch") {
    call_expr.args_ = MakeCallExprs({"3", "bool"}, {}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }

  SECTION("with multiple named arguments - (bool)") {
    call_expr.args_ = MakeCallExprs({}, {{"val", "true"}, {"T", "bool"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Bool)));
  }

  SECTION("with multiple named arguments - (int64)") {
    call_expr.args_ = MakeCallExprs({}, {{"val", "3"}, {"T", "int64"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - correct types") {
    call_expr.args_ = MakeCallExprs({"3"}, {{"T", "int64"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) ==
          VerifyResult::Constant(type::Ptr(type::Int64)));
  }

  SECTION("with both positional and named args - incorrect types") {
    call_expr.args_ = MakeCallExprs({"true"}, {{"T", "int64"}}, &ctx);
    CHECK(VerifyDispatch(&call_expr, os, ResultsForArgs(call_expr.args_, &ctx),
                         &ctx) == VerifyResult::Error());
  }
}

}  // namespace
}  // namespace ast
