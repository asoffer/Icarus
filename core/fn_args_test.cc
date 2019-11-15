#include "core/fn_args.h"

#include <iostream>
#include "test/catch.h"
#include "test/elements_are.h"

namespace core {
namespace {
using test::ElementsAre;

TEST_CASE("size") {
  auto args = FnArgs<int>{};
  CHECK(args.size() == 0);
  CHECK(args.empty());

  args = FnArgs<int>({}, {{"hello", -3}, {"world", -5}});
  CHECK(args.size() == 2);
  CHECK_FALSE(args.empty());

  args = FnArgs<int>({1, 4, 9}, {});
  CHECK(args.size() == 3);
  CHECK_FALSE(args.empty());

  args = FnArgs<int>({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  CHECK(args.size() == 5);
  CHECK_FALSE(args.empty());
}

TEST_CASE("iterators") {
  auto args = FnArgs<int>{};
  CHECK(args.begin() == args.end());

  args = FnArgs<int>({}, {{"hello", -3}, {"world", -5}});
  CHECK(args.begin() != args.end());
}

TEST_CASE("mutable access") {
  FnArgs<int> args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  CHECK(args.pos() == std::vector<int>{1, 4, 9});
  CHECK_THAT(args.named(),
             ElementsAre(std::pair<std::string_view, int>{"hello", -3},
                         std::pair<std::string_view, int>{"world", -5}));
  CHECK(args.at_or_null("world!") == nullptr);
}

TEST_CASE("const access") {
  FnArgs<int> const args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  CHECK(args.pos() == std::vector<int>{1, 4, 9});
  CHECK_THAT(args.named(),
             ElementsAre(std::pair<std::string_view, int>{"hello", -3},
                         std::pair<std::string_view, int>{"world", -5}));
  CHECK(args.at_or_null("world!") == nullptr);
}

TEST_CASE("transform") {
  auto squared = FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}})
                     .Transform([](int n) { return n * n; });
  CHECK(squared.pos() == std::vector<int>{1, 4, 9});
  CHECK_THAT(squared.named(),
             ElementsAre(std::pair<std::string_view, int>{"hello", 9},
                         std::pair<std::string_view, int>{"world", 25}));
}

TEST_CASE("apply") {
  int total = 0;
  FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}}).Apply([&total](int n) {
    total += n;
  });
  CHECK(total == -2);
}

TEST_CASE("apply with index") {
  size_t total = 0;
  FnArgs<size_t>({6, 12, 18}, {{"hello", 10}, {"world", 20}})
      .ApplyWithIndex([&total](auto &&index, size_t n) {
        if constexpr (std::is_same_v<size_t, std::decay_t<decltype(index)>>) {
          total += index;
        } else {
          total += n;
        }
      });
  CHECK(total == 33u);
}

TEST_CASE("const_iterator") {
  SECTION("generic") {
    FnArgs<int> fnargs({1, 2, 3}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    CHECK(total == -2);
  }

  SECTION("positional only") {
    FnArgs<int> fnargs({1, 2, 3}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    CHECK(total == 6);
  }

  SECTION("named only") {
    FnArgs<int> fnargs({}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    CHECK(total == -8);
  }

  SECTION("empty") {
    FnArgs<int> fnargs({}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    CHECK(total == 0);
  }
}

}  // namespace
}  // namespace core
