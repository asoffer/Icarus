#include "ast/fn_args.h"

#include "test/test.h"

namespace ast {
namespace {

TEST(MutableAccess) {
  FnArgs<int> args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  CHECK(args.at(0) == 1);
  CHECK(args.at(1) == 4);
  CHECK(args.at(2) == 9);
  CHECK(args.at("hello") == -3);
  CHECK(args.at("world") == -5);
  CHECK(args.at_or_null("world!") == nullptr);

  CHECK(args.num_pos() == 3u);
  CHECK(args.num_named() == 2u);
}

TEST(ConstAccess) {
  FnArgs<int> const args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  CHECK(args.at(0) == 1);
  CHECK(args.at(1) == 4);
  CHECK(args.at(2) == 9);
  CHECK(args.at("hello") == -3);
  CHECK(args.at("world") == -5);
  CHECK(args.at_or_null("world!") == nullptr);

  CHECK(args.num_pos() == 3u);
  CHECK(args.num_named() == 2u);
}

TEST(Transform) {
  auto squared = FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}})
                     .Transform([](int n) { return n * n; });
  CHECK(squared.at(0) == 1);
  CHECK(squared.at(1) == 4);
  CHECK(squared.at(2) == 9);
  CHECK(squared.at("hello") == 9);
  CHECK(squared.at("world") == 25);

  CHECK(squared.num_pos() == 3u);
  CHECK(squared.num_named() == 2u);
}

TEST(Apply) {
  int total = 0;
  FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}}).Apply([&total](int n) {
    total += n;
  });
  CHECK(total == -2);
}

TEST(ApplyWithIndex) {
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

}  // namespace
}  // namespace ast
