#include "core/ordered_fn_args.h"

#include <memory>

#include "test/catch.h"
#include "test/elements_are.h"

namespace core {
namespace {
using test::ElementsAre;

TEST_CASE("construction") {
  std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
  auto args = OrderedFnArgs<int>{std::move(v)};
  CHECK(args.size() == 0);
  CHECK(args.empty());

  {
    std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
    v.emplace_back("hello", std::make_unique<int>(3));
    v.emplace_back("world", std::make_unique<int>(4));
    auto args = OrderedFnArgs<int>{std::move(v)};
    CHECK(args.size() == 2);
    CHECK_FALSE(args.empty());
    CHECK(args.pos().size() == 0);
    CHECK(args.named().size() == 2);
    CHECK(*args.named().at("hello") == 3);
    CHECK(*args.named().at("world") == 4);
    CHECK(args.ordered_args().size() == 2);
    CHECK(args.ordered_args()[0].first == "hello");
    CHECK(args.ordered_args()[1].first == "world");
  }

  {
    std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
    v.emplace_back("", std::make_unique<int>(2));
    v.emplace_back("hello", std::make_unique<int>(3));
    v.emplace_back("world", std::make_unique<int>(4));
    auto args = OrderedFnArgs<int>{std::move(v)};
    CHECK(args.size() == 3);
    CHECK(args.pos().size() == 1);
    CHECK(*args.pos().at(0) == 2);

    CHECK(args.named().size() == 2);
    CHECK(*args.named().at("hello") == 3);
    CHECK(*args.named().at("world") == 4);

    CHECK(args.ordered_args().size() == 3);
    CHECK(args.ordered_args()[0].first == "");
    CHECK(args.ordered_args()[1].first == "hello");
    CHECK(args.ordered_args()[2].first == "world");
  }
}

TEST_CASE("DropOrder") {
  std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
  v.emplace_back("", std::make_unique<int>(2));
  v.emplace_back("hello", std::make_unique<int>(3));
  v.emplace_back("world", std::make_unique<int>(4));
  auto args = OrderedFnArgs<int>{std::move(v)}.DropOrder();

  CHECK(args.size() == 3);
  CHECK(args.pos().size() == 1);
  CHECK(*args.pos().at(0) == 2);

  CHECK(args.named().size() == 2);
  CHECK(*args.named().at("hello") == 3);
  CHECK(*args.named().at("world") == 4);
}

}  // namespace
}  // namespace core
