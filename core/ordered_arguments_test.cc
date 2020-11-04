#include "core/ordered_arguments.h"

#include <memory>

#include "gtest/gtest.h"

namespace core {
namespace {

TEST(OrderedArguments, construction) {
  std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
  auto args = OrderedArguments<int>{std::move(v)};
  EXPECT_EQ(args.size(), 0);
  EXPECT_TRUE(args.empty());

  {
    std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
    v.emplace_back("hello", std::make_unique<int>(3));
    v.emplace_back("world", std::make_unique<int>(4));
    auto args = OrderedArguments<int>{std::move(v)};
    EXPECT_EQ(args.size(), 2);
    EXPECT_FALSE(args.empty());
    EXPECT_EQ(args.pos().size(), 0);
    EXPECT_EQ(args.named().size(), 2);
    EXPECT_EQ(*args.named().at("hello"), 3);
    EXPECT_EQ(*args.named().at("world"), 4);
    EXPECT_EQ(args.ordered_args().size(), 2);
    EXPECT_EQ(args.ordered_args()[0].first, "hello");
    EXPECT_EQ(args.ordered_args()[1].first, "world");
  }

  {
    std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
    v.emplace_back("", std::make_unique<int>(2));
    v.emplace_back("hello", std::make_unique<int>(3));
    v.emplace_back("world", std::make_unique<int>(4));
    auto args = OrderedArguments<int>{std::move(v)};
    EXPECT_EQ(args.size(), 3);
    EXPECT_EQ(args.pos().size(), 1);
    EXPECT_EQ(*args.pos().at(0), 2);

    EXPECT_EQ(args.named().size(), 2);
    EXPECT_EQ(*args.named().at("hello"), 3);
    EXPECT_EQ(*args.named().at("world"), 4);

    EXPECT_EQ(args.ordered_args().size(), 3);
    EXPECT_EQ(args.ordered_args()[0].first, "");
    EXPECT_EQ(args.ordered_args()[1].first, "hello");
    EXPECT_EQ(args.ordered_args()[2].first, "world");
  }
}

TEST(OrderedArguments, DropOrder) {
  std::vector<std::pair<std::string, std::unique_ptr<int>>> v;
  v.emplace_back("", std::make_unique<int>(2));
  v.emplace_back("hello", std::make_unique<int>(3));
  v.emplace_back("world", std::make_unique<int>(4));
  auto args = OrderedArguments<int>{std::move(v)}.DropOrder();

  EXPECT_EQ(args.size(), 3);
  EXPECT_EQ(args.pos().size(), 1);
  EXPECT_EQ(*args.pos().at(0), 2);

  EXPECT_EQ(args.named().size(), 2);
  EXPECT_EQ(*args.named().at("hello"), 3);
  EXPECT_EQ(*args.named().at("world"), 4);
}

}  // namespace
}  // namespace core
