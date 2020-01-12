#include "compiler/constant/binding_tree.h"

#include "gtest/gtest.h"

namespace {

TEST(ConstantBindingTree, Root) {
  compiler::ConstantBindingTree tree;
  EXPECT_NE(tree.root(), nullptr);
  EXPECT_EQ(tree.root()->parent(), nullptr);
}

TEST(ConstantBindingTree, AddChild) {
  compiler::ConstantBindingTree tree;
  auto *child1 = tree.AddChildTo(tree.root());
  auto *child2 = tree.AddChildTo(tree.root());
  EXPECT_NE(child1, child2);
  EXPECT_EQ(child1->parent(), tree.root());
  EXPECT_EQ(child2->parent(), tree.root());
}

}  // namespace
