#include "core/dependency_node.h"

#include "gtest/gtest.h"

namespace core {
namespace {

TEST(DependencyNode, Test) {
  int m, n;
  EXPECT_EQ(DependencyNode<int>::ParameterValue(&n),
            DependencyNode<int>::ParameterValue(&n));
  EXPECT_NE(DependencyNode<int>::ParameterValue(&m),
            DependencyNode<int>::ParameterValue(&n));
  EXPECT_NE(DependencyNode<int>::ArgumentValue(&n),
            DependencyNode<int>::ParameterValue(&n));
  EXPECT_NE(DependencyNode<int>::ParameterType(&n),
            DependencyNode<int>::ParameterValue(&n));

  EXPECT_EQ(DependencyNode<int>::ParameterValue(&m).kind(),
            DependencyNodeKind::ParameterValue);
  EXPECT_EQ(DependencyNode<int>::ParameterValue(&m).node(), &m);
}

}  // namespace
}  // namespace core

