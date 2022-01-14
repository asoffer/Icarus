#include "compiler/cyclic_dependency_tracker.h"

#include "ast/ast.h"
#include "diagnostic/consumer/tracking.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

TEST(CyclicDependencyTracker, NoErrors) {
  CyclicDependencyTracker dep_tracker;
  diagnostic::TrackingConsumer diag;

  // No errors even if they have the same name.
  ast::Identifier id1(frontend::SourceRange(), "a");
  ast::Identifier id2(frontend::SourceRange(), "b");
  ast::Identifier id3(frontend::SourceRange(), "a");

  auto token1 = dep_tracker.PushDependency(&id1, diag);
  ASSERT_TRUE(static_cast<bool>(token1));

  auto token2 = dep_tracker.PushDependency(&id2, diag);
  ASSERT_TRUE(static_cast<bool>(token2));

  {  // Tracking for `id3` goes out of scope when `token3` does.
    auto token3 = dep_tracker.PushDependency(&id3, diag);
    ASSERT_TRUE(static_cast<bool>(token3));
  }

  {
    auto token3 = dep_tracker.PushDependency(&id3, diag);
    ASSERT_TRUE(static_cast<bool>(token3));
  }

  EXPECT_THAT(diag.diagnostics(), IsEmpty());
  EXPECT_FALSE(dep_tracker.has_error(&id1));
  EXPECT_FALSE(dep_tracker.has_error(&id2));
  EXPECT_FALSE(dep_tracker.has_error(&id3));
}

TEST(CyclicDependencyTracker, Errors) {
  CyclicDependencyTracker dep_tracker;
  diagnostic::TrackingConsumer diag;

  // No errors even if they have the same name.
  ast::Identifier id1(frontend::SourceRange(), "a");
  ast::Identifier id2(frontend::SourceRange(), "a");

  frontend::SourceBuffer buffer("\n");
  module::BasicModule module(&buffer);
  ast::Scope scope(&module);
  ast::Node::Initializer i{.scope = &scope};
  id1.Initialize(i);
  id2.Initialize(i);

  auto token1 = dep_tracker.PushDependency(&id1, diag);
  auto token2 = dep_tracker.PushDependency(&id2, diag);

  auto token3 = dep_tracker.PushDependency(&id1, diag);
  EXPECT_FALSE(static_cast<bool>(token3));
  EXPECT_THAT(diag.diagnostics(),
              ElementsAre(Pair("type-error", "cyclic-dependency")));
  EXPECT_TRUE(dep_tracker.has_error(&id1));
  EXPECT_FALSE(dep_tracker.has_error(&id2));
}

}  // namespace
}  // namespace compiler
