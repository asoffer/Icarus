#include "compiler/cyclic_dependency_tracker.h"

#include "ast/ast.h"
#include "base/ptr_span.h"
#include "compiler/module.h"
#include "diagnostic/consumer/tracking.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

struct TestModule : CompiledModule {
  ~TestModule() override { CompilationComplete(); }
  void ProcessNodes(base::PtrSpan<ast::Node const>,
                    diagnostic::DiagnosticConsumer&) override {}
};

TEST(CyclicDependencyTracker, NoErrors) {
  TestModule mod;
  CyclicDependencyTracker dep_tracker;
  diagnostic::TrackingConsumer diag;

  // No errors even if they have the same name.
  ast::Identifier id1(frontend::SourceRange(), "a");
  ast::Identifier id2(frontend::SourceRange(), "b");
  ast::Identifier id3(frontend::SourceRange(), "a");

  auto token1 = dep_tracker.PushDependency(&id1, mod.data(), diag);
  ASSERT_TRUE(static_cast<bool>(token1));

  auto token2 = dep_tracker.PushDependency(&id2, mod.data(), diag);
  ASSERT_TRUE(static_cast<bool>(token2));

  {  // Tracking for `id3` goes out of scope when `token3` does.
    auto token3 = dep_tracker.PushDependency(&id3, mod.data(), diag);
    ASSERT_TRUE(static_cast<bool>(token3));
  }

  {
    auto token3 = dep_tracker.PushDependency(&id3, mod.data(), diag);
    ASSERT_TRUE(static_cast<bool>(token3));
  }

  EXPECT_THAT(diag.diagnostics(), IsEmpty());
}

TEST(CyclicDependencyTracker, Errors) {
  TestModule mod;
  CyclicDependencyTracker dep_tracker;
  diagnostic::TrackingConsumer diag;

  // No errors even if they have the same name.
  ast::Identifier id1(frontend::SourceRange(), "a");
  ast::Identifier id2(frontend::SourceRange(), "b");

  auto token1 = dep_tracker.PushDependency(&id1, mod.data(), diag);
  auto token2 = dep_tracker.PushDependency(&id2, mod.data(), diag);

  auto token3 = dep_tracker.PushDependency(&id1, mod.data(), diag);
  EXPECT_FALSE(static_cast<bool>(token3));
  EXPECT_THAT(diag.diagnostics(),
              ElementsAre(Pair("type-error", "cyclic-dependency")));
}

}  // namespace
}  // namespace compiler
