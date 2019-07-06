#ifndef ICARUS_VISITOR_DEFERRED_BODY_H
#define ICARUS_VISITOR_DEFERRED_BODY_H

#include <atomic>

#include "absl/container/node_hash_map.h"
#include "ast/ast_fwd.h"
#include "base/guarded.h"
#include "base/move_func.h"

namespace visitor {

template <typename Visitor>
struct DeferredBody {
  void CompleteDeferredBodies() {
    base::move_func<void()> f;
    while (true) {
      {
        auto handle = deferred_work_.lock();
        if (handle->empty()) { return; }
        auto nh = handle->extract(handle->begin());
        f       = std::move(nh.mapped());
      }
      std::move(f)();
    }
  }

  template <typename Fn>
  base::move_func<void()> *AddWork(ast::Node const *node, Fn &&fn) {
    auto [iter, success] =
        deferred_work_.lock()->emplace(node, std::forward<Fn>(fn));
    ASSERT(success == true);
    return &iter->second;
  }

  ~DeferredBody() { ASSERT(deferred_work_.lock()->empty() == true); }

 private:
  base::guarded<absl::node_hash_map<ast::Node const *, base::move_func<void()>>>
      deferred_work_;
};

}  // namespace visitor
#endif  // ICARUS_VISITOR_DEFERRED_BODY_H
