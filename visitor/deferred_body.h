#ifndef ICARUS_VISITOR_DEFERRED_BODY_H
#define ICARUS_VISITOR_DEFERRED_BODY_H

#include "absl/container/node_hash_map.h"
#include "ast/ast_fwd.h"

namespace visitor {

template <typename Visitor>
struct DeferredBody {
  void CompleteBody(ast::Node const *node) {
    auto iter = deferred_work_.find(node);
    if (iter == deferred_work_.end()) { return; }
    iter->second();
  }

  void CompleteDeferredBodies() {
    while (!deferred_work_.empty()) { deferred_work_.begin()->second(); }
  }

  template <typename Fn>
  std::function<void()>* AddWork(ast::Node const *node, Fn &&fn) {
    auto [iter, success] = deferred_work_.emplace(node, std::forward<Fn>(fn));
    ASSERT(success == true);
    return &iter->second;
  }
  void EraseWork(ast::Node const *node) { deferred_work_.erase(node); }

  ~DeferredBody() { ASSERT(deferred_work_.empty() == true); }

 private:
  // TODO should we lock access here so deferred work can be done on multiple
  // threads?
  absl::node_hash_map<ast::Node const *, std::function<void()>> deferred_work_;
};

}  // namespace visitor
#endif  // ICARUS_VISITOR_DEFERRED_BODY_H
