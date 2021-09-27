#include "compiler/resources.h"

namespace compiler {

// void WorkQueue::ProcessOneItem() {
//   ASSERT(items_.empty() == false);
//   auto [item, resources, state] = std::move(items_.front());
//   items_.pop();
//   if (completed_.contains(item)) {
// #if defined(ICARUS_DEBUG)
//     cycle_breaker_count_ = 0;
// #endif
//     return;
//   } else if (auto iter = prerequisites_.find(item);
//              iter != prerequisites_.end()) {
//     // TODO: We colud remove the completed prerequisites.
//     for (auto const& prereq : iter->second) {
//       if (not completed_.contains(prereq)) {
// #if defined(ICARUS_DEBUG)
//         LOG("", "Deferring %s", item.node->DebugString());
//         ++cycle_breaker_count_;
//         ASSERT(cycle_breaker_count_ <= items_.size());
// #endif
//         return;
//       }
//     }
//   }
// 
//   switch (item.Process(resources, state)) {
//     case WorkItem::Result::Deferred: {
// #if defined(ICARUS_DEBUG)
//       LOG("", "Deferring %s", item.node->DebugString());
//       ++cycle_breaker_count_;
//       ASSERT(cycle_breaker_count_ <= items_.size());
// #endif
//       items_.emplace(item, resources, std::move(state));
//     } break;
//     case WorkItem::Result::Failure:
//       // TODO: How should we report this?
//       [[fallthrough]];
//     case WorkItem::Result::Success:
//       completed_.insert(item);
//       prerequisites_.erase(item);
// #if defined(ICARUS_DEBUG)
//       cycle_breaker_count_ = 0;
// #endif
//   }
// }

}  // namespace compiler
