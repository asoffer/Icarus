#ifndef ICARUS_COMPILER_WORK_RESOURCES_H
#define ICARUS_COMPILER_WORK_RESOURCES_H

#include <functional>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "ast/expression.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/value/result_buffer.h"
#include "type/typed_value.h"

namespace compiler {
struct Context;

struct WorkResources {
  std::function<void(WorkItem, absl::flat_hash_set<WorkItem>)> enqueue;
  std::function<std::variant<ir::CompleteResultBuffer,
                             std::vector<diagnostic::ConsumedMessage>>(
      Context &, type::Typed<ast::Expression const *>)>
      evaluate;
  std::function<void(WorkItem const &)> complete;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_WORK_RESOURCES_H
