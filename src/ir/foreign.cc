#include "ir/foreign.h"

#include "base/container/unordered_map.h"
#include "base/guarded.h"

namespace ir {
static base::guarded<
    base::unordered_map<std::string_view, ast::Expression const *>>
    foreign_fns;

ForeignFn::ForeignFn(std::string_view name, ast::Expression const *expr)
    : handle_(&*foreign_fns.lock()->emplace(name, expr).first) {}

}  // namespace ir
