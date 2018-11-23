#include "ir/foreign.h"

#include "base/container/map.h"  // TODO make unordered
#include "base/guarded.h"

namespace ir {
static base::guarded<base::map<std::string_view, ast::Expression const *>>
    foreign_fns;

ForeignFn::ForeignFn(std::string_view name, ast::Expression const *expr)
    : handle_(&*foreign_fns.lock()->emplace(name, expr).first) {}

}  // namespace ir
