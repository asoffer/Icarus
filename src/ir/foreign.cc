#include "ir/foreign.h"

#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "type/type.h"

namespace ir {
static base::guarded<base::unordered_map<std::string_view, ForeignFn::Data>>
    foreign_fns;

ForeignFn::ForeignFn(std::string_view name, ast::Expression const *expr,
                     type::Function const *t)
    : handle_(
          &*foreign_fns.lock()->emplace(name, ForeignFn::Data{expr, t}).first) {
}

}  // namespace ir
