#include "visitor/format.h"

#include "base/log.h"

namespace visitor {
void Format::operator()(ast::Node const *node) const { base::Log() << ":P"; }
}  // namespace visitor
